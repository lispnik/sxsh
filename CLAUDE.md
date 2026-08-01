# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Two ASDF systems in one tree, both SBCL-only:

- **`sxsh`** — a parser for the POSIX shell command language (IEEE Std 1003.1 §2.3 token
  recognition + §2.10 grammar). Produces an AST. Package `#:sxsh`. Deliberately performs
  **no expansion** — words retain raw source text.
- **`sxsh/shell`** — a tree-walking executor over that AST, implementing expansion, builtins,
  redirection, pipelines, and job control. Package `#:sxsh-shell` (nickname `#:sxs`).
  Every external command is launched with **`posix_spawnp(3)`** via `sb-alien` — there is no
  `fork`/`exec` and no `sb-ext:run-program`.

## Layout

```
package.lisp ast.lisp conditions.lisp lexer.lisp parser.lisp   -> system "sxsh"
shell/       package state spawn arith expand redir deparse
             jobs builtins exec driver                          -> system "sxsh/shell"
test/tests.lisp            -> "sxsh/test"        (48 cases)
shell/test-shell.lisp      -> "sxsh/shell/test"  (86 cases)
build.lisp        -> saves the bin/sxsh executable
smoke.sh           -> end-to-end checks against that executable (302 cases)
test/jobs-pty.py   -> job control, driven through a real pty (14 cases)
test/fuzz-parser.py -> mutation fuzzer with shrinking (make fuzz)
test/posix-diff.sh -> differential conformance vs a reference shell (247 cases)
```

`shell/package.lisp` also defines `ast-type`, the `typecase` mapping parser structs to the
executor's dispatch keywords — it is not just a `defpackage`.

## Building and testing

The toolchain is **ocicl**. `ocicl setup` writes a runtime that `~/.sbclrc`
loads, which supplies ASDF and points the source registry at the current
directory -- so `make` works from the repo root with no registry configuration.
CI installs and configures it the same way.

Without ocicl a stock SBCL has no ASDF at all, and `(asdf:test-system ...)`
fails at READ time with `Package ASDF does not exist`; the Makefile passes
`(require :asdf)` so the suites still run in that case.

```bash
make check          # everything (the one to run before calling it done)
make test           # in-image ASDF suites only
make test-parser    # 48 parser cases
make test-shell     # 86 executor cases
make build          # save bin/sxsh (~40MB SBCL image)
make smoke          # build, then drive bin/sxsh end-to-end (302 cases)
make jobs           # job control through a real pty (14 cases)
make fuzz           # fuzz the parser; findings are shrunk and the seed printed
make posix          # differential conformance vs bash (247 cases)
make posix REF_SHELL=/bin/dash    # stricter reference
make clean          # remove bin/ and this project's fasls
```

The equivalent raw invocations, if you need them:

```bash
sbcl --non-interactive --eval '(require :asdf)' --eval '(asdf:test-system "sxsh")'
sbcl --non-interactive --eval '(require :asdf)' --eval '(asdf:test-system "sxsh/shell")'
```

Both `test-op`s **error on any failure**, so a non-zero exit is meaningful — the suites
themselves only print `N passed, M failed` and would otherwise exit 0 on a red run.

The layers test different things and all of them matter. The ASDF suites run in-image and never
touch `main`, argv parsing, or process exit status; `smoke.sh` runs the saved executable and
covers exactly that (plus script mode and stdin mode); `test/jobs-pty.py` allocates a real pty
and puts sxsh in its own session, which is the only way to reach the job-control code paths at
all — without a controlling terminal `have-tty-p` is false, `shell-job-control` stays nil, and
foreground commands never get their own process group. `test/posix-diff.sh` runs the same
source through sxsh and a reference shell and compares output+status, so the expectation comes
from a conforming implementation instead of our own assumptions — it is the cheapest way to
find divergence, and found `set -e`, `read`/IFS and the EXIT-trap bugs in a single pass. None
is hermetic: they spawn real programs (`sort`, `head`, `sleep`, …) and write temp files.

Adding a case to `posix-diff.sh` costs one line. Prefer it over hand-written expectations for
anything the standard specifies. Its third argument marks a case where only the *wording* may
differ (POSIX does not specify diagnostic text) — the exit status must still match, so the case
stays meaningful. If bash itself deviates from POSIX (it disables aliases non-interactively),
it is not a valid reference: assert sxsh's behaviour in `smoke.sh` instead.

When adding a job-control test, assert on *observable process behaviour*, not on what `jobs`
prints. The `bg` builtin sets the job state to `:running` unconditionally, so the table reports
Running even when SIGCONT never arrived — an assertion on that string passes against a job that
is still stopped. Give the job a deadline and check that it actually completed instead.

`sxsh/shell` declares `(:require :sb-posix)`. Without it the first file fails to compile with
`Package SB-POSIX does not exist`; don't drop it when editing `:depends-on`.

### Running a single test

Neither harness supports selecting a case by name — `run-all` is a flat sequence of `check`
macro calls. To exercise one input, call the underlying helper in a REPL:

```lisp
(sxsh/test::p1 "if a; then b; fi")          ; => the sx s-expression the parser test compares
(sxsh-shell/test::capture "echo $((1+1))")  ; => (values stdout-string exit-status)
```

Add a case by inserting a `(check "name" "shell source" expected)` into the relevant
`run-all`. Parser expectations are `sx` forms (a compact s-expression rendering of the AST,
defined at the top of `tests.lisp`); executor expectations are literal stdout strings, usually
built with `(format nil "...~%")`. `check-error` asserts a parse failure; `check-status`
asserts an exit code.

In `smoke.sh` the shape is `check <name> <expected-stdout> <expected-status> <shell-source>`,
where expected stdout has no trailing newline (`$(...)` strips it).

## Architecture

### Parser: the lexer is driven by the parser

The POSIX grammar is not context-free, and this implementation does not pretend otherwise.
`parser.lisp` calls `next-token` with flags that change classification:

- Reserved words (`if then else elif fi do done case esac while until for { } ! in`) are
  recognized **only** in command position — the parser passes `:command-position t` exactly
  where the grammar allows them. `echo if then fi` yields three ordinary words.
- Here-document **bodies** are collected by the lexer on demand (`collect-heredocs`) after the
  parser has registered the `<<` / `<<-` operator, at the next newline.
- `:io-number` and `:assignment-word` are contextual token types, not lexical ones.

Quoting is scanned with balanced/nested awareness (`'...'`, `"..."`, `` `...` ``, `$(...)`,
`${...}`) but preserved **verbatim** in `word-text`. Everything downstream re-reads that raw
text. This is why the executor can `deparse` an AST back to source almost losslessly.

`ast.lisp` defines plain `defstruct` nodes; `conditions.lisp` defines `shell-parse-error`.

### Executor: everything hangs off `exec-node`

`exec-node` (exec.lisp) is the single dispatch point — an `ecase` over `(ast-type node)`. It
sets `shell-last-status` and applies `set -e` (`maybe-errexit`) on every node, so status
bookkeeping lives in one place. Add a new node type by extending both `ast-type`
(shell/package.lisp) and this `ecase`.

Non-local control flow is signalled, not returned:

| Mechanism | Raised by | Caught by |
|---|---|---|
| `shell-exit` condition | `exit`, `set -e`, fatal `set -u` | `main`, `repl`, `exec-subshell` |
| `loop-break` / `loop-continue` (with `n`) | `break`, `continue` | `exec-for`, `exec-while` (decrementing `n` and re-signalling for `break 2`) |
| `func-return` | `return` | function invocation |
| `throw 'not-found` → 127 | PATH lookup failure | each spawn site |
| `throw 'exec-builtin` / `'run-command-bypass` | `exec`, `command` builtins | `exec-simple` |

Because these are `signal`-based conditions rather than `error`s, a stray `handler-case` on
`error` will not swallow them — but `unwind-protect` cleanups still run, which is what
restores fds and shell state.

Key structural decisions:

- **Subshells do not fork.** `exec-subshell` snapshots vars/functions/positionals/cwd
  (`snapshot-shell`), runs the body in-process, and restores in an `unwind-protect`. Any new
  piece of shell state that a subshell must isolate has to be added to *both*
  `snapshot-shell` and `restore-shell` or it will leak out of `( … )`.
- **Redirections have two backends** (redir.lisp): external commands compile them into a list
  of `posix_spawn` file-action recipes; builtins, functions, and compound commands get real
  `dup2` with saved fds restored afterward. Both paths must stay in sync when adding an
  operator.
- **Command substitution captures through a temp file**, not a pipe, so large output cannot
  deadlock a bounded pipe. Trailing newlines are stripped.
- **Builtins run in-process by necessity** (they mutate shell state), including inside
  pipeline stages, where fds are temporarily redirected around the call.
- **Word expansion tracks per-character provenance** (expand.lisp). Each char becomes an
  `xchar` classed `:lit` (source, unquoted — globbable and splittable), `:quoted` (never split,
  never glob), or `:split` (came from an unquoted expansion — IFS-splittable). This mask is
  what makes `"$@"` produce one field per positional (and zero fields when empty) while `"$*"`
  produces one, and what keeps glob metacharacters from an expansion inert. Changing expansion
  almost always means changing how classes are assigned, not just the string building.
- **Job control is real** (jobs.lisp, absent from the README's architecture table): each async
  job gets its own process group via `posix_spawn`'s `SETPGROUP` attribute, tracked in a job
  table with `jobs`/`fg`/`bg`, terminal ownership via `tcsetpgrp` on a private `/dev/tty` fd,
  and `%n` / bare-number / `%prefix` job specs. `set -m` toggles it (`set-monitor`), defaulting
  on for interactive shells and off otherwise.
- **Backgrounding a compound re-execs this binary.** We cannot fork, so `{ ...; } &`, `f &`,
  and any builtin `&` run as a real second process via `sxsh -c <source>` (`async-compound`).
  The source is `async-prelude` + `deparse` of the node: cwd, non-exported variables, function
  definitions and positional parameters are replayed as shell assignments, while exported
  variables travel in the environment. `self-exec-path` returns NIL when we are not a saved
  image (`*runtime-pathname*` ≠ `*core-pathname*`), and then it falls back to running
  synchronously in-process — so the async tests only mean anything against a built `./sxsh`.

### Two job-control invariants that are easy to break

**Never write a signal number as a literal.** Linux and macOS disagree on exactly the signals
job control uses: SIGCONT is 18/19, SIGTSTP 20/18, SIGCHLD 17/20, SIGSTOP 19/17 (SIGTTIN 21 and
SIGTTOU 22 happen to agree). Always use `sb-unix:sigcont` and friends. Every one of these was
previously a Linux literal, and on macOS the results were: `bg`/`fg` sent SIGTSTP instead of
SIGCONT (re-stopping the job), `trap ... CONT` installed on the wrong signal, `128+SIGTSTP` was
computed as 148 instead of 146, and — worst — `init-job-control` set **SIGCHLD** to `SIG_IGN`
believing it was SIGTSTP, which tells the kernel to auto-reap children so every `waitpid`
returns ECHILD and no job ever leaves `:running`.

**`SIG_IGN` is inherited across `exec`; installed handlers are not.** This single fact decides
how each signal is handled:

- SIGTSTP/SIGTTIN/SIGTTOU need `SIG_IGN` in the shell (a handler would make `tcsetpgrp` fail
  with EINTR instead of succeeding), so every spawned child must have them reset to `SIG_DFL`
  or Ctrl-Z on the foreground job does nothing. That is what `child-sigdefaults` (exec.lisp)
  plus `spawn`'s `:sigdefault` (`POSIX_SPAWN_SETSIGDEF`) are for. A signal the *user* ignored
  via `trap '' SIG` is deliberately excluded — POSIX requires that one to be inherited.
  **Any new `spawn` call site must pass `:sigdefault (child-sigdefaults sh)`.**
- SIGINT/SIGQUIT instead get a no-op *handler* (`install-interrupt-handlers`). An interactive
  shell must survive Ctrl-C and return to the prompt rather than being killed; using a handler
  rather than `SIG_IGN` means `exec` resets them automatically, so children stay interruptible
  with no `SETSIGDEF` bookkeeping.

Reaping is once-per-pid: `update-job-state` and `wait-for-job` skip members already in
`job-reaped` and treat an ECHILD from `waitpid` as "finished", not "still running". Treating
ECHILD as running is what used to leave multi-stage pipeline jobs stuck at `:running` forever.
Job status comes from the *last* pid in `job-pids` (pipeline order), per POSIX. `poll-jobs`
also polls *stopped* jobs with `WCONTINUED` so an externally resumed job is noticed; the
continued-status test must run **before** the stopped one, since on macOS a continued status
also carries `0x7f` in its low byte.

### `set -e` has scope, and expansion has side effects

Two executor invariants that are easy to break and were each a real bug:

**`set -e` is scoped.** `maybe-errexit` must not fire for a condition context. `exec-and-or`
binds `*errexit-suppressed*` around its left subtree, `exec-if`/`exec-while` around the
condition, `exec-pipeline-raw` around the stages; `maybe-errexit` additionally skips `:list`,
`:and-or`, and `!`-led pipelines. Firing everywhere made `set -e; if false; then :; fi` exit —
which kills most real scripts at their first `if`. `test/posix-diff.sh` has 21 cases pinning
this against a reference shell; run them after touching anything in that area.

**Expansion is not idempotent.** `expand-command-words` runs command substitutions, so
expanding a command twice runs them twice. `external-simple-command-p` therefore returns the
expanded argv as a second value and every caller threads it into `spawn-external`'s `:words`.
Never call `expand-command-words` a second time on a node someone else already classified.

### Control flow must not escape its execution environment

`return`, `break`, `continue` and `exit` are Lisp conditions (`func-return`, `loop-break`,
`loop-continue`, `shell-exit`) that unwind to a catcher. Every bug in this class has been the
same mistake in one of two directions: a catcher too close, swallowing the condition before it
reaches its target, or no catcher at a boundary that POSIX says is one.

**Too close.** `run-builtin` caught `func-return`, so `return` was merely the exit status of the
`return` builtin and control carried straight on: `f() { return 1; echo x; }` printed `x`. Do
not wrap a builtin call in a handler for these conditions. `run` keeps a `func-return` backstop
because `return` outside a function ends the script being read, as it does in a dot script.

**Missing.** POSIX gives each of these its own execution environment, so control flow ends the
construct and becomes its status rather than reaching the enclosing loop, function, or shell:

| boundary | catcher |
|---|---|
| subshell `( ... )` | `exec-subshell` |
| command substitution `$( ... )` | `command-substitute` |
| each stage of a multi-stage pipeline (XCU 2.9.2) | `spawn-stage` |

`echo a | { exit 4; }` sets the pipeline's status to 4; it does not exit the shell. Only
multi-stage pipelines reach `spawn-stage` — `exec-pipeline-raw` runs a lone command directly —
so a bare `return` is unaffected by the catcher there.

**`eval` and `.` are not boundaries.** They run their input inline in the current environment,
so control flow passes straight through. `run-string-capturing` therefore dispatches nodes
itself rather than calling `run` — `run` backstops `shell-exit` and `func-return`, and routing
`eval` through it made `eval "exit 0"` a silent no-op. The one exception is `return` in a dot
script, which ends the script and becomes its status: the `.` builtin adds that handler back.
bash, zsh and dash agree on every case here except a sourced `break`, where zsh is the outlier
and sxsh follows bash and dash.

Two related divergences are **unspecified**, and sxsh follows zsh on both. Do not "fix" them
toward bash without checking the other shells first:

- `break` inside a function called from a loop: bash and dash error and continue; zsh and
  bash 3.2 break the caller's loop, which is what sxsh does.
- `return` in a trap fired outside any function: bash errors and continues; zsh and dash end
  the script with the return status, which is what sxsh does.

**Test these in a non-final position.** The `return` bug survived for months because every test
used `return` as the last command in the function, where a swallowed condition and a working
one look identical. `smoke.sh` has a `cf-*` block that follows each construct with a command
that must, or must not, still run; ten of those checks fail if the catchers are removed.

### The dash job's ~20 failures are expected, and were triaged

CI runs `make posix REF_SHELL=/bin/dash` as an informational job. It reports
about twenty failures; none of them is a conformance bug on our side. They fall
into four groups:

* **11 x `$'...'`** -- dash predates POSIX Issue 8 (2024) and prints the escape
  literally. We implement it, so we are the more conformant of the two here.
* **5 x exit-status value** -- arithmetic and redirection errors: we exit 1,
  dash exits 2. POSIX requires a non-zero status without specifying which.
* **2 x unspecified behaviour** -- `shift` past `$#` (POSIX: "the results are
  unspecified"; we, bash and zsh return 1 and continue, dash exits) and
  `echo` with backslashes (implementation-defined; dash interprets, bash
  does not).
* **2 x arithmetic dash lacks** -- `**` and the comma operator. Note `**` is a
  ksh/bash extension, not POSIX: ISO C, which POSIX defers to, has no
  exponentiation operator.

Do not "fix" these against dash. If the count moves, triage the delta rather
than assuming a regression.

### Use a modern bash as the differential reference

`/bin/bash` on macOS is **3.2.57 (2007)** -- Apple froze it at the last
GPLv2 release. It predates features the standard has since adopted, so it
reports divergences that say nothing about our conformance: `$'\\u0041'`
prints the escape literally there and `A` in any bash from 4.2 on.
`test/posix-diff.sh` therefore prefers /opt/homebrew/bin/bash, then
/usr/local/bin/bash, then /bin/bash, and prints the reference's version in its
summary so a result is always interpretable. Pass a shell explicitly to
override, e.g. `make posix REF_SHELL=/bin/dash`.

### Real scripts find more bugs than hand-written probes

This has been the most productive technique by a wide margin, and it is worth reaching for
before writing another sweep of small cases. Running one autoconf `configure` found eight bugs
— more than the three preceding hand-written differential sweeps combined. Running
`/opt/homebrew/bin/glibtool` (13.5k lines of deliberately primitive portable sh) found the
`eval` control-flow bug immediately, because libtool's `--config` and `--version` end in a
function reached through `eval` that exits.

The method: run the script under sxsh and under a modern bash in **separate scratch
directories**, then compare stdout, stderr and exit status byte for byte. Anything that
differs is a lead. Both must be given identical inputs and a clean directory, or the diff is
noise. Useful invocations are the ones that exercise a lot of the script without needing a
network or a full build — `--help`, `--version`, `--config`, `--dry-run`, and for libtool a
complete `--mode=compile` → `link` → `install` → `finish` → `clean` cycle over a two-file C
project (that whole sequence is byte-identical today).

Where these bugs hide is the giveaway: a construct in a **non-final position**. `return` was
broken for months because every test used it as the function's last command, where a swallowed
condition and a working one look the same. Real scripts are full of non-final positions.

### Two expansion/IO invariants that hung a real script

**The pattern operand of `${var#pat}` is expanded.** POSIX applies tilde, parameter, command
and arithmetic expansion to it — but neither field splitting nor pathname expansion, so
`expand-nested-pattern` uses `expand-pass`, not `expand-word`. It renders the result with
`xchars->pattern` rather than `xchars->string`, which is what keeps a metacharacter that came
from an expansion live (`p="*."; ${s#$p}`) while one quoted in the source stays inert
(`${s#"*"}`). The function used to return the operand untouched, so `${s#$p}` searched for the
literal characters `$p`. Any script that walks a string a character at a time then loops
forever — that is how gpgrt-config hung.

**`read` must not read ahead.** It reads fd 0 a byte at a time via `fd-read-line` and stops at
the newline, because POSIX requires the file offset to be left just past it: the next command
on that descriptor has to see the following byte. Do not "optimize" this into a buffered
stream. Going through SBCL's `*standard-input*` slurped the whole file on the first call, so in
`while read l; do read X <f; done <g` the inner redirected `read` was served leftover bytes
from `g`. `{ read first; cat; } <f` is the cheapest test that the offset is right.

### The Oils spec "hang" is environmental, not a shell bug

Two spec cases (`exit-status` "If subshell true WITH OUTPUT", `arith` "Logical
Ops Short Circuit") time out on macOS with XQuartz installed. They are designed
around commands named `X` and `x` not existing, but `/opt/X11/bin/X` and
`/opt/X11/bin/x` are symlinks to `Xquartz` — so the shell dutifully starts an X
server and waits for it. **bash hangs on the same input.** Do not go looking for
a deadlock in the executor; `test/oils-spec.py` passes `--timeout` so these are
recorded as failures rather than blocking the suite.

The arith case reaches it because `(( ... ))` is ksh/bash arithmetic that we do
not implement, so `(( 0 || (x = 33) ))` parses as nested subshells and runs `x`
as a command. dash does the same thing; it is not a defect.

### Never use `(truename ".")` for the working directory

`sb-posix:chdir` changes the process cwd but leaves `*default-pathname-defaults*` alone, and CL
resolves `"."` against the latter — so `(truename ".")` reports whatever directory the image
started in, forever. Go through `current-directory` / `change-directory` (state.lisp), which
read the real cwd via `getcwd` and keep `*default-pathname-defaults*` in step. Using `truename`
here had `pwd`, `$PWD`, `cd -`, the subshell cwd snapshot, and the async prelude all reporting a
stale directory after the first `cd`.

### FFI layer (spawn.lisp)

`posix_spawn_file_actions_t` and `posix_spawnattr_t` are opaque and represented differently
per platform: inline structs on Linux/glibc (80 / 336 bytes), heap pointers on macOS. The code
hands libc a zeroed byte block sized by `#+darwin` / `#-darwin` reader conditionals and never
inspects the internals. Wait-status decoding uses portable bit arithmetic instead of the
`sb-posix` `W*` macros, which are not exported on every platform. Linux and macOS only;
`main` exits with a message on non-POSIX platforms.

## README drift

`README.md` is behind the code. It still says job control is minimal with "no `jobs`/`fg`/`bg`
table, no process-group/terminal control" and that a backgrounded pipeline runs synchronously —
all three are now implemented (jobs.lisp, `async-pipeline`). Its architecture table omits
`jobs.lisp` and `deparse.lisp`; its test counts (48 + 73 = 121) are stale; and its invocation
snippets predate the Makefile and the `sxsh/shell/test` system. `deparse.lisp`'s header comment
claims deparsed source is used to re-exec the binary with `-c`; in the current code `deparse` is
only used to record a job's display text. Prefer the source over the README, and update the
README when you touch these areas.

## Entry points

```lisp
(sxsh:parse-string "if true; then echo hi; fi")   ; => list of COMPLETE-COMMAND nodes
(sxsh:tokenize "echo $(date) | wc -l")            ; raw token stream

(let ((sh (sxsh-shell:make-shell)))
  (sxsh-shell:run-string sh "for i in 1 2 3; do echo $i; done"))
(sxsh-shell:repl (sxsh-shell:make-shell))
(sxsh-shell:main)                                 ; script / -c / interactive dispatch
```

`main` reads `(rest sb-ext:*posix-argv*)`: no args → interactive REPL, `-c cmd [args]` →
run string, otherwise treat argv[0] as a script path. The EXIT trap runs from an
`unwind-protect` regardless of how the shell terminates.
