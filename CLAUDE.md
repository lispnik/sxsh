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
test/git-suite.py  -> git's own test suite, differentially (make git-tests)
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
make git-tests      # git's t/ suite under sxsh vs bash (slow; builds test-tool once)
make posix          # differential conformance vs bash (247 cases)
make posix REF_SHELL=/bin/dash    # stricter reference
make posix-yash     # strictest reference; rejects extensions (brew install yash)
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

### `local` exists, and is not POSIX Issue 7

Every shell that matters has it and real scripts require it, so sxsh implements it. Scoping is
**dynamic**: `shell-local-frames` is a save/restore stack, one frame per active function call,
and a callee still sees the caller's locals. The frame is popped in `call-function`'s cleanup so
locals are restored however the function ends — `return`, falling off the end, `set -e`, or a
`break` unwinding past it.

Shells disagree on the details, because it went unstandardized for decades. A bare `local x`
leaves x **unset** in bash, **inherited** in dash (its man page says so explicitly), and
**empty** in zsh. sxsh follows bash: that is the reading that makes `local` a declaration. Do
not "fix" this toward dash without a reason.

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

### bash extensions are in scope, tracked by `make bashisms`

sxsh targets POSIX first, but the goal is to support bash's extensions **in addition**.
`test/bashisms.sh` is the scoreboard: ~57 cases, each run under sxsh and under a modern bash
and compared on output+status. It is a scoreboard, never a gate.

Take the expectation from bash, not from what you assume bash does. Two examples that caught
me: `RANDOM=5` **seeds** bash's generator rather than assigning to it (so `echo $RANDOM` after
it prints a random number, not 5), and `unset RANDOM` destroys the dynamic behaviour
permanently, after which it is an ordinary variable and assignment does stick.

Done so far: `${x/pat/rep}` and its `//`, `/#`, `/%` forms; `${x^}` `${x^^}` `${x,}` `${x,,}`
with optional pattern; `${!x}` indirection and `${!prefix*}`; `name+=value`; `set -o pipefail`;
`$RANDOM` and `$SECONDS`. Plus what was already there: `${x:o:l}`, `**`, `$'...'`, `echo -e`,
`printf %*d`, `type -a`, `local`.

Tranche 2 added the parser-level batch: here-strings `<<<`, `&>` / `&>>`, `|&`, the `function`
keyword (with optional `()`), and brace expansion including ranges and steps.

Two things there are easy to get wrong. `&>` has to back up fd 2 as well as fd 1, or restoring
leaves stderr dup'd to the file and every later diagnostic disappears. And brace expansion runs
*before* every other expansion on the RAW word text, so it cannot live inside
`expand-word-to-fields` -- it wraps the call in `expand-command-words` and turns one word into
several. It must skip quoted regions itself, since quote removal has not happened yet.

Tranche 3 added the builtin batch: `printf -v` and `%q`, and `read`'s `-n -N -d -t -u -s`.
`%q`'s escape set was taken by probing bash over every printable character rather than guessed
-- it is not the shell metacharacter set, and notably excludes `#`, `~`, `=`, `%` and `:`.
`-n` and `-N` are separate options rather than one with a flag because `-N` treats no byte as a
delimiter *and* skips IFS trimming entirely. `read -s` echoes its newline only on a terminal;
unconditionally meant a stray blank line in every piped `read -s`.

Tranche 4 added syntax: `case` fall-through (`;&` runs the next body without testing it, `;;&`
keeps testing the patterns that follow -- the parser already recorded both terminators, only
`exec-case` ignored them), plus `((expr))` and `for ((init;cond;step))`.

`((` is lexed as a single `:dlparen` token carrying the inner text, recognised whenever a
matching `))` follows. It deliberately does NOT check for command position -- the parser never
passes that flag -- so `((a); (b))` stops being a subshell-of-subshell. bash resolves the same
ambiguity the same way, and `( (a); (b) )` with spaces is unaffected. Note `((expr))`'s status
is INVERTED from the value: an arithmetic 0 is false, which is what makes `((i < n))` work as a
loop condition.

Tranche 5 added arrays, indexed and associative. A variable cell is still `(VALUE .
EXPORTED-P)`; for an array VALUE is an `SH-ARRAY` instead of a string, so nothing that only
reads scalars had to change -- `get-var` still returns a string, using `scalar-of`, because
bash's `$a` means `${a[0]}`. Both flavours use a hash table because bash arrays are **sparse**:
`a[5]=x` on an empty array leaves 0-4 genuinely absent, not empty.

Four things this broke or nearly broke, all worth knowing before touching it again:

- **The RHS of an array literal must reach the executor RAW.** `apply-assignments` expands the
  value first, which quote-removes it, so `a=(1 "b c" 3)` arrived as `(1 b c 3)` and split into
  four elements. `array-literal-p` is checked on the raw word text and the expansion is skipped.
- **`${a[@]}` needs field separators**, which only the caller can emit -- so it is special-cased
  in `expand-pass` and `expand-double` alongside `$@`, not inside `expand-braced-param`. `[*]`
  joins, `[@]` splits, exactly as `$*` and `$@` differ.
- **The async re-exec serialises variables** into a prelude. Arrays cannot travel as scalars, so
  they are emitted as `name=([k]=v ...)` literals (with a `declare -A` first when associative);
  otherwise `shell-quote` is handed a struct and every `{ ... } &` test fails.
- **`declare -i` is an attribute, not a one-off.** Every *later* assignment to the name is
  evaluated arithmetically too, which is the point of `declare -i n; n=3+4`. It lives in
  `shell-int-vars`.

Arrays are never exported: there is nowhere in the environment to put subscripts, and bash does
not export them either.

Tranche 6 added `[[ ]]`, including `=~`.

**`[[ ]]` is not `test` with different spelling**, which is why it is a separate node rather
than sugar. Inside it there is no field splitting and no pathname expansion, `<` and `>` are
string comparisons rather than redirections, the right operand of `=`/`==`/`!=` is a **pattern**
whose metacharacters are live unless quoted in the source, and `=~` is a regular expression.
The lexer therefore scans `[[ ... ]]` whole (like `((`) and the words are split at execution
time, where expansion happens. File and arithmetic comparisons delegate to `eval-test` so the
two can never drift apart.

`shell/regex.lisp` is a small backtracking POSIX ERE engine, written rather than depended on:
the project has no external libraries -- only sb-posix, which ships with SBCL -- and pulling in
a regex library for one operator would change the build story for CI and for anyone cloning.
Capture groups had to be tracked regardless, since bash exposes them as `$BASH_REMATCH`. It
supports the ERE set (`. [] * + ? {n,m} | () ^ $`, character classes) but **not** backreferences,
which are not in POSIX ERE either. Note two details: a group's span is undone on backtracking,
or a group matched inside a rejected branch leaks into `BASH_REMATCH`; and a quoted section of
the `=~` operand is escaped rather than quote-removed, so `[[ $x =~ "a.c" ]]` wants a literal
dot.

Tranche 7 added `shopt` (a namespace separate from `set -o`, as in bash), the `ERR` and `DEBUG`
traps, and `select`. `nullglob` and `dotglob` genuinely take effect rather than merely being
remembered -- a shopt that is stored and ignored is worse than an absent one, because scripts
feature-detect with `shopt -q`.

`select` is deliberately NOT in `+reserved+`. `reservedp` compares token text and does not
consult that list, so the `select` branch in `parse-command` still fires; but `any-reserved-p`
does consult it, and listing `select` there stopped `select() { ...; }` being recognised as a
function definition. `select` is not POSIX, so a conforming script may legitimately define a
function with that name -- bash rejects it, we do not. The same reasoning does not apply to
`function`, which stays listed.

`shell/regex.lisp` has direct unit coverage in `shell/test-shell.lisp` (`run-regex-tests`),
because reaching the engine's corners through `[[ =~ ]]` alone is awkward: empty matches,
greedy backtracking, and group spans that must be undone when an alternative is rejected.

Tranche 8 added namerefs (`declare -n` / `local -n`) and extglob.

Namerefs live in `shell-namerefs` and are resolved by `get-var` and `set-var`, so a read or a
write through the alias reaches the target. `resolve-nameref` is depth-limited: `declare -n a=b;
declare -n b=a` is a cycle, and bash reports it rather than hanging.

**extglob is gated in the matcher but NOT in the lexer, deliberately.** The matcher checks
`*extglob*` because without the option `ab?(c)` really does mean `ab?` followed by something
else. The lexer cannot be gated the same way: sxsh parses a whole script before running any of
it, so `shopt -s extglob` on line 1 has not executed when line 2 is lexed, and `case x in
a?(b))` would still fail to parse. Scanning the group into the word unconditionally is safe
because the alternative reading -- a subshell in pattern position -- is a syntax error in bash
too, so this only makes sxsh more permissive at parse time while the *meaning* of a pattern
still changes only when extglob is on.

Note the extglob branch in `pat-match` must come BEFORE the plain `*` and `?` branches, or
those consume the quantifier character first and `?(...)`/`*(...)` silently fall back to
ordinary wildcards. `@`, `+` and `!` are not wildcards, so they worked while the other two did
not -- a good reminder that partial success here is not success.

Tranche 9 added process substitution. `<(cmd)` creates a pipe, runs the command in a re-exec of
this binary with one end wired to its stdout, and substitutes `/dev/fd/N` naming the end we
keep. The descriptor is deliberately left inheritable -- the command being built is spawned
afterwards and has to open that path -- and is closed by `exec-simple` once that command
finishes, via `*procsub-fds*`. Leaving it open leaks a descriptor per `<(...)` and, worse, keeps
the pipe's write end alive so a reader never sees EOF. It works as an argument and as a
redirection target (`wc -l < <(...)`).

Two traps in the lexing, both of which bit:

- `<` is an **operator-start** character, so `next-token` never reaches `scan-word`. The `<(`
  case has to be caught in `next-token`, before the operator branch.
- Inside `scan-word`, the group must be consumed **before** the loop. `word-char-terminator-p`
  is tested at the top of the loop and `<` is a terminator, so the word ended immediately, the
  caller re-scanned from the same position, and the lexer looped until the heap was exhausted.

The `(` must be adjacent: `cat < (echo x)` stays a redirection of a subshell, as in bash.

Still missing: `coproc`, and a subscript containing whitespace (`m[a b]=v`, which the lexer
splits)
-- which drag in `declare`/`local` options, `PIPESTATUS`, `read -a`, `mapfile` and namerefs, and
which touch the variable model everywhere, so they want their own tranche.

Note the tension with the previous section: every extension added here is one more thing a
strict mode would have to gate, and one more divergence `make posix-yash` will report. That is
the intended trade, not a regression -- but keep the two scoreboards in view together.

### yash is the strictest reference, and the only one that rejects extensions

`make posix-yash` runs the differential suite against yash (`brew install yash` / `apt install
yash`). It is worth having as a *second* reference, not a replacement for bash, for one reason:
it is the only shell here whose POSIX mode genuinely **rejects** extensions rather than merely
adjusting behaviour. Under `yash --posix`, arrays, the `function` keyword and here-strings are
all parse errors, and even by default yash has no `**`, no `${v:o:l}` and no `source`. That
makes it the reference that finds bashisms we ship without noticing.

It found six: `type -p` and `type -t` (POSIX `type` takes no options), `printf %*d` (no `*`
field width in POSIX), and `**` in arithmetic. Those are real, and are the first candidates for
a strict-mode gate.

**Do not treat every yash divergence as our bug.** The rest of its ~18 fall into four buckets,
none of them conformance failures on our side:

- **Status 1 vs 2, both non-zero.** `readonly r=1; r=2`, `$((1/0))`, `$((1%0))`, `umask 999`:
  all four shells abort, sxsh and bash exit 1, dash and yash exit 2. POSIX requires only a
  non-zero status, so the value is unspecified.
- **Diagnostic and output format.** `type` prints `cc: an external command at /usr/bin/cc` in
  yash; POSIX does not specify the wording. Same for redirection failure messages.
- **Features yash lacks.** `\u` inside `$'...'` (it has `$'...'`, just not that escape), and the
  arithmetic comma operator, which ISO C has and POSIX inherits — yash is arguably wrong there.
- **yash idiosyncrasies.** `command -v echo` prints an external path rather than `echo`.

**Beware probes that lean on `echo`.** What `echo` does with a backslash in its operand is
implementation-defined: XSI echo expands escapes, bash's does not. `echo "a\\b"` prints `a\b`
under bash and sxsh but `a` under dash and yash, so a probe written that way tests the reference
shell's choice of `echo` rather than the thing it means to test. Use `printf "%s\n"`. One probe
(`q-backslash`) had exactly this defect and yash is what exposed it.

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

### git's test suite is the most demanding real script we have

`make git-tests` runs git's own t/ suite under sxsh (`test/git-suite.py`, submodule
`third_party/git` pinned to v2.55.0). test-lib.sh and test-lib-functions.sh are ~3000 lines of
deliberately portable sh, and each tNNNN script drives them through hundreds of cases, so it
reaches constructs our own tests barely touch.

It is **differential, and that is the point**: every script runs twice, once under sxsh and
once under a reference bash, and only tests that PASS under the reference and FAIL under sxsh
are reported. Tests needing an unbuilt helper or an absent filesystem feature fail for both and
cancel out, so every line of output is a real lead.

Three setup details, all handled by the harness — it does not build git:

- `GIT-BUILD-OPTIONS` is generated with `make GIT-BUILD-OPTIONS`, a target that only runs `sed`.
  test-lib.sh sources it and bails without it.
- `templates/blt` normally comes from a full build; we symlink the installed git's templates.
- `t/helper/test-tool` genuinely must be compiled — test-lib.sh refuses to run otherwise. Only
  that target is built, once, then cached in the submodule. `ignore = dirty` in .gitmodules
  keeps the resulting build products out of `git status`.

The installed git is used via `GIT_TEST_INSTALLED`, so nothing links against the submodule.

**Failures here are often order-dependent, and that is a feature.** Three t0001 failures
vanished when run with `--run=N` alone: an earlier test was leaking state into them. Bisecting
with `--run=13-18`, `--run=14-18` found the culprit, and the root cause was the assignment-prefix
bug below — `GIT_DIR=x git init` left GIT_DIR set for every later test. A suite that runs
hundreds of cases in one shell process is the only thing that finds that class.

### Assignment prefixes are command-scoped, except on special built-ins

POSIX 2.9.1: `V=x cmd` puts V in the command's environment and leaves the shell's V alone —
unless `cmd` is a **special** built-in (`+special-builtins+` in exec.lisp), where it persists.
sxsh persisted for every command type, which is what poisoned the git tests above.

Two things make this easy to get wrong in either direction:

- The binding must still be **visible** while the command runs. `IFS=: read x y` is the whole
  point of the feature. `apply-assignments` without `:to-shell` cannot serve, because it
  restores before returning — right for an external command, which gets the values through its
  environment, wrong for anything in-process. `bind-assignments` / `restore-assignments` are
  the in-process pair.
- Functions do **not** persist, matching bash, dash and zsh — do not lump them in with special
  built-ins.

Use `bash --posix` as the reference for this rule. Plain bash does not persist even for special
built-ins; that is one of the deviations `set -o posix` exists to fix, so plain bash will
mislead you here. It is a better reference generally, though it is not a strict mode — arrays,
`[[ ]]` and `<<<` all still work under it.

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
