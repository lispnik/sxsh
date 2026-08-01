# sxsh — a POSIX shell in Common Lisp

Two ASDF systems: a parser for the POSIX shell command language, and an
executor built on it that runs as a working shell. SBCL only; Linux and macOS.

```
make check     # build and run every suite (540 checks)
make build     # save a standalone ./sxsh
./sxsh -c 'for i in 1 2 3; do echo $i; done'
./sxsh script.sh args...
./sxsh                     # interactive, with job control
```

---

# sxsh — the parser

Parses the POSIX shell command language (IEEE Std 1003.1, §2.10) into an AST,
implementing the token recognizer of §2.3 and a recursive-descent parser,
including the lexer/parser coupling the standard requires.

```lisp
(asdf:load-system "sxsh")

(sxsh:parse-string "if true; then echo hi; fi")
;; => list of COMPLETE-COMMAND AST nodes

(sxsh:tokenize "echo $(date) | wc -l")   ; raw token stream
```

## What it handles

- Simple commands: assignments, words, redirections, in POSIX order
- Quoting kept verbatim: `'...'`, `"..."`, `` `...` ``, `$(...)`, `${...}`,
  with correct balanced/nested scanning
- Pipelines (`|`), with leading `!`; and-or lists (`&&`, `||`), left-associative
- Lists with `;` and `&` separators
- All redirection operators: `< > >> << <<- <& >& <> >|`, optional IO_NUMBER
- Here-documents, including `<<-` tab stripping and quoted-delimiter detection,
  with bodies collected via parser/lexer feedback at the next newline
- Compound commands: `( )`, `{ }`, `if/elif/else`, `for` (incl. bare `for x`),
  `while`, `until`, `case` (incl. `|` patterns, `;;`, empty items)
- Function definitions `name() compound-command`
- Comments, blank lines, `\`-newline continuations
- Position-sensitive reserved words (reserved only in command position)

## Design notes

The POSIX shell grammar is not purely context-free: token classification
depends on parser context. This implementation follows the standard's actual
structure — the parser drives the lexer. Reserved words are recognized only
where the grammar permits them; here-doc bodies are read by the lexer on
demand after the parser registers the operator; IO_NUMBER and ASSIGNMENT_WORD
are contextual token types.

Expansion is a runtime concern and is deliberately *not* performed here —
words retain their raw source text. That is also what lets the executor
`deparse` an AST back into equivalent source.

---

# sxsh/shell — the executor

Executes the AST as a working shell. Every external command is launched with
**`posix_spawnp(3)`** via SBCL's alien FFI — there is no `fork`/`exec` and no
use of `sb-ext:run-program`.

```lisp
(asdf:load-system "sxsh/shell")

(let ((sh (sxsh-shell:make-shell)))
  (sxsh-shell:run-string sh "for i in 1 2 3; do echo $i; done"))

(sxsh-shell:repl (sxsh-shell:make-shell))
(sxsh-shell:main)          ; script / -c / interactive
```

## Architecture

| File | Responsibility |
|------|----------------|
| `shell/spawn.lisp` | FFI for `posix_spawnp`, file-actions, attributes; argv/env marshalling; `SETPGROUP` and `SETSIGDEF` |
| `shell/state.lisp` | Variables (with export flags), functions, positional params, `$?`/`$$`/`$!`, options, working directory |
| `shell/expand.lisp` | Word expansion (2.6): tilde, parameter, command & arithmetic substitution, field splitting on `$IFS`, globbing, quote removal |
| `shell/arith.lisp` | `$(( ))` integer arithmetic, C-like precedence, `**`, assignment ops, ternary |
| `shell/redir.lisp` | Redirections as `posix_spawn` file-actions (external) or `dup2`-with-restore (builtins/compounds); here-doc temp files |
| `shell/deparse.lisp` | AST back to shell source — used to run compounds asynchronously and to label jobs |
| `shell/jobs.lisp` | Job table, process groups, terminal ownership, `set -m`, reaping |
| `shell/builtins.lisp` | The builtin set (see below) |
| `shell/exec.lisp` | Tree-walking executor: pipelines, and-or lists, functions, subshells, compounds, command substitution, `set -e` scoping |
| `shell/driver.lisp` | `run`, `repl`, `main` |

## How commands run

- **External command** → argv/env marshalled to C, redirections compiled to
  file-actions, `posix_spawnp` creates the child; the parent `waitpid`s and
  decodes the status (`128+signo` for signals).
- **Pipeline** → one `pipe(2)` per stage boundary, wired with `dup2`
  file-actions. Builtin stages run in-process with fds temporarily redirected.
- **Builtin / function / compound** → run in the shell process (they must, to
  affect shell state); redirections applied with `dup2` and restored after.
- **Command substitution** → captured through a temp file, so large output
  cannot deadlock a bounded pipe; trailing newlines stripped.
- **Subshell** `( … )` → in-process with a snapshot/restore of variables,
  functions, positional params, and cwd.
- **Background compound** `{ …; } &`, `f &` → since we cannot fork, the shell
  re-execs itself as `sxsh -c <source>`, replaying cwd, shell variables,
  functions and positional parameters as a generated prelude. Exported
  variables travel in the environment.

## Language features

Simple commands, assignments and temporary command-env assignments, quoting,
pipelines (`|`, leading `!`), and-or lists, `;` and `&` separators, all
redirection operators with optional fd numbers, here-documents (incl. `<<-`
and quoted delimiters), `if/elif/else`, `for` (incl. `for x` over `"$@"`),
`while`, `until`, `case`, brace groups, subshells, function definitions, and
the `time` reserved word (with `-p`).

Expansion: tilde (incl. after `:` in assignments), parameter (`${x:-w}`,
`${x#pat}`, `${x%%pat}`, `${#x}`, `${x:off:len}`, and the assign/alt/error
variants), command and arithmetic substitution, `$?`/`$$`/`$!`/`$#`/`$-`/`$0`,
field splitting on `$IFS`, and pathname globbing. `"$@"` yields one field per
positional parameter (none when there are none); `"$*"` joins into one.

`set -e` follows POSIX scoping: it is ignored while evaluating the condition of
`if`/`elif`/`while`/`until`, in any command of an and-or list but the last, in
any pipeline stage but the last, and for a pipeline led by `!`.

Builtins: `: true false echo printf pwd cd export unset shift exit return break
continue read set eval . source type test [ command getopts trap wait kill jobs
fg bg umask hash readonly times exec alias unalias`. `set` supports
`-e -x -u -f -m`. `trap` installs real signal handlers plus `EXIT`, whose
status follows POSIX (the trap's own commands do not overwrite it).

## Job control

Active when interactive with a controlling terminal, and toggled by `set -m`.
Each asynchronous job runs in its own process group (via `posix_spawn`'s
`SETPGROUP`), tracked in a job table with `jobs`, `fg`, `bg` and `kill`,
addressable as `%n`, `%%`, `%-`, `%prefix` or a bare job number. Ctrl-Z stops
the foreground job; `bg`/`fg` resume it; a job resumed from outside the shell
is noticed via `WCONTINUED`. Terminal ownership goes through a private
`/dev/tty` fd, so it keeps working when the shell's own stderr is redirected.
Ctrl-C and Ctrl-\ at the prompt return to the prompt rather than killing the
shell.

## Platform support

Runs on **Linux and macOS** — any POSIX system with `posix_spawn`. Windows is
not supported; `main` exits with a clear message on non-POSIX platforms.

Two portability hazards this code takes seriously, because both produced real
bugs that were invisible on one platform:

- **Signal numbers differ.** SIGCONT is 18 on Linux and 19 on macOS; SIGTSTP 20
  vs 18; SIGCHLD 17 vs 20; `WCONTINUED` 8 vs 16. Every signal number comes from
  `sb-unix` or a reader conditional, never a literal.
- **The spawn objects differ.** On Linux/glibc `posix_spawn_file_actions_t` and
  `posix_spawnattr_t` are inline structs (80 and 336 bytes); on macOS they are
  pointers that `..._init()` fills with a `malloc`'d struct. We hand libc a
  zeroed block sized for either via `#+darwin` conditionals and only touch the
  internals through libc accessors.

Wait-status decoding uses portable bit arithmetic rather than the `sb-posix`
`W*` macros, which are not exported on every platform.

## Deliberate limitations

Interactive niceties (line editing, history, completion, `PS1` escape
expansion) are absent. `hash` is a functional stub, since `$PATH` is resolved
fresh each time. Non-POSIX extensions — `[[ ]]`, `((...))`, `for ((;;))`,
`<<<`, `{a,b}` brace expansion, `$RANDOM`, arrays — are not implemented.
Backgrounding a compound requires a saved executable; under `sbcl --load` it
falls back to running synchronously.

---

## Testing

```
make check          # everything below
make test-parser    # 48   parser unit tests           (in-image)
make test-shell     # 84   executor unit tests         (in-image)
make smoke          # 98   end-to-end vs ./sxsh
make jobs           # 14   job control through a real pty
make posix          # 76   differential vs a reference shell
```

The layers deliberately overlap, because each reaches something the others
cannot. The in-image suites never touch `main`, argv handling or process exit
status. `smoke.sh` drives the saved executable and covers exactly that.
`test/jobs-pty.py` allocates a pty and puts sxsh in its own session — without a
controlling terminal the job-control paths are never entered at all.
`test/posix-diff.sh` runs the same source through sxsh and a reference shell
and compares output and status, so the expected answer comes from a conforming
implementation rather than from our own assumptions; the handful of cases where
shells legitimately differ are annotated in that file with the reason.

```
make posix REF_SHELL=/bin/dash    # stricter reference than bash
```
