# sxsh --- POSIX shell parser + executor in Common Lisp (SBCL only)

LISP    ?= sbcl
# ocicl is the toolchain: its runtime, loaded from ~/.sbclrc by `ocicl setup',
# supplies ASDF and points the source registry at the current directory.
#
# (require :asdf) is added ONLY when that runtime is absent. Doing it
# unconditionally loads SBCL's bundled ASDF over ocicl's, after which the
# system search falls through to ocicl's own finder and dies looking for an
# ocicl.csv this project has no reason to have.
OCICL_RUNTIME := $(HOME)/.local/share/ocicl/ocicl-runtime.lisp
ASDF_FALLBACK := $(if $(wildcard $(OCICL_RUNTIME)),,--eval '(require :asdf)')
SBCL    := $(LISP) --non-interactive $(ASDF_FALLBACK)
BINDIR  := bin
BIN     := $(BINDIR)/sxsh
CACHE   := $(HOME)/.cache/common-lisp

PYTHON  ?= python3
# Leave REF_SHELL empty so posix-diff.sh picks a modern bash itself; macOS
# /bin/bash is 3.2 and misreports conformance.
REF_SHELL ?=
# yash for `make posix-yash'. Resolved from PATH so it works wherever it is
# installed; the target reports plainly if it is absent.
YASH ?= $(shell command -v yash 2>/dev/null)
# Iterations etc. for `make fuzz'; findings are reproducible from the seed.
FUZZ_ARGS ?= -n 1000
# Spec files from third_party/oils that are in scope for a POSIX shell.
OILS_SPECS ?= smoke posix quote word-split var-sub exit-status pipeline \
              command-sub arith assign redirect loop case_ if_ subshell \
              builtin-echo builtin-read builtin-trap builtin-cd glob tilde

.PHONY: all build test test-parser test-shell smoke jobs posix oils fuzz check clean help

all: build

## build: save a standalone bin/sxsh executable
build: $(BIN)

$(BIN): sxsh.asd build.lisp $(wildcard *.lisp) $(wildcard shell/*.lisp)
	$(SBCL) --load build.lisp

## test: run both in-image ASDF suites (parser + executor)
test: test-parser test-shell

## test-parser: parser suite only (48 cases)
test-parser:
	$(SBCL) --eval '(asdf:test-system "sxsh")'

## test-shell: executor suite only (84 cases)
test-shell:
	$(SBCL) --eval '(asdf:test-system "sxsh/shell")'

## smoke: build the binary and drive it end-to-end
smoke: $(BIN)
	./smoke.sh ./$(BIN)

## jobs: job-control tests, driven through a real pty
jobs: $(BIN)
	$(PYTHON) test/jobs-pty.py ./$(BIN)

## term-tests: raw mode and key decoding, through a real pty
term-tests: $(BIN)
	$(PYTHON) test/term-pty.py

## lineedit: the line editor, driven through a real pty
lineedit-tests: $(BIN)
	$(PYTHON) test/lineedit-pty.py ./$(BIN)

## history: history and `fc', driven through a real pty
history-tests: $(BIN)
	$(PYTHON) test/history-pty.py ./$(BIN)

## posix: differential conformance suite against a reference shell
posix: $(BIN)
	./test/posix-diff.sh ./$(BIN) $(REF_SHELL)

## posix-yash: same suite against yash, the strictest reference available
posix-yash: $(BIN)
	@test -n "$(YASH)" || { echo "posix-yash: yash not found (brew install yash / apt install yash)"; exit 1; }
	@./test/posix-diff.sh ./$(BIN) $(YASH) || true

# Not part of `check': yash is a scoreboard, not a gate. It is the only shell
# here whose POSIX mode actually REJECTS extensions (arrays, `function',
# here-strings are all parse errors under --posix), and by default it lacks
# `**', `${v:o:l}' and `source', so it is the reference that finds bashisms we
# ship without noticing. It also has idiosyncrasies of its own -- see
# CLAUDE.md before treating one of its divergences as our bug.

## oils: Oils cross-shell spec tests (needs the submodule + a python2)
oils: $(BIN)
	@$(PYTHON) test/oils-spec.py --summary $(OILS_SPECS) || true

# `oils` is deliberately not part of `check`: it is a scoreboard to drive down,
# not a pass/fail gate.
## git-tests: run git's own test suite under sxsh, differentially vs bash
git-tests: $(BIN)
	@$(PYTHON) test/git-suite.py $(GIT_TESTS) || true

# Also a scoreboard rather than a gate, and a slow one: the first run compiles
# t/helper/test-tool (a few minutes, then cached in the submodule). Only tests
# that PASS under the reference shell and FAIL under sxsh are reported, so
# every line of output is a real lead.
#   make git-tests GIT_TESTS="t0000-basic t3600-rm"
#   test/git-suite.py --list        # everything available
## bashisms: scoreboard of bash extensions supported so far (not a gate)
bashisms: $(BIN)
	@./test/bashisms.sh ./$(BIN) $(REF_SHELL)

## fuzz: throw mutated and random input at the parser (add --exec to go deeper)
fuzz: $(BIN)
	$(PYTHON) test/fuzz-parser.py $(FUZZ_ARGS)

## check: everything -- both suites, smoke, job control, POSIX conformance
check: test smoke jobs term-tests history-tests lineedit-tests posix

## clean: remove built executables and this project's compiled fasls
clean:
	rm -rf $(BINDIR)
	rm -rf $(CACHE)/*/$(CURDIR)

help:
	@grep -E '^## ' $(MAKEFILE_LIST) | sed 's/## /  make /'
