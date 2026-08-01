# posh --- POSIX shell parser + executor in Common Lisp (SBCL only)

LISP    ?= sbcl
SBCL    := $(LISP) --non-interactive
BIN     := posh
CACHE   := $(HOME)/.cache/common-lisp

PYTHON  ?= python3
# Leave REF_SHELL empty so posix-diff.sh picks a modern bash itself; macOS
# /bin/bash is 3.2 and misreports conformance.
REF_SHELL ?=
# Spec files from third_party/oils that are in scope for a POSIX shell.
OILS_SPECS ?= smoke posix quote word-split var-sub exit-status pipeline \
              command-sub arith assign redirect loop case_ if_ subshell \
              builtin-echo builtin-read builtin-trap builtin-cd glob tilde

.PHONY: all build test test-parser test-shell smoke jobs posix oils check clean help

all: build

## build: save a standalone ./posh executable
build: $(BIN)

$(BIN): posh.asd build.lisp $(wildcard *.lisp) $(wildcard shell/*.lisp)
	$(SBCL) --load build.lisp

## test: run both in-image ASDF suites (parser + executor)
test: test-parser test-shell

## test-parser: parser suite only (48 cases)
test-parser:
	$(SBCL) --eval '(asdf:test-system "posh")'

## test-shell: executor suite only (84 cases)
test-shell:
	$(SBCL) --eval '(asdf:test-system "posh/shell")'

## smoke: build the binary and drive it end-to-end
smoke: $(BIN)
	./smoke.sh ./$(BIN)

## jobs: job-control tests, driven through a real pty
jobs: $(BIN)
	$(PYTHON) test/jobs-pty.py ./$(BIN)

## posix: differential conformance suite against a reference shell
posix: $(BIN)
	./test/posix-diff.sh ./$(BIN) $(REF_SHELL)

## oils: Oils cross-shell spec tests (needs the submodule + a python2)
oils: $(BIN)
	@$(PYTHON) test/oils-spec.py --summary $(OILS_SPECS) || true

# `oils` is deliberately not part of `check`: it is a scoreboard to drive down,
# not a pass/fail gate.
## check: everything -- both suites, smoke, job control, POSIX conformance
check: test smoke jobs posix

## clean: remove the executable and this project's compiled fasls
clean:
	rm -f $(BIN)
	rm -rf $(CACHE)/*/$(CURDIR)

help:
	@grep -E '^## ' $(MAKEFILE_LIST) | sed 's/## /  make /'
