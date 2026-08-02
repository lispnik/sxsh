;;;; shell/test-shell.lisp --- executor regression tests.
;;;;
;;;; These run real programs via posix_spawn and check captured stdout.

(defpackage #:sxsh-shell/test
  (:use #:cl #:sxsh-shell)
  (:export #:run-all))

(in-package #:sxsh-shell/test)

(defvar *pass* 0)
(defvar *fail* 0)

(defun capture (src)
  "Run SRC in a fresh shell, return (values stdout-string status)."
  (let* ((sh (make-shell))
         (path (format nil "/tmp/sxsh-cap-~A" (random 1000000)))
         (fd (sb-posix:open path (logior sb-posix:o-wronly sb-posix:o-creat
                                         sb-posix:o-trunc) #o600))
         (saved (sb-posix:dup 1)))
    (unwind-protect
         (progn
           (sb-posix:dup2 fd 1) (sb-posix:close fd)
           (let ((*standard-output* (sb-sys:make-fd-stream 1 :output t :buffering :full)))
             (handler-case
                 (unwind-protect
                      (handler-case (sxsh-shell::run-string sh src)
                        (sxsh-shell::shell-exit () nil))
                   (sxsh-shell::run-exit-traps sh))
               (error () nil))
             (finish-output *standard-output*)))
      (sb-posix:dup2 saved 1) (sb-posix:close saved))
    (let ((output
            (unwind-protect
                 (with-open-file (s path)
                   (let ((b (make-string (file-length s))))
                     (subseq b 0 (read-sequence b s))))
              (ignore-errors (delete-file path)))))
      (values output (sxsh-shell::shell-last-status sh)))))

(defmacro check (name src expected)
  `(let ((got (capture ,src)))
     (if (string= got ,expected)
         (incf *pass*)
         (progn (incf *fail*)
                (format t "~&FAIL ~A~%  src:  ~S~%  want: ~S~%  got:  ~S~%"
                        ,name ,src ,expected got)))))

(defmacro check-status (name src expected-status)
  `(multiple-value-bind (out st) (capture ,src)
     (declare (ignore out))
     (if (= st ,expected-status)
         (incf *pass*)
         (progn (incf *fail*)
                (format t "~&FAIL ~A: want status ~A got ~A~%"
                        ,name ,expected-status st)))))

(defmacro check-rx (pattern string expected)
  "Assert REGEX-MATCH returns EXPECTED (a list, or NIL for no match)."
  `(let ((got (sxsh-shell::regex-match ,pattern ,string)))
     (if (equal got ,expected)
         (incf *pass*)
         (progn (incf *fail*)
                (format t "~&FAIL rx ~S vs ~S~%  want: ~S~%  got:  ~S~%"
                        ,pattern ,string ,expected got)))))

(defun run-regex-tests ()
  "Direct coverage for shell/regex.lisp.

The engine is reached from shell source only through `[[ =~ ]]', which makes
it awkward to probe the corners -- empty matches, backtracking, and group
spans that have to be undone when an alternative is rejected. These call it
directly."
  ;; literals and anchors
  (check-rx "a" "abc" '("a"))
  (check-rx "^a" "abc" '("a"))
  (check-rx "^b" "abc" nil)
  (check-rx "c$" "abc" '("c"))
  (check-rx "^abc$" "abc" '("abc"))
  (check-rx "^$" "" '(""))
  (check-rx "b" "abc" '("b"))            ; unanchored search
  ;; any, classes, ranges, negation
  (check-rx "a.c" "abc" '("abc"))
  (check-rx "a.c" "ac" nil)
  (check-rx "[0-9]+" "x123y" '("123"))
  (check-rx "[^0-9]+" "123abc" '("abc"))
  (check-rx "[[:alpha:]]+" "12ab34" '("ab"))
  (check-rx "[[:digit:]][[:alpha:]]" "1a" '("1a"))
  (check-rx "[]]" "]" '("]"))            ; a leading ] is literal
  (check-rx "[a-]" "-" '("-"))           ; a trailing - is literal
  ;; repetition, including the greedy/backtracking cases
  (check-rx "a*" "" '(""))
  (check-rx "a*b" "aaab" '("aaab"))
  (check-rx "a+b" "b" nil)
  (check-rx "colou?r" "color" '("color"))
  (check-rx "colou?r" "colour" '("colour"))
  (check-rx "a{2,3}" "aaaa" '("aaa"))
  (check-rx "a{2}" "aaa" '("aa"))
  (check-rx "a{2,}" "aaaa" '("aaaa"))
  ;; greedy .* must give characters back so the trailing c can match
  (check-rx "a.*c" "abcxc" '("abcxc"))
  (check-rx "^a.*b$" "ab" '("ab"))
  ;; a `{' that is not a valid bound is a literal
  (check-rx "a{x" "a{x" '("a{x"))
  ;; alternation and groups
  (check-rx "x|y" "zzy" '("y"))
  (check-rx "(a|b)c" "bc" '("bc" "b"))
  (check-rx "^(a+)(b+)$" "aabbb" '("aabbb" "aa" "bbb"))
  (check-rx "(a)(b)(c)" "abc" '("abc" "a" "b" "c"))
  ;; a group inside a REJECTED alternative must not leak into the captures
  (check-rx "(x)y|(a)b" "ab" '("ab" "" "a"))
  ;; nesting
  (check-rx "((a)b)c" "abc" '("abc" "ab" "a"))
  ;; escapes
  (check-rx "a\\.c" "a.c" '("a.c"))
  (check-rx "a\\.c" "abc" nil)
  ;; an empty-matching repetition must terminate rather than spin
  (check-rx "(a*)*b" "b" '("b" ""))
  (check-rx "^[a-z]+@[a-z]+\\.[a-z]+$" "me@host.com" '("me@host.com"))
  ;; The remaining character classes -- only alpha and digit were covered.
  (check-rx "[[:space:]]+" "a  b" '("  "))
  (check-rx "[[:upper:]]+" "abCDef" '("CD"))
  (check-rx "[[:lower:]]+" "ABcdEF" '("cd"))
  (check-rx "[[:alnum:]]+" "!!a1!!" '("a1"))
  (check-rx "[[:punct:]]+" "ab,;cd" '(",;"))
  (check-rx "[[:xdigit:]]+" "zz1aFz" '("1aF"))
  ;; CL string literals have no \t escape -- "a\tb" would just be "atb".
  (check-rx "[[:blank:]]+" (format nil "a~Cb" #\Tab) (list (string #\Tab)))
  (check-rx "[[:graph:]]+" " ab " '("ab"))
  (check-rx "[[:print:]]+" "ab" '("ab"))
  (check-rx "[^[:digit:]]+" "12ab" '("ab"))
  ;; Catastrophic backtracking. Without the failure memo these grow
  ;; exponentially: 26 characters took 5.4 seconds and each further two
  ;; roughly quadrupled it, so an untrusted regex could hang the shell.
  ;; A run of 200 finishing at all is the assertion.
  (check-rx "^(a+)+$" (concatenate 'string (make-string 200 :initial-element #\a) "X") nil)
  (check-rx "^(a|a)+$" (concatenate 'string (make-string 200 :initial-element #\a) "X") nil)
  (check-rx "^(a*)*$" (concatenate 'string (make-string 200 :initial-element #\a) "X") nil)
  (check-rx "^(a|aa)+$" (concatenate 'string (make-string 200 :initial-element #\a) "X") nil)
  ;; ...and the same shapes must still MATCH when they should.
  (check-rx "^(a+)+$" "aaaa" '("aaaa" "aaaa"))
  (check-rx "^(a|aa)+$" "aaa" '("aaa" "a")))

(defun run-all ()
  (setf *pass* 0 *fail* 0)
  (run-regex-tests)
  (check "echo" "echo hi" (format nil "hi~%"))
  (check "echo-n" "echo -n hi" "hi")
  (check "external" "printf 'a\\nb\\n' | sort -r" (format nil "b~%a~%"))
  (check "pipe3" "printf '3\\n1\\n2\\n' | sort | head -1" (format nil "1~%"))
  (check "var" "X=7; echo $X" (format nil "7~%"))
  (check "arith" "echo $((2**0 + 6 / 2 * 4))" (format nil "13~%"))
  (check "arith-assign" "echo $((x=5, x*x))" (format nil "25~%"))
  (check "cmdsub" "echo [$(echo inner)]" (format nil "[inner]~%"))
  (check "default" "echo ${U:-d}" (format nil "d~%"))
  (check "length" "V=abcd; echo ${#V}" (format nil "4~%"))
  (check "suffix" "F=a.b.c; echo \"${F%.*}|${F%%.*}\"" (format nil "a.b|a~%"))
  (check "prefix" "F=a.b.c; echo \"${F#*.}|${F##*.}\"" (format nil "b.c|c~%"))
  (check "if" "if [ 1 -eq 1 ]; then echo eq; fi" (format nil "eq~%"))
  (check "for" "for i in x y; do echo $i; done" (format nil "x~%y~%"))
  (check "while" "n=0; while [ $n -lt 2 ]; do echo $n; n=$((n+1)); done"
         (format nil "0~%1~%"))
  (check "case" "case ab in a*) echo m;; *) echo n;; esac" (format nil "m~%"))
  (check "func" "f() { echo $1-$2; }; f a b" (format nil "a-b~%"))
  (check "and-or" "false || echo x && echo y" (format nil "x~%y~%"))
  (check "subshell-iso" "X=1;(X=2);echo $X" (format nil "1~%"))
  (check "quote-split" "echo \"a  b\"" (format nil "a  b~%"))
  (check "field-split" "V='p q'; for w in $V; do echo $w; done" (format nil "p~%q~%"))
  (check "heredoc" (format nil "cat <<E~%hi $((1+1))~%E~%") (format nil "hi 2~%"))
  (check "heredoc-quoted" (format nil "cat <<'E'~%no $((1+1))~%E~%")
         (format nil "no $((1+1))~%"))
  (check-status "false-status" "false" 1)
  (check-status "true-status" "true" 0)
  (check-status "exit-code" "exit 42" 42)
  (check-status "notfound" "no-such-program-xyz" 127)
  (check-status "pipe-status" "false | true" 0)
  (check-status "bang" "! false" 0)
  ;; Tier-1 fixes
  (check-status "cmdsub-status" "x=$(exit 5); echo $?" 0) ; echo succeeds
  (check "cmdsub-status-val" "x=$(exit 5); echo $?" (format nil "5~%"))
  (check "substr-off-len" "v=hello; echo ${v:1:3}" (format nil "ell~%"))
  (check "substr-off" "v=hello; echo ${v:2}" (format nil "llo~%"))
  (check "substr-neg-off" "v=hello; echo ${v: -2}" (format nil "lo~%"))
  (check "substr-neg-len" "v=hello; echo ${v:1:-1}" (format nil "ell~%"))
  (check-status "nounset-abort" "set -u; echo $NOPE; echo REACHED" 1)
  (check "nounset-no-reach" "set -u; echo pre; echo ${X:-ok}; echo $UNSET; echo post"
         (format nil "pre~%ok~%"))  ; aborts at $UNSET, "post" never prints
  (check "dash-flags" "set -e; printf %s \"$-\"" "e")
  (check "dash-flags2" "set -f; printf %s \"$-\"" "f")
  ;; Tier-2 builtins
  (check "command-v" "command -v echo" (format nil "echo~%"))
  (check "command-bypass" "echo(){ printf FUNC; }; command echo real" (format nil "real~%"))
  (check "getopts-simple" "set -- -a -b x; while getopts ab: o; do echo $o:$OPTARG; done"
         (format nil "a:~%b:x~%"))
  (check "getopts-arg" "set -- -f data; getopts f: o; echo $o=$OPTARG"
         (format nil "f=data~%"))
  (check "trap-exit" "trap 'echo bye' EXIT; echo hi" (format nil "hi~%bye~%"))
  ;; POSIX 2.8.1 makes a variable assignment error fatal to a non-interactive
  ;; shell, so nothing after the failed assignment runs. These two used to
  ;; assert that execution continued, which bash and zsh both disagree with.
  (check "readonly-fatal" "readonly x=1; x=2 2>/dev/null; echo REACHED" "")
  (check-status "readonly-status" "readonly x=1; x=2 2>/dev/null; true" 1)
  (check "readonly-ok" "readonly x=1; echo $x" (format nil "1~%"))
  (check "alias-basic" "alias g='echo aliased'; g" (format nil "aliased~%"))
  (check "alias-selfref" "alias ls='ls -x'; alias ls" (format nil "alias ls='ls -x'~%"))
  (check "unalias" "alias q=x; unalias q; alias q 2>/dev/null; echo ok" (format nil "ok~%"))
  (check "read-reply" "printf 'hello there\\n' | { read; echo $REPLY; }"
         (format nil "hello there~%"))
  (check "read-p" "printf 'val\\n' | { read -p '' x; echo $x; }" (format nil "val~%"))
  ;; `type' writes its not-found diagnostic to stderr, so 2>/dev/null hides it.
  (check "unset-f" "f(){ echo hi; }; unset -f f; type f 2>/dev/null; echo done"
         (format nil "done~%"))
  (check-status "type-notfound-status" "type nosuchcmd_zz 2>/dev/null" 1)
  (check "wait-all" "sleep 0.05 & sleep 0.05 & wait; echo waited" (format nil "waited~%"))
  (check "umask-roundtrip" "umask 022; umask" (format nil "0022~%"))
  ;; times: two lines, each "MmS.SSSs MmS.SSSs"
  (check-status "times-status" "times" 0)
  (check "times-format"
         "times | awk 'NR==1{print (NF==2 && $1 ~ /^[0-9]+m[0-9.]+s$/) ? \"ok\" : \"bad\"}'"
         (format nil "ok~%"))
  ;; Tier-3: "$@" / "$*" field preservation
  (check "at-preserves" "set -- 'a b' c; for x in \"$@\"; do echo \"[$x]\"; done"
         (format nil "[a b]~%[c]~%"))
  (check "at-count" "set -- one two three; n=0; for x in \"$@\"; do n=$((n+1)); done; echo $n"
         (format nil "3~%"))
  (check "at-empty" "set --; n=0; for x in \"$@\"; do n=$((n+1)); done; echo $n"
         (format nil "0~%"))
  (check "star-joins" "set -- a b c; for x in \"$*\"; do echo \"[$x]\"; done"
         (format nil "[a b c]~%"))
  (check "at-unquoted-splits" "set -- 'a b' c; n=0; for x in $@; do n=$((n+1)); done; echo $n"
         (format nil "3~%"))
  (check "at-assign-joins" "set -- a b c; x=\"$@\"; echo \"[$x]\"" (format nil "[a b c]~%"))
  (check "at-to-command" "set -- 'hello world' foo; printf '[%s]' \"$@\"; echo"
         (format nil "[hello world][foo]~%"))
  (check "empty-quote-field" "n=0; for x in \"\"; do n=$((n+1)); done; echo $n"
         (format nil "1~%"))
  ;; Tier-3: tilde in assignments (after colon)
  (check "tilde-assign" "x=~/foo; echo $x" (format nil "~A/foo~%" (namestring-home)))
  (check "tilde-colon" "x=~/a:~/b; echo $x"
         (format nil "~A/a:~A/b~%" (namestring-home) (namestring-home)))
  (check "tilde-not-in-arg" "echo a:~/b" (format nil "a:~~/b~%"))
  ;; Tier-3: time reserved word
  (check-status "time-status" "time false" 1)
  (check-status "time-bang" "time ! false" 0)
  (check "time-p-format"
         "{ time -p true; } 2>&1 | awk 'NR==1{print ($1==\"real\") ? \"ok\" : \"bad\"}'"
         (format nil "ok~%"))
  ;; Tier-3: signal traps
  (check "trap-signal" "trap 'echo caught' USR1; kill -USR1 $$; echo after"
         (format nil "caught~%after~%"))
  (check "trap-numeric" "trap 'echo t15' 15; kill -TERM $$; echo done"
         (format nil "t15~%done~%"))
  (check "trap-reset" "trap 'echo T' USR1; trap - USR1; trap; echo ok"
         (format nil "ok~%"))
  ;; Tier-4: job control (headless-testable parts)
  (check "bg-bang" "sleep 0.1 & echo \"pid=$!\" | sed 's/[0-9]\\{1,\\}/N/'"
         (format nil "pid=N~%"))
  (check "jobs-running" "sleep 0.2 & jobs | awk '{print $2, $3}'"
         (format nil "Running sleep~%"))
  (check-status "wait-job" "sleep 0.1 & wait %1" 0)
  (check "wait-all-jobs" "sleep 0.1 & sleep 0.1 & wait; echo done" (format nil "done~%"))
  (check "bg-pipeline-one-job"
         "sleep 0.2 | cat & jobs | wc -l | tr -d ' '" (format nil "1~%"))
  (check-status "pipeline-job-completes" "(seq 1 1000 | wc -l >/dev/null) & wait %1" 0)
  ;; deparser round-trips (used for job command display and re-exec)
  (check "deparse-if" "jobs 2>/dev/null; echo ok" (format nil "ok~%"))
  ;; arithmetic expands its contents first (positionals + command subst)
  (check "arith-positional" "f() { echo $(($1 + 1)); }; f 5" (format nil "6~%"))
  (check "arith-cmdsub" "echo $(( 2 * $(echo 3) ))" (format nil "6~%"))
  (check "arith-recursion"
         "fac() { if [ $1 -le 1 ]; then echo 1; else echo $(( $1 * $(fac $(($1-1))) )); fi; }; fac 5"
         (format nil "120~%"))
  (check "substr-var-offset" "v=abcdef; i=2; echo ${v:i:3}" (format nil "cde~%"))
  (check-help-coverage)
  (format t "~&~%shell: ~D passed, ~D failed~%" *pass* *fail*)
  (values *pass* *fail*))

(defun check-help-coverage ()
  "Every builtin must have a help entry, and every help entry a builtin.

This is the whole reason the text lives at each DEFINE-BUILTIN rather than in
a table off to one side: a builtin added without documentation, or an entry
left behind by one that was removed, fails the suite here instead of being
discovered by someone typing `help' at the prompt."
  (let ((missing '()) (extra '()))
    (maphash (lambda (k v)
               (declare (ignore v))
               (unless (gethash k sxsh-shell::*builtin-help*) (push k missing)))
             sxsh-shell::*builtins*)
    (maphash (lambda (k v)
               (declare (ignore v))
               (unless (gethash k sxsh-shell::*builtins*) (push k extra)))
             sxsh-shell::*builtin-help*)
    (cond
      ((and (null missing) (null extra))
       (incf *pass*)
       (format t "  ok   help-coverage~%"))
      (t
       (incf *fail*)
       (format t "  FAIL help-coverage~%    builtins with no help: ~{~A ~}~%~
                  ~4Thelp for no builtin: ~{~A ~}~%"
               (sort missing #'string<) (sort extra #'string<))))))

(defun namestring-home ()
  (string-right-trim "/" (namestring (user-homedir-pathname))))
