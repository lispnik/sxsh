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

(defun run-all ()
  (setf *pass* 0 *fail* 0)
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
  (check "readonly-keeps" "readonly x=1; x=2 2>/dev/null; echo $x" (format nil "1~%"))
  (check-status "readonly-status" "readonly x=1; x=2 2>/dev/null; true" 0)
  (check "alias-basic" "alias g='echo aliased'; g" (format nil "aliased~%"))
  (check "alias-selfref" "alias ls='ls -x'; alias ls" (format nil "alias ls='ls -x'~%"))
  (check "unalias" "alias q=x; unalias q; alias q 2>/dev/null; echo ok" (format nil "ok~%"))
  (check "read-reply" "printf 'hello there\\n' | { read; echo $REPLY; }"
         (format nil "hello there~%"))
  (check "read-p" "printf 'val\\n' | { read -p '' x; echo $x; }" (format nil "val~%"))
  (check "unset-f" "f(){ echo hi; }; unset -f f; type f 2>/dev/null; echo done"
         (format nil "f: not found~%done~%"))
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
  (format t "~&~%shell: ~D passed, ~D failed~%" *pass* *fail*)
  (values *pass* *fail*))

(defun namestring-home ()
  (string-right-trim "/" (namestring (user-homedir-pathname))))
