;;;; shell/exec.lisp --- execute the parsed AST.
;;;;
;;;; External commands are launched with posix_spawnp (see spawn.lisp); no
;;;; fork/exec. Builtins, functions, and compound commands run in-process.
;;;; Pipelines wire children together with pipe(2) and per-stage file actions.

(in-package #:sxsh-shell)

;;; ---------------------------------------------------------------------------
;;; PATH search
;;; ---------------------------------------------------------------------------

(defun find-in-path (sh name &key allow-slash)
  "Resolve NAME to an executable path, or NIL.

A name containing a slash is used as given rather than searched for -- but it
still has to exist and be executable. Returning it unconditionally meant a
command like `/bin/uname' on a system without that file was handed to
posix_spawn, whose failure surfaced as an unhandled error that killed the
shell. Autoconf probes exactly that path on purpose."
  (declare (ignore allow-slash))
  (if (find #\/ name)
      (and (executable-p name) name)
      (let ((path (or (nth-value 0 (get-var sh "PATH")) "/usr/bin:/bin")))
        (dolist (dir (split-string path #\:) nil)
          (let ((candidate (if (string= dir "")
                               name
                               (concatenate 'string dir "/" name))))
            (when (executable-p candidate)
              (return candidate)))))))

(defun readable-file-p (path)
  (handler-case
      (and (not (directoryp path))
           (progn (sb-posix:access path sb-posix:r-ok) t))
    (error () nil)))

(defun find-source-file (sh name)
  "Resolve the operand of `.' -- POSIX searches $PATH when it contains no
slash. A sourced file only has to be READABLE; requiring execute permission
(as the command search does) would reject most shell libraries."
  (if (find #\/ name)
      (and (readable-file-p name) name)
      (or (let ((path (or (nth-value 0 (get-var sh "PATH")) "/usr/bin:/bin")))
            (dolist (dir (split-string path #\:) nil)
              (let ((candidate (if (string= dir "")
                                   name
                                   (concatenate 'string dir "/" name))))
                (when (readable-file-p candidate)
                  (return candidate)))))
          ;; every shell also falls back to the current directory
          (and (readable-file-p name) name))))

(defun executable-p (path)
  (handler-case
      (let ((st (sb-posix:stat path)))
        (and (not (directoryp path))
             (logtest (sb-posix:stat-mode st) #o111)))
    (error () nil)))

(defun split-string (s ch)
  (loop with start = 0 with out = '()
        for i from 0 below (length s)
        when (char= (char s i) ch)
          do (push (subseq s start i) out) (setf start (1+ i))
        finally (push (subseq s start) out) (return (nreverse out))))

;;; ---------------------------------------------------------------------------
;;; Top-level entry
;;; ---------------------------------------------------------------------------

(defun run (sh ast-list)
  "Execute a list of complete-command nodes. Returns the last status."
  (handler-case
   (handler-case
      (dolist (node ast-list (shell-last-status sh))
        (exec-node sh node))
    (shell-exit (e)
      (setf (shell-last-status sh) (or (shell-exit-code e) 0))
      (shell-last-status sh)))
    ;; `return' outside a function ends the script being read, as it does in a
    ;; dot script; it must never escape as an unhandled condition.
    (func-return (e)
      (setf (shell-last-status sh) (cf-code e))
      (shell-last-status sh))))

(defun run-string (sh src)
  "Execute SRC, running each complete command as soon as it parses.

See MAP-COMPLETE-COMMANDS: a syntax error late in a script must not undo the
commands before it, and an EXIT trap set earlier has to be in place when the
error terminates the shell."
  (handler-case
      (handler-case
          (progn (map-complete-commands src (lambda (node) (exec-node sh node)))
                 (shell-last-status sh))
        (shell-exit (e)
          (setf (shell-last-status sh) (or (shell-exit-code e) 0))
          (shell-last-status sh)))
    (func-return (e)
      (setf (shell-last-status sh) (cf-code e))
      (shell-last-status sh))))

(defun run-trap (sh condition-name)
  "Run the trap action registered for CONDITION-NAME (already normalized), if
any. Errors in the trap are contained.

The status a trap leaves behind is its own business: `$?' after the handler
returns is whatever it was before the signal arrived. A trap ending in
`( exit 42 )' otherwise made the next `echo $?' report 42, as though the
interrupted command had failed."
  (multiple-value-bind (action found) (gethash condition-name (shell-traps sh))
    (when (and found (plusp (length action)))
      (let ((entry-status (shell-last-status sh))
            (exited nil))
        (handler-case (run sh (parse-string action))
          (shell-exit (e)
            (setf exited t)
            (setf (shell-last-status sh) (or (shell-exit-code e) 0)))
          (error () nil))
        (unless exited (setf (shell-last-status sh) entry-status))))))

(defun run-exit-traps (sh)
  "Execute the EXIT trap, if set. Called once when the shell terminates.

The shell's exit status is the one already in effect when termination began;
commands run by the trap must not change it. Only an explicit `exit N' inside
the trap overrides it. Without this, `trap \"echo done\" EXIT; exit 3' exited 0,
because the trap's own `echo' overwrote the 3."
  (multiple-value-bind (action found) (gethash "EXIT" (shell-traps sh))
    (when (and found (plusp (length action)))
      (let* ((saved (shell-last-status sh))
             (*trap-entry-status* saved)
             (overridden nil))
        ;; Dispatch the nodes directly rather than through RUN: RUN catches
        ;; SHELL-EXIT itself and turns it into a status, which would hide an
        ;; explicit `exit N' in the trap from the handler below.
        (handler-case
            (dolist (node (parse-string action)) (exec-node sh node))
          (shell-exit (e)
            (setf (shell-last-status sh) (or (shell-exit-code e) 0)
                  overridden t))
          (error () nil))
        (unless overridden
          (setf (shell-last-status sh) saved))))))

(defun exec-node (sh node)
  "Dispatch on AST node type. Returns exit status; also sets shell-last-status."
  ;; `set -n' (noexec): parse but do not execute, so a script can be syntax
  ;; checked. The test belongs here rather than in RUN: `set -n; echo hi' is a
  ;; single :list node, so a check over RUN's node list would never see the
  ;; flag change. `set -n' itself still runs -- the flag is not set yet when
  ;; its own node is dispatched. POSIX makes this a no-op for interactive
  ;; shells, or there would be no way to turn it back off.
  (when (and (opt sh :noexec) (not (shell-interactive sh)))
    (return-from exec-node 0))
  ;; $LINENO is the line of the command currently executing. Nodes the parser
  ;; did not stamp keep line 0, so only update when we have a real line.
  (let ((line (sxsh::node-line node)))
    (when (plusp line)
      (setf (gethash "LINENO" (shell-vars sh))
            (cons (princ-to-string line) nil))))
  (maybe-debug-trap sh node)
  (let ((status
          (handler-case
              (ecase (ast-type node)
                (:list     (exec-list sh node))
                (:and-or   (exec-and-or sh node))
                (:pipeline (exec-pipeline sh node))
                (:simple   (exec-simple sh node))
                (:subshell (exec-subshell sh node))
                (:brace    (exec-brace sh node))
                (:if       (exec-if sh node))
                (:for      (exec-for sh node))
                (:while    (exec-while sh node nil))
                (:until    (exec-while sh node t))
                (:case     (exec-case sh node))
                (:func     (exec-func-def sh node))
                (:arith    (exec-arith-command sh node))
                (:cond     (exec-cond-expr sh node))
                (:select   (exec-select sh node))
                (:coproc   (exec-coproc sh node))
                (:arith-for (exec-arith-for sh node)))
            (shell-unset-var (e)
              (format *error-output* "sxsh: ~A~%" e)
              ;; set -u in a non-interactive shell is fatal
              (unless (shell-interactive sh)
                (signal 'shell-exit :code 1))
              1)
            (readonly-violation (e)
              (format *error-output* "sxsh: ~A~%" e)
              ;; POSIX 2.8.1: a variable assignment error is fatal to a
              ;; non-interactive shell. Reporting and carrying on let a script
              ;; run past a failed assignment with the old value still in
              ;; place, which is exactly what readonly exists to prevent.
              ;; *ASSIGNMENT-ERROR-FATAL* marks the constructs that contain it.
              (unless (or (shell-interactive sh)
                          (not *assignment-error-fatal*))
                (signal 'shell-exit :code 1))
              1))))
    (setf (shell-last-status sh) status)
    ;; ERR runs before errexit, so a trap that reports the failure still gets
    ;; to run when `set -e' is about to end the shell.
    (maybe-err-trap sh status node)
    (maybe-errexit sh status node)
    status))

;;; --- set -e scoping --------------------------------------------------------
;;;
;;; POSIX does not apply -e to every failing command: it is ignored while
;;; evaluating a *condition*, because failure there is the meaningful answer
;;; rather than an error. The exempt contexts (POSIX 2.14, `set -e`) are
;;;
;;;   * the compound list following while, until, if, or elif
;;;   * any command of an AND-OR list other than the last
;;;   * any command of a pipeline other than the last
;;;   * a pipeline beginning with the ! reserved word
;;;
;;; Everything else -- including a failing subshell or brace group -- does
;;; trigger the exit. We model the exemption with a dynamic flag bound around
;;; the exempt sub-execution; because EXEC-NODE runs MAYBE-ERREXIT after the
;;; node's own dispatch returns, binding it around a child's EXEC-NODE call
;;; correctly covers that child's entire subtree.

(defvar *errexit-suppressed* nil
  "True while executing a context POSIX exempts from `set -e`.")

(defmacro without-errexit (&body body)
  "Execute BODY with `set -e` suppressed (a condition context)."
  `(let ((*errexit-suppressed* t)) ,@body))

(defvar *in-trap-action* nil
  "True while a trap action runs, so ERR/DEBUG cannot recurse into themselves.")

(defun maybe-err-trap (sh status node)
  "Fire the bash ERR trap. Same exemptions as `set -e': a condition context is
not a failure, and container nodes have already given their child a turn."
  (when (and (/= status 0)
             (not *in-trap-action*)
             (not *errexit-suppressed*)
             (not (and (eq (ast-type node) :pipeline) (pipeline-bang node)))
             (not (member (ast-type node) '(:list :and-or)))
             (gethash "ERR" (shell-traps sh)))
    (let ((*in-trap-action* t))
      (run-trap sh "ERR"))))

(defun maybe-debug-trap (sh node)
  "Fire the bash DEBUG trap before a command runs."
  (when (and (not *in-trap-action*)
             (member (ast-type node) '(:simple :cond :arith))
             (gethash "DEBUG" (shell-traps sh)))
    (let ((*in-trap-action* t))
      (run-trap sh "DEBUG"))))

(defun maybe-errexit (sh status node)
  (when (and (opt sh :errexit)
             (/= status 0)
             (not *errexit-suppressed*)
             ;; `! cmd` is exempt as a whole: its non-zero result is the
             ;; negation's answer, not a failure.
             (not (and (eq (ast-type node) :pipeline) (pipeline-bang node)))
             ;; :list and :and-or are containers -- whichever child produced
             ;; the status already had its own chance to trigger. Letting
             ;; :and-or trigger would break `false && true`, where the list
             ;; short-circuits to 1 but no *last* command ever failed.
             (not (member (ast-type node) '(:list :and-or))))
    (signal 'shell-exit :code status)))

;;; ---------------------------------------------------------------------------
;;; Lists, and-or, pipelines
;;; ---------------------------------------------------------------------------

(defun run-pending-traps (sh)
  "Run trap actions for any signals received since the last check. Called at
safe points between commands. Signals are handled in the order received."
  (when (shell-pending-signals sh)
    (let ((sigs (nreverse (shell-pending-signals sh))))
      (setf (shell-pending-signals sh) '())
      (dolist (sig sigs)
        (multiple-value-bind (action found) (gethash sig (shell-traps sh))
          (when (and found (plusp (length action)))
            ;; Dispatch the nodes directly rather than through RUN: RUN catches
            ;; SHELL-EXIT and turns it into a status, so an `exit' inside a
            ;; trap action never reached the handler below and the shell
            ;; carried on -- `trap "exit 9" TERM' did nothing on TERM.
            (let ((*trap-entry-status* (shell-last-status sh)))
             (handler-case
                (progn
                  (dolist (node (parse-string action)) (exec-node sh node))
                  ;; What the handler left in `$?' is its own business: after
                  ;; it returns, `$?' is whatever it was when the signal
                  ;; arrived. A trap ending in `( exit 42 )' otherwise made
                  ;; the next `echo $?' report 42, as though the command the
                  ;; signal interrupted had failed.
                  (setf (shell-last-status sh) *trap-entry-status*))
              (shell-exit (e)
                (setf (shell-last-status sh) (or (shell-exit-code e) 0))
                (signal 'shell-exit :code (shell-last-status sh)))
              (error () nil)))))))))

(defun exec-list (sh node)
  (let ((status 0))
    (dolist (entry (complete-command-entries node) status)
      (run-pending-traps sh)
      (destructuring-bind (child . sep) entry
        (if (eq sep :async)
            (setf status (exec-async sh child))
            (setf status (exec-node sh child)))))))

(defun exec-and-or (sh node)
  ;; Only the LAST command of an and-or list can trigger -e. Lists are
  ;; left-associative, so the whole left subtree is "other than the last".
  (let ((left (without-errexit (exec-node sh (and-or-left node)))))
    (ecase (and-or-op node)
      (:&& (if (zerop left) (exec-node sh (and-or-right node)) left))
      (:\|\| (if (zerop left) left (exec-node sh (and-or-right node)))))))

(defun exec-pipeline (sh node)
  (if (pipeline-timed node)
      (run-timed sh node)
      (let ((status (exec-pipeline-raw sh node)))
        (if (pipeline-bang node) (if (zerop status) 1 0) status))))

(defun run-timed (sh node)
  "Execute a `time`-prefixed pipeline, printing elapsed wall-clock plus the
user/system CPU time consumed, to stderr, then return the pipeline's status.
CPU time sums the shell's own delta (for builtins/functions run in-process)
and its children's delta (for external commands). POSIX -p form uses the
fixed 'real/user/sys' layout."
  (multiple-value-bind (cu0 cs0) (rusage-times sb-unix:rusage_children)
    (multiple-value-bind (su0 ss0) (rusage-times sb-unix:rusage_self)
      (let ((wall0 (get-internal-real-time)))
        (let ((status (let ((bang (pipeline-bang node)))
                        (let ((st (exec-pipeline-raw sh node)))
                          (if bang (if (zerop st) 1 0) st)))))
          (let ((wall (/ (- (get-internal-real-time) wall0)
                         internal-time-units-per-second)))
            (multiple-value-bind (cu1 cs1) (rusage-times sb-unix:rusage_children)
              (multiple-value-bind (su1 ss1) (rusage-times sb-unix:rusage_self)
                (let ((real wall)
                      (user (+ (- cu1 cu0) (- su1 su0)))
                      (sys  (+ (- cs1 cs0) (- ss1 ss0))))
                  (if (eq (pipeline-timed node) :time-p)
                      (format *error-output* "real ~,2F~%user ~,2F~%sys ~,2F~%"
                              (float real) (float user) (float sys))
                      (format *error-output* "~%real~A~A~%user~A~A~%sys~A~A~%"
                              #\Tab (format-cpu-time real)
                              #\Tab (format-cpu-time user)
                              #\Tab (format-cpu-time sys)))
                  (finish-output *error-output*)))))
          status)))))

(defun exec-pipeline-raw (sh node)
  ;; Individual stages never trigger -e ("any command of a pipeline other than
  ;; the last"); the pipeline as a whole reports the last stage's status and
  ;; triggers from its own node. Without this, `false | true` exits even though
  ;; the pipeline succeeded.
  (without-errexit
    (let ((cmds (pipeline-commands node)))
      (if (= 1 (length cmds))
          (exec-node sh (first cmds))
          (exec-multi-pipeline sh cmds)))))

(defun exec-multi-pipeline (sh cmds)
  "Run a multi-stage pipeline. Each stage is spawned (external) or run in a
child for builtins/compounds via a forked spawn of ourselves is not available,
so builtins in non-final pipeline stages run in-process writing to the pipe."
  (let* ((n (length cmds))
         ;; All pipes up front: pipe i joins stage i to stage i+1.
         (pipes (loop repeat (1- n) collect (multiple-value-list (sb-posix:pipe))))
         (all-fds (loop for p in pipes append (list (first p) (second p))))
         (stages (make-array n :initial-element nil)))
    ;; Classify every stage EXACTLY ONCE, up front. Doing it per pass would
    ;; expand each command's words twice, and expansion is not idempotent.
    (let ((classes (map 'vector
                        (lambda (cmd)
                          (multiple-value-bind (ext words)
                              (external-simple-command-p sh cmd)
                            (cons ext words)))
                        cmds)))
    (flet ((stdin-of (i) (if (zerop i) 0 (first (nth (1- i) pipes))))
           (stdout-of (i) (if (= i (1- n)) 1 (second (nth i pipes)))))
      (unwind-protect
           (progn
             ;; PASS 1: start every stage that becomes a child process. This
             ;; must happen BEFORE any in-process stage runs. An in-process
             ;; stage runs to completion inline, so if its reader had not been
             ;; started yet it would block once the pipe filled --
             ;; `{ seq 1 50000; } | cat' deadlocked exactly there, and so did
             ;; any `while ...; do echo; done | consumer' over 64KB. That is
             ;; what hung git's t3600-rm.
             (loop for cmd in cmds for i from 0 do
               (when (car (aref classes i))
                 (multiple-value-bind (pid st)
                     (spawn-stage sh cmd (stdin-of i) (stdout-of i) all-fds
                                  :classified (aref classes i))
                   (setf (aref stages i) (if pid (cons :pid pid) (cons :status st))))))
             ;; Drop the parent's copy of every fd that only a spawned child
             ;; needs, BEFORE running any in-process stage. Holding a pipe's
             ;; write end open means the reader never sees EOF -- and if that
             ;; reader is an in-process stage, the shell waits on itself. That
             ;; is `find | awk | while read ...; done', which hung forever.
             ;; Keyed on whether a child was ACTUALLY started, not on how the
             ;; stage was classified. A command that is external but cannot be
             ;; found never spawns, so nothing will ever read its input pipe;
             ;; closing that read end here made the producer die of EPIPE and
             ;; the pipeline report 141 instead of the 127 the failed lookup
             ;; owes it. `echo hi | nosuchcmd' is the case.
             (loop for i from 0 below (1- n) do
               (when (eq (car (aref stages i)) :pid)
                 (ignore-errors (sb-posix:close (second (nth i pipes)))))
               (when (eq (car (aref stages (1+ i))) :pid)
                 (ignore-errors (sb-posix:close (first (nth i pipes))))))
             ;; PASS 2: the in-process stages, in pipeline order. Each one's
             ;; reader is already running. Close our copy of a stage's write
             ;; end as soon as it finishes, or the downstream reader never
             ;; sees EOF.
             (loop for cmd in cmds for i from 0 do
               (unless (aref stages i)
                 (multiple-value-bind (pid st)
                     (spawn-stage sh cmd (stdin-of i) (stdout-of i) all-fds
                                  :classified (aref classes i)
                                  :lastp (= i (1- n)))
                   (setf (aref stages i) (if pid (cons :pid pid) (cons :status st))))
                 ;; Same reasoning for an in-process stage, once it is done:
                 ;; release its output pipe so the next reader sees EOF, and
                 ;; its input pipe since nothing else will read it.
                 (when (< i (1- n))
                   (ignore-errors (sb-posix:close (second (nth i pipes)))))
                 (when (plusp i)
                   (ignore-errors (sb-posix:close (first (nth (1- i) pipes))))))))
        ;; Every remaining parent-side pipe fd goes now: a reader still holding
        ;; a write end would wait for an EOF that cannot arrive.
        (dolist (fd all-fds) (ignore-errors (sb-posix:close fd))))))
    (setf stages (coerce stages 'list))
    ;; Reap every spawned child so none are left as zombies; remember the
    ;; status of the FINAL stage specifically -- that is the pipeline's status.
    (let ((final 0) (last-index (1- n)) (rightmost-failure nil) (all '()))
      (loop for stage in stages for i from 0 do
        (let ((st (ecase (car stage)
                    (:pid (wait-for (cdr stage)))
                    (:status (cdr stage)))))
          (push st all)
          (when (/= st 0) (setf rightmost-failure st))
          (when (= i last-index) (setf final st))))
      ;; bash $PIPESTATUS: one element per stage, in order.
      (set-var sh "PIPESTATUS"
               (array-from-list (mapcar #'princ-to-string (nreverse all))))
      ;; bash `set -o pipefail': the pipeline's status is that of the last
      ;; stage to fail, not of the last stage. Without it a failing producer is
      ;; invisible -- `false | cat' succeeds -- which is why build scripts
      ;; reach for it.
      (if (and (opt sh :pipefail) rightmost-failure)
          rightmost-failure
          final))))

(defconstant +max-async-source+ 100000
  "Ceiling on the generated `-c` source. argv+env is bounded (ARG_MAX), and a
shell with a very large variable table could otherwise overflow it; past this
we run synchronously instead of failing to spawn.")

(defparameter +parent-only-builtins+
  '("jobs" "wait" "fg" "bg" "disown")
  "Builtins that must run in the shell that owns the child processes.

Forking them breaks their whole purpose: a forked `jobs' calls waitpid on
processes that are its SIBLINGS, gets ECHILD, and reports every running job as
Done. `jobs | awk ...' did exactly that. They are safe to run inline in a
pipeline anyway -- their output is a job table, never enough to fill a pipe.")

(defun parent-only-stage-p (cmd words)
  "True if CMD is a simple command invoking a builtin that must not be forked."
  (and (eq (ast-type cmd) :simple)
       (let ((name (or (first words)
                       ;; No expansion available: fall back to the raw first
                       ;; word, which covers the literal `jobs | ...' case.
                       (let ((w (first (simple-command-words cmd))))
                         (and w (word-text w))))))
         (and name (member name +parent-only-builtins+ :test #'string=)))))

(defun fork-stage (sh cmd stdin stdout close-in-child)
  "Run an in-process pipeline stage in a forked child. Returns a pid, or NIL if
the fork failed and the caller should fall back to running it inline.

WHY FORK, given this shell otherwise has none. A pipeline stage that runs
inline runs to completion before the next stage starts, so two adjacent
in-process stages deadlock as soon as the first writes more than the pipe
holds: `( ... ) | :' -- a subshell into a builtin -- never gets to the reader.
Starting children first fixes it only when the READER is a child.

The alternative, re-executing this binary with the deparsed stage, was tried
and rejected: the child loses too much context and git's t1006-cat-file fell
from 255/256 to 179/256. fork preserves the environment exactly, which is the
whole point, and the image is single-threaded so it is safe. No exec follows,
so this does not reintroduce the fork+exec that posix_spawn exists to avoid --
external commands are still spawned, never forked.

The flush before forking is not optional: buffered output present in the
parent would otherwise be written twice, once by each process.

Thread safety is left to SB-POSIX:FORK, which does it better than a check here
could. fork clones only the calling thread, so a mutex held by any other stays
locked forever and the child would deadlock on its first allocation -- and this
child runs arbitrary Lisp rather than exec'ing immediately. SBCL stops its
finalizer thread around the fork and restarts it afterwards, prunes dead thread
structs, and then tests (avl-count *all-threads*) -- lower level than
LIST-ALL-THREADS, which by SBCL's own comment does not expose newborn threads.
All of that happens under *make-thread-lock*, so it is atomic with the fork;
a check of our own would be both weaker and racy. It signals rather than
forking unsafely, which the IGNORE-ERRORS below turns into NIL, and the caller
then runs the stage inline -- the older behaviour, not a wedged child.

vfork is NOT an alternative: its child shares the parent's address space and
may only call exec or _exit, so running a stage in one would corrupt the
parent. That is exactly why posix_spawn may use vfork internally and we
cannot -- it execs immediately, and this never execs."
  (finish-output *standard-output*)
  (finish-output *error-output*)
  (let ((pid (ignore-errors (sb-posix:fork))))
    (cond
      ((null pid) nil)
      ((zerop pid)
       ;; Child. Wire up the pipe, run the stage, leave without unwinding --
       ;; the parent's cleanups and EXIT traps are not ours to run.
       (let ((status 1))
         (ignore-errors
          (unless (= stdin 0) (sb-posix:dup2 stdin 0))
          (unless (= stdout 1) (sb-posix:dup2 stdout 1))
          (dolist (fd close-in-child)
            (unless (or (= fd stdin) (= fd stdout))
              (ignore-errors (sb-posix:close fd))))
          (setf status
                (handler-case (exec-node sh cmd)
                  (shell-exit (e) (or (shell-exit-code e) 0))
                  (func-return (e) (or (cf-code e) 0))
                  (loop-break () 0)
                  (loop-continue () 0)
                  (stream-error () 141)
                  (error () 1)))
          (finish-output *standard-output*))
         (sb-posix:exit (if (integerp status) status 0))))
      (t pid))))

(defun spawn-stage (sh cmd stdin stdout close-in-child &key classified lastp)
  "Run one pipeline stage. Returns (values pid nil) for an external command, or
(values nil status) for a builtin/compound run in-process.
STDIN/STDOUT are fds to attach. CLOSE-IN-CHILD lists pipe fds to close.

CLASSIFIED, when given, is the (EXTERNALP . WORDS) pair already computed for
CMD. It must be reused rather than recomputed: EXTERNAL-SIMPLE-COMMAND-P
expands the command's words, and expansion is not idempotent -- a command
substitution among them runs again. Classifying a stage twice made
`/bin/echo hi$(echo x >>f) | cat' append to f twice."
  (multiple-value-bind (externalp words)
      (if classified
          (values (car classified) (cdr classified))
          (external-simple-command-p sh cmd))
   (if externalp
      ;; A failed PATH lookup throws 'not-found; catch it here or the throw
      ;; escapes the pipeline with no catcher at all ("attempt to THROW to a
      ;; tag that does not exist"). A stage that cannot start is just a stage
      ;; whose status is 127 -- the rest of the pipeline still runs.
      (let ((pid (catch 'not-found
                   (spawn-external sh cmd
                                   :words words
                                   :extra-actions
                                   (append
                                    (unless (= stdin 0) (list (fa-dup2 stdin 0)))
                                    (unless (= stdout 1) (list (fa-dup2 stdout 1)))
                                    (mapcar #'fa-close
                                            (remove-duplicates
                                             (remove-if (lambda (f)
                                                          (or (= f stdin) (= f stdout)))
                                                        close-in-child))))))))
        (cond
          ((eq pid :redirect-error) (values nil 1))
          ((and (consp pid) (eq (first pid) :command-error))
           (values nil (second pid)))     ; lookup failed: no child to wait for
          (t (values pid nil))))
      ;; A non-final builtin/compound must run concurrently with the stage
      ;; reading from it, or the two deadlock once the pipe fills.
      (let ((pid (unless (or lastp (parent-only-stage-p cmd words))
                   (fork-stage sh cmd stdin stdout close-in-child))))
       (if pid
        (values pid nil)
      ;; builtin / compound: run in-process with fds redirected temporarily
      (let ((saved-in (sb-posix:dup 0)) (saved-out (sb-posix:dup 1)))
        (unwind-protect
             (progn
               (unless (= stdin 0) (sb-posix:dup2 stdin 0))
               (unless (= stdout 1) (sb-posix:dup2 stdout 1))
               ;; POSIX: each pipeline stage runs in a subshell environment, so
               ;; control flow ends the stage and becomes its status -- it does
               ;; not reach the enclosing shell. `echo a | { exit 4; }' sets the
               ;; pipeline's status to 4; it does not exit the shell. Only
               ;; multi-stage pipelines reach here (EXEC-PIPELINE-RAW runs a
               ;; lone command directly), so a bare `return' is unaffected.
               (values nil (handler-case (exec-node sh cmd)
                             (shell-exit (e) (or (shell-exit-code e) 0))
                             (func-return (e) (or (cf-code e) 0))
                             (loop-break () 0)
                             (loop-continue () 0))))
          (sb-posix:dup2 saved-in 0) (sb-posix:close saved-in)
          (sb-posix:dup2 saved-out 1) (sb-posix:close saved-out))))))))

;;; ---------------------------------------------------------------------------
;;; Simple commands
;;; ---------------------------------------------------------------------------

(defun external-simple-command-p (sh cmd)
  "True if CMD is a simple command whose command word is neither a builtin nor
a function (so it must be spawned).

Returns the expanded argv as a SECOND VALUE, and callers must reuse it rather
than expanding again. Expansion is not idempotent -- a command substitution in
the words runs a command -- so `/bin/echo $(date >>log)' executed the
substitution twice when the test and the spawn each expanded independently."
  (if (eq (ast-type cmd) :simple)
      (let ((words (expand-command-words sh cmd)))
        (values (and words
                     (not (builtin-p (first words)))
                     (not (gethash (first words) (shell-functions sh)))
                     t)
                words))
      (values nil nil)))

(defparameter +declaration-utilities+
  '("export" "readonly" "declare" "typeset" "local")
  "Utilities whose `name=value' operands are expanded as assignments.

POSIX 2.9.1: for these, a word of that form is treated the way a real
assignment would be, so the tilde after `=' (and after each unquoted `:')
expands. Without this `export PATH=~/bin' exported a literal ~. The same rule
suppresses field splitting and globbing, which is why `typeset foo=*' assigns
a literal `*' rather than matching files named `foo=...'.")

(defun alias-resolved-name (sh name)
  "Follow the alias chain from NAME and return the command word it ends at.

The declaration-utility test has to run BEFORE the operands are expanded, but
aliases are substituted afterwards, in APPLY-ALIAS. Testing the unresolved word
meant `alias e=export; e x=$v' split the value that a plain `export x=$v'
keeps whole."
  (let ((seen '()) (cur name))
    (loop
      (multiple-value-bind (body found) (gethash cur (shell-aliases sh))
        (if (and found (not (member cur seen :test #'string=)))
            (progn
              (push cur seen)
              (let ((parts (remove "" (split-string-ws body) :test #'string=)))
                (if parts (setf cur (first parts)) (return cur))))
            (return cur))))))

(defun assignment-operand-p (raw)
  "True if RAW has the shape name=value."
  (let ((eq (position #\= raw)))
    (and eq (plusp eq)
         (let ((name (subseq raw 0 eq)))
           (and (or (alpha-char-p (char name 0)) (char= (char name 0) #\_))
                (every (lambda (c) (or (alphanumericp c) (char= c #\_)))
                       name))))))

(defun array-literal-operand-p (raw)
  "True if RAW is a declaration-utility operand of the form `name=(...)'."
  (let ((eq (position #\= raw)))
    (and eq (array-literal-p (subseq raw (1+ eq))))))

(defun process-substitution-p (raw)
  "True if RAW is a `<(cmd)' or `>(cmd)' word."
  (and (>= (length raw) 3)
       (member (char raw 0) '(#\< #\>))
       (char= (char raw 1) #\()
       (char= (char raw (1- (length raw))) #\))))

(defun open-process-substitution (sh raw)
  "Start `<(cmd)' / `>(cmd)' and return the /dev/fd path standing for it.

A pipe is created, the command is run in a child with one end wired to its
stdout (for `<') or stdin (for `>'), and the word becomes /dev/fd/N naming the
end WE keep. The descriptor is deliberately left inheritable: the command
being built is spawned afterwards and has to be able to open that path.

Returns (values path fd) so the caller can close the descriptor once the
consuming command has finished."
  (let* ((readp (char= (char raw 0) #\<))
         (src (subseq raw 2 (1- (length raw))))
         (self (self-exec-path)))
    (unless self (return-from open-process-substitution (values nil nil)))
    (multiple-value-bind (r w) (sb-posix:pipe)
      (handler-case
          (let* ((child-fd (if readp w r))   ; the end the child writes/reads
                 (ours (if readp r w))
                 (pid (spawn self (list "sxsh" "-c"
                                        (concatenate 'string (async-prelude sh) src))
                             :env (exported-environ sh)
                             :file-actions
                             (list (fa-dup2 child-fd (if readp 1 0))
                                   (fa-close ours)))))
            (declare (ignore pid))
            (sb-posix:close child-fd)
            (values (format nil "/dev/fd/~D" ours) ours))
        (error ()
          (ignore-errors (sb-posix:close r))
          (ignore-errors (sb-posix:close w))
          (values nil nil))))))

(defun expand-command-words (sh cmd)
  "Expand the word list of a simple command into argv (no assignments).
Applies alias substitution to the command name (first word)."
  (let ((argv '()) (declaration nil) (first t))
    (dolist (w (simple-command-words cmd))
     ;; Brace expansion comes first and can turn one word into several, so it
     ;; wraps the rest rather than being a step inside EXPAND-WORD-TO-FIELDS.
     (dolist (raw (brace-expand (word-text w)))
      (if (process-substitution-p raw)
          ;; `<(cmd)' becomes a /dev/fd path naming a live pipe; it is not a
          ;; word to expand, and the descriptor stays open until the command
          ;; consuming it has finished.
          (multiple-value-bind (path fd) (open-process-substitution sh raw)
            (when path
              (push fd *procsub-fds*)
              (push path argv)
              (setf first nil)))
      (let* ((as-assignment (and declaration (assignment-operand-p raw)))
             (fields
               (cond
                 ;; `declare a=(x "y z")' has to reach ASSIGN-ONE with its
                 ;; quoting intact: the array literal is expanded word by word
                 ;; there, and quote removal here would have already turned
                 ;; "y z" into two elements. A real assignment keeps the raw
                 ;; text for the same reason; this is the declaration-utility
                 ;; spelling of it.
                 ((and as-assignment (array-literal-operand-p raw)) (list raw))
                 (t (expand-word-to-fields
                     sh raw
                     ;; Operands of export/readonly are expanded exactly like
                     ;; a real assignment: tilde after = and :, and NO field
                     ;; splitting or pathname expansion -- `export x=$v' with
                     ;; v="a b" must export the whole value, not just "a".
                     :assignment as-assignment
                     :split (not as-assignment)
                     :glob (not as-assignment))))))
        (when first
          (setf declaration (and (member (alias-resolved-name sh (first fields))
                                         +declaration-utilities+
                                         :test #'string=)
                                 t)
                first nil))
        (dolist (f fields) (push f argv))))))
    (nreverse argv)))

(defun apply-alias (sh argv)
  "Substitute aliases at the front of ARGV.

POSIX 2.3.1: normally only the command word is a candidate, but if the value
an alias expands to ends in a <blank>, the NEXT word is checked too -- the
rule that makes `alias sudo=\"sudo \"' expand an aliased command after sudo.
Without it, `alias g=\"echo \"; alias h=world; g h' printed h instead of world.

Self-reference is guarded per position: an alias may not expand itself, but
the same name may legitimately reappear once we have moved on to a later word."
  (if (null argv)
      argv
      (let ((out '())          ; already-settled words, reversed
            (rest argv)
            (check-next t))
        (loop
          (when (or (null rest) (not check-next))
            (return (nconc (nreverse out) rest)))
          (let ((seen '()) (expanded nil))
            ;; expand this position until it is no longer an alias
            (loop
              (let ((head (first rest)))
                (multiple-value-bind (body found) (gethash head (shell-aliases sh))
                  (if (and found (not (member head seen :test #'string=)))
                      (progn
                        (push head seen)
                        (setf expanded t)
                        ;; a trailing blank licenses checking the next word
                        (setf check-next
                              (and (plusp (length body))
                                   (member (char body (1- (length body)))
                                           '(#\Space #\Tab))
                                   t))
                        (let ((parts (remove "" (split-string-ws body)
                                             :test #'string=)))
                          (setf rest (append parts (cdr rest)))))
                      (return)))))
            (unless expanded (setf check-next nil))
            ;; this position is settled; move past it
            (when (and check-next rest)
              (push (pop rest) out)))))))

(defun split-string-ws (s)
  (let ((out '()) (start 0) (n (length s)))
    (dotimes (i n)
      (when (member (char s i) '(#\Space #\Tab))
        (push (subseq s start i) out) (setf start (1+ i))))
    (push (subseq s start) out)
    (nreverse out)))

(defun set-single-pipestatus (sh status)
  "bash keeps $PIPESTATUS meaningful for a lone command too: one element."
  (set-var sh "PIPESTATUS" (array-from-list (list (princ-to-string status))))
  status)

(defun exec-simple (sh node)
  ;; PIPESTATUS is set from the result, AFTER the words have been expanded --
  ;; so `echo ${PIPESTATUS[0]}' still reports the previous command.
  (let ((*procsub-fds* '()))
    (unwind-protect
         (set-single-pipestatus sh (exec-simple-1 sh node))
      (dolist (fd *procsub-fds*) (ignore-errors (sb-posix:close fd))))))

(defun exec-simple-1 (sh node)
  ;; Reset the command-substitution status tracker; any $(...) expanded while
  ;; building this command's words or assignments will set it.
  (setf (shell-last-cmdsub-status sh) nil)
  (let ((words (expand-command-words sh node)))
    ;; set -x : trace the expanded command to stderr
    (when (and (opt sh :xtrace) (or words (simple-command-assignments node)))
      (xtrace sh node words))
    (cond
      ;; no command word: assignments affect the shell; redirects still run.
      ;; POSIX: such a command's exit status is that of the last command
      ;; substitution it performed, or 0 if there were none.
      ((null words)
       (apply-assignments sh (simple-command-assignments node) :to-shell t)
       (when (simple-command-redirects node)
         (multiple-value-bind (saved temps)
             (apply-redirects-in-process sh (simple-command-redirects node))
           (restore-redirects saved)
           (dolist (p temps) (ignore-errors (delete-file p)))))
       (or (shell-last-cmdsub-status sh) 0))
      ;; `exec` : with args, replace the shell image; with none, apply this
      ;; command's redirections permanently to the shell.
      ((string= (first words) "exec")
       (exec-exec-builtin sh node (rest words)))
      ;; `command NAME ...` : run NAME as a builtin/external, bypassing any
      ;; shell function of the same name. -v/-V are handled inside the builtin.
      ;; -p means "run it, using the standard PATH"; only -v and -V merely
      ;; report and stay in the builtin. Excluding -p here sent `command -p cmd'
      ;; into the builtin, which throws to a tag only this path establishes --
      ;; crashing with "attempt to THROW to a tag that does not exist".
      ((and (string= (first words) "command")
            (rest words)
            (not (member (second words) '("-v" "-V") :test #'string=)))
       (exec-command-bypass sh node (rest words)))
      ;; `builtin NAME ...' : NAME must be a builtin, and it runs HERE rather
      ;; than through a nested dispatch, so `builtin break' really breaks the
      ;; enclosing loop and `builtin exit 5' really exits. The prefixes nest
      ;; and compose with `command', so `command builtin exit 5' and
      ;; `builtin command return 99' both work.
      ((and (string= (first words) "builtin") (rest words))
       (exec-builtin-prefix sh node (rest words)))
      ;; function call
      ((gethash (first words) (shell-functions sh))
       (call-function sh (gethash (first words) (shell-functions sh)) words node))
      ;; builtin
      ((builtin-p (first words))
       (run-builtin sh (first words) (rest words) node))
      ;; external via posix_spawn
      (t
       (let ((thrown
              (catch 'not-found
         (if (shell-job-control sh)
             ;; job control: put the child in its own group, give it the
             ;; terminal, wait, then reclaim -- so Ctrl-Z/Ctrl-C hit the child.
             (let ((pid (spawn-external sh node :words words
                                               :setpgroup t :pgroup 0)))
               (return-from exec-simple-1
                 (wait-foreground sh pid (ignore-errors (deparse node)))))
             (let ((pid (spawn-external sh node :words words)))
               (return-from exec-simple-1 (wait-for pid)))))))
         (cond ((eq thrown :redirect-error) 1)
               ((and (consp thrown) (eq (first thrown) :command-error))
                (second thrown))
               (t 127)))))))

(defun wait-foreground (sh pid command)
  "Wait for a foreground child PID that is a group leader, handling terminal
ownership and job-control stop. If the child stops (Ctrl-Z), register it as a
stopped job and return; otherwise return its exit status."
  (let ((pgid pid))
    (tty-set-pgrp pgid)                  ; give terminal to the child's group
    (unwind-protect
         (multiple-value-bind (wpid status)
             (handler-case (sb-posix:waitpid pid +wuntraced+)
               (error () (values pid 0)))
           (declare (ignore wpid))
           (cond
             ((wait-stopped-p status)
              ;; child stopped: create a stopped job the user can fg/bg
              (let ((job (add-job sh pgid (list pid) (or command "?")
                                  :state :stopped)))
                (when (shell-interactive sh)
                  (format *error-output* "~%[~D]+ Stopped~28T~A~%"
                          (job-id job) (job-command job))))
              (+ 128 sb-unix:sigtstp))    ; SIGTSTP is 20 on Linux, 18 on macOS
             (t (decode-wait-status status))))
      ;; reclaim the terminal for the shell
      (fg-reclaim-terminal sh))))

(defparameter +standard-path+ "/usr/bin:/bin:/usr/sbin:/sbin"
  "PATH used by `command -p': a default guaranteed to find the standard
utilities, regardless of what the caller has done to $PATH.")

(defun exec-builtin-prefix (sh node cmd-words)
  "Run `builtin NAME args'. NAME must be a shell builtin; a function or an
external of the same name is not consulted.

Further `builtin' and `command' prefixes are stripped first: bash accepts any
mix of them, and the innermost name is what runs."
  (loop while (and cmd-words
                   (member (first cmd-words) '("builtin" "command")
                           :test #'string=)
                   (rest cmd-words))
        do (pop cmd-words))
  (cond
    ((null cmd-words) 0)
    ((builtin-p (first cmd-words))
     (run-builtin sh (first cmd-words) (rest cmd-words) node))
    (t (format *error-output* "builtin: ~A: not a shell builtin~%"
               (first cmd-words))
       1)))

(defun exec-command-bypass (sh node cmd-words)
  "Run `command NAME args`: NAME resolves to a builtin or external, never a
function. Redirections and assignments on NODE still apply."
  (let ((use-standard-path nil))
    (loop while (and cmd-words (string= (first cmd-words) "-p"))
          do (pop cmd-words) (setf use-standard-path t))
    (when (null cmd-words) (return-from exec-command-bypass 0))
    (if use-standard-path
        (let ((saved (multiple-value-list (get-var sh "PATH"))))
          (unwind-protect
               (progn (set-var sh "PATH" +standard-path+)
                      (command-bypass-1 sh node cmd-words))
            (if (second saved)
                (setf (gethash "PATH" (shell-vars sh))
                      (cons (first saved) (third saved)))
                (remhash "PATH" (shell-vars sh)))))
        (command-bypass-1 sh node cmd-words))))

(defun command-bypass-1 (sh node cmd-words)
  ;; `command builtin exit 5': hand the rest to the builtin prefix, which runs
  ;; it in THIS context so the control-flow builtins still take effect.
  (when (and (string= (first cmd-words) "builtin") (rest cmd-words))
    (return-from command-bypass-1 (exec-builtin-prefix sh node (rest cmd-words))))
  ;; `command command seq 3': the prefix nests. Running the `command' BUILTIN
  ;; here instead would have it throw RUN-COMMAND-BYPASS to a tag only the
  ;; outer dispatch establishes -- which is exactly what crashed the shell with
  ;; "attempt to THROW to a tag that does not exist". -v and -V do not run
  ;; anything, so they stay in the builtin.
  (when (string= (first cmd-words) "command")
    (return-from command-bypass-1
      (if (member (second cmd-words) '("-v" "-V") :test #'string=)
          (run-builtin sh "command" (rest cmd-words) node)
          (exec-command-bypass sh node (rest cmd-words)))))
  (let ((name (first cmd-words)))
    (cond
      ((builtin-p name)
       ;; run the builtin with the command's redirections
       (run-builtin sh name (rest cmd-words) node))
      (t
       ;; build a synthetic simple command with cmd-words as the argv but
       ;; reuse NODE's assignments and redirects by spawning directly
       (catch 'not-found
         (let ((pid (spawn-external-words sh node cmd-words)))
           (return-from command-bypass-1 (wait-for pid))))
       127))))

(defun exec-exec-builtin (sh node exec-words)
  "Implement the `exec` builtin. With EXEC-WORDS non-empty, replace the shell
process image via posix_spawn semantics is not possible (spawn creates a
child); instead we spawn the command, wait, and exit with its status -- the
observable effect (the shell is replaced) is preserved for scripts. With no
words, apply NODE's redirections permanently to the shell."
  ;; `--' ends the options and `-a NAME' renames argv[0]. Both used to be taken
  ;; for the command, so `exec -- echo hi' reported `--: not found' and left the
  ;; shell 127.
  (let ((argv0 nil))
    (loop while (and exec-words (> (length (first exec-words)) 1)
                     (char= (char (first exec-words) 0) #\-))
          do (let ((o (pop exec-words)))
               (cond ((string= o "--") (return))
                     ((string= o "-a") (setf argv0 (pop exec-words)))
                     ((string= o "-c"))   ; empty environment: accepted
                     ((string= o "-l"))   ; login dash on argv[0]: accepted
                     (t (format *error-output* "exec: ~A: invalid option~%" o)
                        (return-from exec-exec-builtin 2)))))
    (if (null exec-words)
        ;; permanent redirections: apply and do NOT restore
        (progn
          (multiple-value-bind (saved temps)
              (apply-redirects-in-process sh (simple-command-redirects node))
            (declare (ignore saved))     ; intentionally not restored
            (dolist (p temps) (ignore-errors (delete-file p))))
          0)
        ;; exec a command: spawn it, and on success replace the shell (exit
        ;; with its status). A failure to find the command is fatal to the
        ;; shell.
        ;;
        ;; The redirections are part of the exec and take effect before it, so
        ;; `exec nosuchcmd 2>/dev/null' must swallow the diagnostic. Applying
        ;; them only inside the spawn meant the message escaped to the shell's
        ;; own stderr when the lookup failed.
        (let ((prog (find-in-path sh (first exec-words))))
          (if (null prog)
              (progn
                (ignore-errors
                 (apply-redirects-in-process sh (simple-command-redirects node)))
                (format *error-output* "exec: ~A: not found~%" (first exec-words))
                (finish-output *error-output*)
                (signal 'shell-exit :code 127) 127)
              (let ((pid (spawn-external-words sh node exec-words :argv0 argv0)))
                (let ((status (wait-for pid)))
                  (signal 'shell-exit :code status)
                  status)))))))

(defun spawn-external-words (sh node argv-words &key argv0)
  "Like spawn-external but with an explicit ARGV-WORDS list (already expanded),
reusing NODE's assignments and redirections.

ARGV0 replaces what the command sees as its own name, which is what
`exec -a NAME cmd' asks for; the program looked up is still ARGV-WORDS's
first element."
  (let* ((prog (or (find-in-path sh (first argv-words))
                   (progn (throw 'not-found
                            (list :command-error
                                  (report-not-found sh node (first argv-words)))))))
         (temp-env (apply-assignments sh (simple-command-assignments node)))
         (env (merge-env (exported-environ sh) temp-env)))
    (multiple-value-bind (redir-actions temps)
        (build-spawn-file-actions sh (simple-command-redirects node))
      (unwind-protect
           (spawn prog (if argv0 (cons argv0 (rest argv-words)) argv-words)
                  :env env :file-actions redir-actions
                  :sigdefault (child-sigdefaults sh))
        (dolist (p temps) (ignore-errors (delete-file p)))))))

(defun xtrace (sh node words)
  "Print the expanded simple command to stderr, prefixed by $PS4 (default '+ ')."
  (let ((ps4 (or (nth-value 0 (get-var sh "PS4")) "+ "))
        (assigns (loop for a in (simple-command-assignments node)
                       collect (format nil "~A=~A"
                                       (assignment-name a)
                                       (if (assignment-value a)
                                           (first (expand-word-to-fields
                                                   sh (word-text (assignment-value a))
                                                   :split nil :glob nil))
                                           "")))))
    (format *error-output* "~A~{~A~^ ~}~%"
            ps4 (append assigns words))
    (finish-output *error-output*)))

(defun split-subscript (name)
  "Split `a[i]' into (values \"a\" \"i\"); a plain name yields (values name nil)."
  (let ((open (position #\[ name)))
    (if (and open (plusp open) (char= (char name (1- (length name))) #\]))
        (values (subseq name 0 open) (subseq name (1+ open) (1- (length name))))
        (values name nil))))

(defun array-literal-p (value)
  "True if an assignment RHS is a parenthesised array literal."
  (let ((v (string-trim " " value)))
    (and (>= (length v) 2)
         (char= (char v 0) #\()
         (char= (char v (1- (length v))) #\)))))

(defun expand-array-literal (sh value)
  "Expand `(a b c)' into a list of elements, or an alist for `([k]=v ...)'.

Returns (values plain-elements indexed-pairs). Each element is expanded with
field splitting and globbing, exactly like command words -- `a=($x)' with
x=\"1 2\" gives two elements, which is the usual idiom for splitting a string."
  (let* ((v (string-trim " " value))
         (inner (subseq v 1 (1- (length v))))
         (plain '()) (pairs '()))
    (dolist (w (split-array-words inner))
      (multiple-value-bind (key val) (array-literal-entry w)
        (if key
            (push (cons (single-expand sh key)
                        (or (first (expand-word-to-fields sh val :split nil))
                            ""))
                  pairs)
            (dolist (f (expand-word-to-fields sh w)) (push f plain)))))
    (values (nreverse plain) (nreverse pairs))))

(defun array-literal-entry (w)
  "For `[k]=v' return (values \"k\" \"v\"), else (values nil nil)."
  (if (and (plusp (length w)) (char= (char w 0) #\[))
      (let ((close (position #\] w)))
        (if (and close (< (1+ close) (length w)) (char= (char w (1+ close)) #\=))
            (values (subseq w 1 close) (subseq w (+ close 2)))
            (values nil nil)))
      (values nil nil)))

(defun split-array-words (s)
  "Split an array literal's body on unquoted whitespace, keeping quoting and
substitutions intact for the later expansion pass."
  (let ((words '()) (buf (make-string-output-stream)) (i 0) (n (length s))
        (any nil))
    (flet ((flush () (let ((w (get-output-stream-string buf)))
                       (when (or any (plusp (length w))) (push w words))
                       (setf any nil))))
      (loop while (< i n) do
        (let ((c (char s i)))
          (cond
            ((member c '(#\Space #\Tab #\Newline)) (flush) (incf i))
            ((char= c #\\)
             (write-char c buf)
             (when (< (1+ i) n) (write-char (char s (1+ i)) buf))
             (setf any t) (incf i 2))
            ((char= c #\')
             (let ((j (position #\' s :start (1+ i))))
               (write-string (subseq s i (if j (1+ j) n)) buf)
               (setf any t i (if j (1+ j) n))))
            ((char= c #\")
             (let ((j (1+ i)))
               (loop while (< j n) do
                 (cond ((char= (char s j) #\\) (incf j 2))
                       ((char= (char s j) #\") (return))
                       (t (incf j))))
               (write-string (subseq s i (min n (1+ j))) buf)
               (setf any t i (min n (1+ j)))))
            ((and (char= c #\$) (< (1+ i) n) (char= (char s (1+ i)) #\())
             (let ((depth 0) (j (1+ i)))
               (loop while (< j n) do
                 (cond ((char= (char s j) #\() (incf depth) (incf j))
                       ((char= (char s j) #\)) (decf depth) (incf j)
                        (when (zerop depth) (return)))
                       (t (incf j))))
               (write-string (subseq s i j) buf)
               (setf any t i j)))
            (t (write-char c buf) (setf any t) (incf i)))))
      (flush))
    (nreverse words)))

(defun assign-one (sh name value appendp)
  "Perform one assignment, honouring array syntax on either side.

Four shapes: plain scalar, `a=(...)' whole-array, `a[i]=v' element, and the
`+=' variants of each."
  (multiple-value-bind (base sub) (split-subscript name)
    ;; Readonly is checked HERE, before anything is touched. SET-VAR checks it
    ;; too, but the array paths mutate the existing SH-ARRAY in place and only
    ;; then call SET-VAR -- so `readonly -a a; a+=(4)' raised the error with
    ;; the element already appended -- and the `a[i]=v' path never reaches
    ;; SET-VAR at all when the array exists.
    (when (readonly-p sh (resolve-nameref sh base))
      (error 'readonly-violation :name base))
    (cond
      ;; a=(...)  /  a+=(...)
      ((array-literal-p value)
       (multiple-value-bind (plain pairs) (expand-array-literal sh value)
         (let* ((existing (and appendp (var-array sh base)))
                (arr (or existing
                         (make-sh-array
                          ;; `declare -A' having run first is what makes this
                          ;; an associative array; otherwise indexed.
                          (let ((old (var-array sh base)))
                            (if (and old (eq (sh-array-kind old) :assoc))
                                :assoc :indexed))))))
           (let ((next (if existing (array-next-index arr) 0)))
             (dolist (v plain) (array-set arr next v) (incf next)))
           (dolist (kv pairs) (array-set arr (car kv) (cdr kv)))
           (set-var sh base arr)
           value)))
      ;; a[i]=v
      (sub
       (let ((arr (or (var-array sh base)
                      (let ((new (make-sh-array :indexed)))
                        ;; Promote an existing scalar to element 0, as bash does.
                        (multiple-value-bind (old found) (get-var sh base)
                          (when (and found old (plusp (length old)))
                            (array-set new 0 old)))
                        (set-var sh base new)
                        new))))
         (let ((key (if (eq (sh-array-kind arr) :assoc)
                        (single-expand sh sub)
                        (eval-arith sh sub))))
           (array-set arr key
                      (if appendp
                          (concatenate 'string (or (array-get arr key) "") value)
                          value))))
       value)
      ;; plain scalar
      (t
       (when (nth-value 1 (gethash base (shell-int-vars sh)))
         (setf value (princ-to-string (eval-arith sh value))))
       (let ((v (if appendp
                    (concatenate 'string (or (nth-value 0 (get-var sh base)) "")
                                 value)
                    value)))
         (set-var sh base v)
         v)))))

(defun apply-assignments (sh assignments &key to-shell)
  "Evaluate NAME=VALUE assignments. When TO-SHELL, set in the shell (persist);
otherwise return a list of temporary K=V for a command environment."
  (let ((temp '()) (saved '()))
    (unwind-protect
         (dolist (a assignments)
           (let* ((name (assignment-name a))
                  (vw (assignment-value a))
                  (raw (and vw (word-text vw)))
                  ;; An array literal must reach ASSIGN-ONE unexpanded: the
                  ;; per-element expansion has to see the original quoting, or
                  ;; `a=(1 "b c" 3)' loses the quotes and splits into four.
                  (literalp (and raw (array-literal-p raw)))
                  (val (cond
                         ((null vw) "")
                         (literalp raw)
                         (t (first (expand-word-to-fields sh raw
                                                          :split nil :glob nil
                                                          :assignment t))))))
             (setf val (or val ""))
             (cond
               ;; ASSIGN-ONE handles the array shapes and `+=' itself.
               (to-shell (assign-one sh name val (assignment-append a)))
               (t
                ;; POSIX evaluates these left to right, and an earlier one is
                ;; visible to a later one: `a=1 b=$a cmd' must give b=1. Bind
                ;; it in the shell for the duration of this list, then undo --
                ;; the value reaches the command through its environment.
                (push (cons name (multiple-value-list (get-var sh name))) saved)
                (set-var sh name val)
                (push (cons name val) temp)))))
      (dolist (entry saved)
        (destructuring-bind (name value found exported) (cons (car entry)
                                                              (cdr entry))
          (if found
              (setf (gethash name (shell-vars sh)) (cons value exported))
              (remhash name (shell-vars sh))))))
    (nreverse temp)))

(defparameter +special-builtins+
  '(":" "." "break" "continue" "eval" "exec" "exit" "export" "readonly"
    "return" "set" "shift" "times" "trap" "unset")
  "POSIX 2.14 special built-ins.

The distinction is not cosmetic: a variable assignment prefixed to a special
built-in persists after the command, while one prefixed to a regular built-in,
an external command or a function does not. `V=x :' leaves V set; `V=x true'
must not. bash --posix, dash, zsh and bash all agree on the regular case, and
bash --posix and dash on the special case (bash's default mode deviates, which
is one of the things `set -o posix' fixes).")

(defun special-builtin-p (name)
  (member name +special-builtins+ :test #'string=))

(defun bind-assignments (sh assignments)
  "Bind NAME=VALUE assignments in the shell and return an undo list for
RESTORE-ASSIGNMENTS.

This is the command-scoped form: the binding has to be VISIBLE while the
command runs -- `IFS=: read x y' is the whole reason the feature exists -- but
must not outlive it. APPLY-ASSIGNMENTS without :TO-SHELL cannot serve, because
it restores before it returns; that is right for an external command, which
receives the values through its environment, but not for anything we run
in-process."
  (let ((saved '()))
    (dolist (a assignments)
      (let* ((name (assignment-name a))
             (vw (assignment-value a))
             (raw (and vw (word-text vw)))
             (val (cond
                    ((null vw) "")
                    ((array-literal-p raw) raw)
                    (t (or (first (expand-word-to-fields sh raw
                                                         :split nil :glob nil
                                                         :assignment t))
                           "")))))
        ;; Push before setting, so an earlier assignment is visible to a later
        ;; one (`a=1 b=$a cmd') while still restoring to the original value.
        (push (cons name (multiple-value-list (get-var sh name))) saved)
        (assign-one sh name val (assignment-append a))
        ;; A prefix assignment goes into the command's ENVIRONMENT, so any
        ;; external command run while it is in force must see it -- including
        ;; one run inside a shell function, which is what makes `F=f f' with
        ;; `f() { printenv F; }' print f rather than nothing.
        (export-var sh name)))
    ;; The frame is boxed so UNSET can drop individual records from it.
    (let ((frame (list saved)))
      (push frame *temp-frames*)
      frame)))

(defun restore-assignments (sh frame)
  (when frame
    (setf *temp-frames* (remove frame *temp-frames* :test #'eq))
    (dolist (entry (car frame))
      (destructuring-bind (name value found exported) entry
        (if found
            (setf (gethash name (shell-vars sh)) (cons value exported))
            (remhash name (shell-vars sh)))))))

(defun run-builtin (sh name args node)
  "Run a builtin with redirections applied in-process."
  ;; Assignments persist only for a special built-in; for a regular one they
  ;; are scoped to the command, but still have to be visible while it runs.
  (let ((assign-undo nil))
    (if (special-builtin-p name)
        (apply-assignments sh (simple-command-assignments node) :to-shell t)
        (setf assign-undo
              (bind-assignments sh (simple-command-assignments node))))
  (let (saved temps)
    (handler-case
        (multiple-value-setq (saved temps)
          (apply-redirects-in-process sh (simple-command-redirects node)))
      ;; A redirection we cannot set up fails this command with status 1
      ;; rather than aborting the shell.
      (redirect-error (e)
        (format *error-output* "sxsh: ~A~%" e)
        (restore-assignments sh assign-undo)
        (return-from run-builtin 1)))
    (unwind-protect
         ;; NOTE: FUNC-RETURN is deliberately not caught here. Catching it made
         ;; `return' merely the exit status of the return builtin, so control
         ;; carried straight on to the next command: `f() { return 1; echo x; }'
         ;; printed x. It has to travel up to CALL-FUNCTION (or to the dot
         ;; script, or to RUN as a backstop).
         (funcall (find-builtin name) sh args *standard-output*)
      (finish-output *standard-output*)
      (restore-redirects saved)
      (restore-assignments sh assign-undo)
      (dolist (p temps) (ignore-errors (delete-file p)))))))

(defun child-sigdefaults (sh)
  "Signal numbers that must be reset to SIG_DFL in a spawned child.

The shell sets SIGTSTP/SIGTTIN/SIGTTOU to SIG_IGN for its own job-control
machinery, and SIG_IGN survives exec -- so without this a child would ignore
Ctrl-Z and could never be stopped. A signal the user explicitly ignored with
`trap '' SIG` is deliberately left out: POSIX requires that disposition to be
inherited by children.

SIGPIPE is reset ALWAYS, not just under job control, and for a different
reason: SBCL ignores it so that writes report an error instead, and that
disposition is inherited by every child we spawn. The effect is that a
producer outliving its consumer does not die quietly the way it should --
`yes | head -2' printed `yes: standard output: Broken pipe' and the producer
exited 1 rather than 141. Every shell relies on the default disposition here."
  (append
   (unless (equal "" (gethash "PIPE" (shell-traps sh)))
     (list sb-unix:sigpipe))
   (when (shell-job-control sh)
     (loop for (name . num) in `(("TSTP" . ,sb-unix:sigtstp)
                                 ("TTIN" . ,sb-unix:sigttin)
                                 ("TTOU" . ,sb-unix:sigttou))
           unless (equal "" (gethash name (shell-traps sh)))
             collect num))))

(defun spawn-external (sh node &key extra-actions setpgroup (pgroup 0) words)
  "Spawn the external command described by simple-command NODE. Temporary
assignments become part of the child environment. When SETPGROUP is true, the
child is placed in process group PGROUP (0 = new group led by the child), for
job control. Returns the pid.

WORDS supplies an already-expanded argv. Pass it whenever the caller has
expanded the command (they all have, via EXTERNAL-SIMPLE-COMMAND-P): expanding
a second time re-runs any command substitution in the words."
  (let* ((words (or words (expand-command-words sh node)))
         (prog (or (find-in-path sh (first words))
                   (return-from spawn-external
                     (signal-not-found
                      (report-not-found sh node (first words))))))
         (temp-env (apply-assignments sh (simple-command-assignments node)))
         (env (merge-env (exported-environ sh) temp-env)))
    (multiple-value-bind (redir-actions temps)
        (handler-case (build-spawn-file-actions sh (simple-command-redirects node))
          ;; A redirection we cannot set up fails this command, not the shell.
          (redirect-error (e)
            (format *error-output* "sxsh: ~A~%" e)
            (setf (shell-last-status sh) 1)
            ;; distinct from a failed PATH lookup: that is 127, a redirection
            ;; that could not be set up is 1
            (throw 'not-found :redirect-error)))
      (let ((all-actions (append extra-actions redir-actions)))
        (unwind-protect
             (spawn prog words :env env :file-actions all-actions
                               :setpgroup setpgroup :pgroup pgroup
                               :sigdefault (child-sigdefaults sh))
          (dolist (p temps) (ignore-errors (delete-file p))))))))

(defun signal-not-found (&optional (status 127))
  ;; a sentinel: callers expect a pid to wait on, so we throw to a handler
  ;; instead. The status travels in a marker that cannot be mistaken for a pid.
  (throw 'not-found (list :command-error status)))

(defun merge-env (base overrides)
  "OVERRIDES is a list of (name . value). Returns merged K=V list."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (kv base)
      (let ((eq (position #\= kv)))
        (when eq (setf (gethash (subseq kv 0 eq) table) (subseq kv (1+ eq))))))
    (dolist (o overrides) (setf (gethash (car o) table) (cdr o)))
    (let ((out '()))
      (maphash (lambda (k v) (push (format nil "~A=~A" k v) out)) table)
      out)))

;;; ---------------------------------------------------------------------------
;;; Functions
;;; ---------------------------------------------------------------------------

(defun exec-func-def (sh node)
  (setf (gethash (function-def-name node) (shell-functions sh)) node)
  0)

(defvar *function-depth* 0
  "How many shell functions are currently on the stack.")

(defparameter +max-function-depth+ 1000
  "Nesting limit for shell function calls.

POSIX does not require a limit, but the alternative is not \"recurse
forever\" -- it is exhausting the Lisp control stack, which SBCL reports as
`INFO: Control stack guard page unprotected' followed by a backtrace. A shell
must fail as a shell. Found by fuzzing, which reduced it to `f()${}f' followed
by a call to f.")

(defun call-function (sh def words node)
  "Invoke a shell function. Positional params become the call args; $0 stays."
  (when (>= *function-depth* +max-function-depth+)
    (format *error-output*
            "sxsh: ~A: maximum function nesting level exceeded (~D)~%"
            (first words) +max-function-depth+)
    (return-from call-function 1))
  (let ((saved-pos (shell-positional sh))
        (*function-depth* (1+ *function-depth*)))
    ;; Scoped to the call, not persistent: `V=x f' must leave V as it was.
    ;; bash, bash --posix, dash and zsh all agree here -- unlike the special
    ;; built-in case, where dash and bash --posix persist.
    (let ((assign-undo (bind-assignments sh (simple-command-assignments node))))
    (set-positional sh (rest words))
    (multiple-value-bind (saved temps)
        (apply-redirects-in-process sh (simple-command-redirects node))
      (push-local-frame sh)
      (unwind-protect
           (handler-case (exec-node sh (function-def-body def))
             (func-return (e) (cf-code e)))
        ;; Popped in the cleanup so locals are restored however the function
        ;; ends -- `return', falling off the end, `set -e', or a `break'
        ;; unwinding past it.
        (pop-local-frame sh)
        (restore-redirects saved)
        (dolist (p temps) (ignore-errors (delete-file p)))
        (restore-assignments sh assign-undo)
        (setf (shell-positional sh) saved-pos))))))

;;; ---------------------------------------------------------------------------
;;; Compound commands
;;; ---------------------------------------------------------------------------

(defun command-error-status (name)
  "POSIX 2.8.2: 126 when the command is found but cannot be executed, 127 when
it is not found at all."
  (if (and (find #\/ name) (probe-file name)) 126 127))

(defun report-not-found (sh node name)
  "Emit the `command not found' diagnostic with NODE's redirections in force.

A real shell forks and the child applies the redirections before reporting, so
`nosuchcmd 2>/dev/null' is silent. We spawn instead of forking, and the child
never exists when the lookup fails -- so the message would come out of the
shell's own unredirected stderr unless we apply the redirections in-process
around it."
  (exec-with-redirects
   sh (and node (simple-command-redirects node))
   (lambda ()
     (format *error-output* "~A: ~A~%" name
             (if (= (command-error-status name) 126)
                 "Permission denied"
                 "command not found"))
     (finish-output *error-output*)))
  (setf (shell-last-status sh) (command-error-status name))
  (command-error-status name))

(defun exec-with-redirects (sh redirects thunk)
  (if (null redirects)
      (funcall thunk)
      (let (saved temps (ok t))
        ;; A redirection that cannot be set up fails *this command* with status
        ;; 1; it must not abort the shell. Only the setup is guarded -- errors
        ;; from the command itself still propagate.
        (handler-case
            (multiple-value-setq (saved temps)
              (apply-redirects-in-process sh redirects))
          (redirect-error (e)
            (format *error-output* "sxsh: ~A~%" e)
            (setf ok nil)))
        (if (not ok)
            1
            (unwind-protect (funcall thunk)
              (restore-redirects saved)
              (dolist (p temps) (ignore-errors (delete-file p))))))))

(defun exec-brace (sh node)
  (exec-with-redirects sh (brace-group-redirects node)
                       (lambda () (exec-node sh (brace-group-body node)))))

(defun exec-subshell (sh node)
  "A subshell should isolate state. We spawn a child copy of ourselves is not
possible without fork; instead we snapshot/restore shell state around the body
and run it in-process. State changes (cd, var sets) are rolled back."
  (exec-with-redirects
   sh (subshell-redirects node)
   (lambda ()
     (let ((snap (snapshot-shell sh)))
       (unwind-protect
            (progn
              ;; The parent's EXIT trap belongs to the parent: it must fire
              ;; when the shell exits, not each time a subshell ends. One set
              ;; INSIDE the subshell does fire here, when the subshell ends.
              (remhash "EXIT" (shell-traps sh))
              ;; A subshell is a separate execution environment, so control
              ;; flow must not escape it: `(break)' cannot break the caller's
              ;; loop and `(return 4)' cannot return from the caller's
              ;; function. Both terminate the subshell body instead -- bash,
              ;; zsh, dash and bash 3.2 all agree on that. For the status they
              ;; agree too on return (the return code); break/continue end the
              ;; body with 0, following zsh, which is the shell sxsh already
              ;; matches on the related unspecified question of whether break
              ;; crosses a function boundary.
              (prog1 (handler-case (exec-node sh (subshell-body node))
                       (shell-exit (e) (or (shell-exit-code e) 0))
                       (func-return (e) (or (cf-code e) 0))
                       (loop-break () 0)
                       (loop-continue () 0))
                (run-exit-traps sh)))
         (restore-shell sh snap))))))

(defun snapshot-shell (sh)
  (list (alexandria-copy-hash (shell-vars sh))
        (alexandria-copy-hash (shell-functions sh))
        (copy-seq (shell-positional sh))
        (ignore-errors (current-directory))
        ;; traps are shell state too: without this, `(trap "..." EXIT)' leaked
        ;; out and fired when the whole shell exited
        (alexandria-copy-hash (shell-traps sh))
        ;; ...and so are aliases, now that substitution happens in the parser
        (alexandria-copy-hash (shell-aliases sh))
        ;; A subshell inherits the directory stack and its pushd/popd stay
        ;; inside it, exactly as its cd does.
        (copy-list (shell-dir-stack sh))))

(defun restore-shell (sh snap)
  (destructuring-bind (vars funcs pos cwd traps aliases dirs) snap
    (setf (shell-vars sh) vars
          (shell-functions sh) funcs
          (shell-positional sh) pos
          (shell-traps sh) traps
          (shell-aliases sh) aliases
          (shell-dir-stack sh) dirs)
    (when cwd (ignore-errors (change-directory cwd)))))

(defun alexandria-copy-hash (ht)
  (let ((new (make-hash-table :test (hash-table-test ht))))
    (maphash (lambda (k v) (setf (gethash k new) v)) ht)
    new))

(defun exec-if (sh node)
  ;; Redirections on a compound command apply to the whole construct. The
  ;; parser has always collected these; the executor used to drop them, so
  ;; `while read l; do ...; done <<EOF' and `if read l; then ...; fi < file'
  ;; ran with the shell's own stdin and read nothing.
  (exec-with-redirects
   sh (if-clause-redirects node)
   (lambda ()
     (if (zerop (without-errexit (exec-node sh (if-clause-condition node))))
         (exec-node sh (if-clause-then node))
         (let ((else (if-clause-else node)))
           (if else (exec-node sh else) 0))))))

(defun exec-while (sh node until-p)
  (exec-with-redirects
   sh (if until-p (until-clause-redirects node) (while-clause-redirects node))
   (lambda () (exec-while-1 sh node until-p))))

(defun exec-while-1 (sh node until-p)
  (let ((status 0))
    (handler-case
        (loop
          (run-pending-traps sh)
          (let ((c (without-errexit
                     (exec-node sh (if until-p (until-clause-condition node)
                                       (while-clause-condition node))))))
            (when (if until-p (zerop c) (not (zerop c))) (return)))
          (handler-case
              (setf status (exec-node sh (if until-p (until-clause-body node)
                                             (while-clause-body node))))
            (loop-continue (e) (when (> (cf-n e) 1)
                                 (signal 'loop-continue :n (1- (cf-n e)))))))
      ;; `break' is the last command executed, and it succeeded, so the loop's
      ;; status is 0 -- not whatever the body left behind on an EARLIER pass.
      ;; The signal unwinds before the enclosing and-or can record its own
      ;; result, so `while true; do x=$((x-1)); [ $x = 0 ] && break; done'
      ;; kept the 1 from the first iteration's failed test.
      (loop-break (e) (if (> (cf-n e) 1)
                          (signal 'loop-break :n (1- (cf-n e)))
                          (setf status 0))))
    status))

(defun expand-word-list (sh words)
  "Expand a `for'/`select' word list into items.

Brace expansion comes first and can turn one word into several, exactly as in
EXPAND-COMMAND-WORDS. Calling EXPAND-WORD-TO-FIELDS alone skipped it, so
`for i in -{a,b}' iterated once over the literal `-{a,b}' -- while the same
word passed to `echo' expanded correctly, which is what made it look like
brace expansion worked at all."
  (loop for w in words
        append (loop for raw in (brace-expand (word-text w))
                     append (expand-word-to-fields sh raw))))

(defun exec-for (sh node)
  (exec-with-redirects
   sh (for-clause-redirects node)
   (lambda () (exec-for-1 sh node))))

(defun exec-for-1 (sh node)
  (let* ((name (for-clause-name node))
         (words-spec (for-clause-words node))
         (items (if (eq words-spec :default)
                    (coerce (shell-positional sh) 'list)
                    (expand-word-list sh words-spec)))
         (status 0))
    (handler-case
        (dolist (item items)
          (run-pending-traps sh)
          (set-var sh name item)
          (handler-case
              (setf status (exec-node sh (for-clause-body node)))
            (loop-continue (e) (when (> (cf-n e) 1)
                                 (signal 'loop-continue :n (1- (cf-n e)))))))
      ;; See EXEC-WHILE-1: a completed `break' leaves status 0.
      (loop-break (e) (if (> (cf-n e) 1)
                          (signal 'loop-break :n (1- (cf-n e)))
                          (setf status 0))))
    status))

(defun exec-arith-command (sh node)
  "bash `((expr))': evaluate, status 0 when the value is non-zero.

The inverted sense is deliberate and is what makes `((i < n))' usable as a
loop condition -- an arithmetic 0 is false, like C."
  (exec-with-redirects
   sh (arith-command-redirects node)
   (lambda ()
     (if (zerop (eval-arith sh (arith-command-expr node))) 1 0))))

(defun exec-arith-for (sh node)
  "bash `for ((init; cond; step)) do ... done'."
  (exec-with-redirects
   sh (arith-for-redirects node)
   (lambda ()
     (let ((status 0))
       (unless (string= (string-trim " " (arith-for-init node)) "")
         (eval-arith sh (arith-for-init node)))
       (loop
         ;; An omitted condition is true, as in C.
         (let ((test (arith-for-cond node)))
           (unless (or (string= (string-trim " " test) "")
                       (/= 0 (eval-arith sh test)))
             (return status)))
         (handler-case (setf status (exec-node sh (arith-for-body node)))
           (loop-break (e)
             (when (> (cf-n e) 1) (signal 'loop-break :n (1- (cf-n e))))
             (return status))
           (loop-continue (e)
             (when (> (cf-n e) 1) (signal 'loop-continue :n (1- (cf-n e))))))
         (unless (string= (string-trim " " (arith-for-step node)) "")
           (eval-arith sh (arith-for-step node))))))))

(defun exec-coproc (sh node)
  "bash `coproc [NAME] command'.

Two pipes: the command's stdout comes back to us, and our writes go to its
stdin. NAME becomes a two-element array holding OUR ends -- [0] to read the
coprocess's output, [1] to write to its input -- and NAME_PID holds its pid.

Our ends are left inheritable on purpose. `read -u ${COPROC[0]}' is the point
of the feature, and a later spawned command must be able to reach them; they
outlive this command, unlike a process substitution's."
  (let ((name (coproc-clause-name node))
        (self (self-exec-path))
        (body (ignore-errors (deparse (coproc-clause-body node)))))
    (if (not (and self body))
        (progn (format *error-output* "coproc: cannot start coprocess~%") 1)
        (multiple-value-bind (from-r from-w) (sb-posix:pipe)   ; coproc stdout
          (multiple-value-bind (to-r to-w) (sb-posix:pipe)     ; coproc stdin
            (handler-case
                (let ((pid (spawn self
                                  (list "sxsh" "-c"
                                        (concatenate 'string (async-prelude sh) body))
                                  :env (exported-environ sh)
                                  :setpgroup t :pgroup 0
                                  :sigdefault (child-sigdefaults sh)
                                  :file-actions
                                  (list (fa-dup2 to-r 0)
                                        (fa-dup2 from-w 1)
                                        (fa-close from-r)
                                        (fa-close to-w)))))
                  ;; The child owns the far ends now.
                  (sb-posix:close from-w)
                  (sb-posix:close to-r)
                  (set-var sh name
                           (array-from-list (list (princ-to-string from-r)
                                                  (princ-to-string to-w))))
                  (set-var sh (concatenate 'string name "_PID")
                           (princ-to-string pid))
                  (setf (shell-last-bg-pid sh) pid)
                  (add-job sh pid (list pid) (or body "coproc"))
                  0)
              (error ()
                (dolist (fd (list from-r from-w to-r to-w))
                  (ignore-errors (sb-posix:close fd)))
                (format *error-output* "coproc: cannot start coprocess~%")
                1)))))))

(defun exec-select (sh node)
  "bash `select NAME in WORDS; do ... done'.

The menu goes to stderr, not stdout, so the loop body's output can still be
redirected on its own. An empty reply reprints the menu; an out-of-range one
sets NAME empty but still runs the body; EOF ends the loop."
  (exec-with-redirects
   sh (select-clause-redirects node)
   (lambda ()
     (let* ((raw (select-clause-words node))
            (items (if (eq raw :default)
                       (coerce (shell-positional sh) 'list)
                       (expand-word-list sh raw)))
            (status 0))
       (loop
         (print-select-menu items)
         (write-string (or (nth-value 0 (get-var sh "PS3")) "#? ") *error-output*)
         (finish-output *error-output*)
         (multiple-value-bind (line escaped eof) (read-one-logical-line 0 nil)
           (declare (ignore escaped))
           (when (or (eq line :eof) (and eof (string= line "")))
             ;; bash ends the prompt line before giving up on EOF.
             (terpri *error-output*)
             (finish-output *error-output*)
             (return status))
           (set-var sh "REPLY" line)
           (let* ((idx (ignore-errors (parse-integer (string-trim " " line))))
                  (choice (and idx (<= 1 idx (length items)) (nth (1- idx) items))))
             ;; An empty line reprints the menu without running the body.
             (unless (string= (string-trim " " line) "")
               (set-var sh (select-clause-name node) (or choice ""))
               (handler-case (setf status (exec-node sh (select-clause-body node)))
                 (loop-break (e)
                   (when (> (cf-n e) 1) (signal 'loop-break :n (1- (cf-n e))))
                   (return status))
                 (loop-continue (e)
                   (when (> (cf-n e) 1)
                     (signal 'loop-continue :n (1- (cf-n e))))))))))))))

(defun print-select-menu (items)
  (loop for it in items for i from 1
        do (format *error-output* "~D) ~A~%" i it))
  (finish-output *error-output*))

(defun exec-case (sh node)
  (exec-with-redirects
   sh (case-clause-redirects node)
   (lambda () (exec-case-1 sh node))))

(defun exec-case-1 (sh node)
  (let ((word (first (expand-word-to-fields sh (word-text (case-clause-word node))
                                            :split nil :glob nil)))
        (status 0)
        ;; bash `;&' runs the NEXT clause's body without testing its pattern.
        (falling nil))
    (setf word (or word ""))
    (flet ((matches-p (item)
             (some (lambda (pat)
                     ;; Render with quoting preserved: plain quote removal
                     ;; would turn `a\*b' into a live wildcard.
                     (let ((p (xchars->pattern
                               (expand-pass sh (word-text pat) :tilde nil))))
                       (shell-pattern-match (or p "") word)))
                   (case-item-patterns item))))
      (dolist (item (case-clause-items node) status)
        (when (or falling (matches-p item))
          (setf status (if (case-item-body item)
                           (exec-node sh (case-item-body item))
                           0))
          (ecase (or (case-item-terminator item) :\;\;)
            ;; `;;' -- done.
            (:\;\; (return status))
            ;; `;&' -- fall through into the next body unconditionally.
            (:\;& (setf falling t))
            ;; `;;&' -- stop falling through, but keep testing the patterns
            ;; that follow, so more than one clause can run.
            (:\;\;& (setf falling nil))))))))

;;; ---------------------------------------------------------------------------
;;; Background execution & waiting
;;; ---------------------------------------------------------------------------

(defun exec-async (sh node)
  "Run NODE in the background as a job, returning 0.

External simple commands and pipelines are spawned directly into their own
process group. Everything else -- builtins, functions, and compound commands --
becomes a genuine asynchronous job by re-executing this sxsh binary with `-c`
on the deparsed source (see ASYNC-COMPOUND); we cannot fork, so re-exec is the
only way to get a second process. If no re-exec is possible the command falls
back to running in-process, synchronously.

A failure to START the job -- a redirection that cannot be opened, most often
-- belongs to the job, not to the list that launched it. POSIX gives an
asynchronous list status 0 regardless, so the diagnostic is printed and 0
returned. Letting the condition escape aborted the whole list instead: in
`wc f > nodir/x & echo after' the `echo' never ran."
  (handler-case (exec-async-1 sh node)
    (error (e)
      (format *error-output* "sxsh: ~A~%" e)
      (finish-output *error-output*)
      0)))

(defun exec-async-1 (sh node)
  (let ((cmd-text (ignore-errors (deparse node))))
    ;; Each command is classified exactly once and its expanded argv carried
    ;; to the spawn: EXTERNAL-SIMPLE-COMMAND-P expands, and expanding twice
    ;; would re-run any command substitution in the words.
    (if (eq (ast-type node) :pipeline)
        (let* ((cmds (pipeline-commands node))
               (classified (mapcar (lambda (c)
                                     (multiple-value-list
                                      (external-simple-command-p sh c)))
                                   cmds)))
          (if (every #'first classified)
              (async-pipeline sh node cmd-text (mapcar #'second classified))
              (async-compound sh node cmd-text)))
        (multiple-value-bind (externalp words) (external-simple-command-p sh node)
          (if externalp
              (catch 'not-found
                (let ((pid (spawn-external sh node :words words
                                                   :setpgroup t :pgroup 0)))
                  (let ((job (add-job sh pid (list pid) (or cmd-text "?"))))
                    (announce-bg sh job))
                  0))
              (async-compound sh node cmd-text))))))

;;; ---------------------------------------------------------------------------
;;; Asynchronous compounds: re-exec ourselves with the deparsed source
;;; ---------------------------------------------------------------------------


(defun self-exec-path ()
  "Path to this sxsh executable, or NIL if we are not a saved image.

Under `sbcl --load` the runtime is plain sbcl, which knows nothing about `-c`;
re-executing it would run the wrong program entirely. In a saved executable
the core is appended to the runtime, so the two pathnames coincide -- that
equality is the test."
  (let ((runtime (ignore-errors (namestring sb-ext:*runtime-pathname*)))
        (core (ignore-errors (namestring sb-ext:*core-pathname*))))
    (when (and runtime core (string= runtime core) (probe-file runtime))
      runtime)))

(defun assignable-name-p (name)
  "True for strings that are valid shell variable names, so that entries like
the environment's odd 'BASH_FUNC_x%%' cannot produce unparseable source."
  (and (plusp (length name))
       (or (alpha-char-p (char name 0)) (char= (char name 0) #\_))
       (every (lambda (c) (or (alphanumericp c) (char= c #\_))) name)))

(defun async-prelude (sh)
  "Shell source reproducing enough of SH's state for a fresh sxsh to run a
background command faithfully: cwd, shell options, shell (non-exported)
variables, function definitions, and positional parameters. Exported variables
need no prelude -- they travel in the child's environment.

The OPTIONS matter as much as the variables. A re-executed pipeline stage that
did not inherit them ran with different globbing rules from the shell that
started it, so `shopt -s nullglob; echo nomatch* | wc -w' reported 1 instead of
0 -- the stage globbed under the child's defaults."
  (with-output-to-string (s)
    (let ((cwd (ignore-errors (current-directory))))
      (when cwd (format s "cd ~A~%" (shell-quote cwd))))
    ;; `set -o' flags, then shopts.
    (maphash (lambda (kw on)
               (when on
                 (let ((entry (find kw +shell-options+ :key #'third)))
                   (when entry
                     (format s "set -o ~A~%" (second entry))))))
             (shell-options sh))
    (maphash (lambda (name on)
               (when on (format s "shopt -s ~A~%" name)))
             (shell-shopts sh))
    (maphash (lambda (k cell)
               (when (and (not (cdr cell))         ; not exported
                          (assignable-name-p k))
                 ;; An array cannot travel as a scalar: emit it as a literal
                 ;; so the child rebuilds it, subscripts and all. Without this
                 ;; SHELL-QUOTE was handed a struct.
                 (let ((v (car cell)))
                   (if (sh-array-p v)
                       (progn
                         (when (eq (sh-array-kind v) :assoc)
                           (format s "declare -A ~A~%" k))
                         (format s "~A=(~{~A~^ ~})~%" k
                                 (mapcar (lambda (key)
                                           (format nil "[~A]=~A"
                                                   (shell-quote (princ-to-string key))
                                                   (shell-quote (or (array-get v key) ""))))
                                         (array-keys v))))
                       (format s "~A=~A~%" k (shell-quote v))))))
             (shell-vars sh))
    (maphash (lambda (k def)
               (declare (ignore k))
               (let ((src (ignore-errors (deparse def))))
                 (when src (format s "~A~%" src))))
             (shell-functions sh))
    (when (plusp (length (shell-positional sh)))
      (format s "set --~{ ~A~}~%"
              (map 'list #'shell-quote (shell-positional sh))))))

(defun async-compound (sh node cmd-text)
  "Run a builtin/function/compound NODE as a real background job by spawning
this same binary with `-c <prelude + source>`. Falls back to synchronous
in-process execution when that is not possible."
  (let* ((self (self-exec-path))
         (body (and self (ignore-errors (deparse node))))
         (src (and body (concatenate 'string (async-prelude sh) body))))
    (if (or (null src) (> (length src) +max-async-source+))
        (async-compound-inline sh node cmd-text)
        (handler-case
            (let ((pid (spawn self (list "sxsh" "-c" src)
                              :env (exported-environ sh)
                              :setpgroup t :pgroup 0
                              :sigdefault (child-sigdefaults sh))))
              (let ((job (add-job sh pid (list pid) (or cmd-text "?"))))
                (announce-bg sh job))
              0)
          (error () (async-compound-inline sh node cmd-text))))))

(defun async-compound-inline (sh node cmd-text)
  "Fallback for when we cannot re-exec: run NODE synchronously in-process and
record it as an already-finished job. $! is left alone -- there is no pid."
  (let ((status (exec-node sh node)))
    (let ((job (add-job sh nil nil (or cmd-text "?") :state :done)))
      (setf (job-status job) status (job-notified job) t)
      (remove-job sh job))
    0))

(defun async-pipeline (sh node cmd-text stage-words)
  "Spawn every stage of a pipeline into a single new process group, tracked as
one background job. All stages are external (checked by caller); STAGE-WORDS
holds their already-expanded argvs, one per stage, in order."
  (let* ((cmds (pipeline-commands node))
         (n (length cmds))
         (pids '()) (pgid nil) (prev-read nil))
    (unwind-protect
         (loop for cmd in cmds for words in stage-words for i from 0 do
           (let* ((last (= i (1- n)))
                  (rw (unless last (multiple-value-list (sb-posix:pipe))))
                  (read-fd (first rw)) (write-fd (second rw))
                  (stdin (or prev-read 0))
                  (stdout (if last 1 write-fd)))
             (let ((pid (spawn-external
                         sh cmd
                         :words words
                         :setpgroup t
                         :pgroup (or pgid 0)  ; first stage makes the group
                         :extra-actions
                         (append
                          (unless (= stdin 0) (list (fa-dup2 stdin 0)))
                          (unless (= stdout 1) (list (fa-dup2 stdout 1)))
                          (mapcar #'fa-close
                                  (remove nil (list prev-read read-fd write-fd)))))))
               (push pid pids)
               (unless pgid (setf pgid pid)))  ; group leader = first pid
             (when prev-read (sb-posix:close prev-read))
             (when write-fd (sb-posix:close write-fd))
             (setf prev-read read-fd)))
      (when prev-read (ignore-errors (sb-posix:close prev-read))))
    (let ((job (add-job sh pgid (nreverse pids) (or cmd-text "?"))))
      (announce-bg sh job))
    0))

(defun announce-bg (sh job)
  "Print the [n] pid line when starting a background job (interactive)."
  (when (shell-interactive sh)
    (format *error-output* "[~D] ~D~%" (job-id job) (car (last (job-pids job)))))
  0)

(defun wait-for (pid)
  "Wait for PID, return a shell exit status (128+sig if signalled)."
  (multiple-value-bind (wpid status) (sb-posix:waitpid pid 0)
    (declare (ignore wpid))
    (decode-wait-status status)))

(defun wait-and-decode (pid)
  "Like WAIT-FOR but tolerant of a pid that has already been reaped or does not
exist (returns 127), for the `wait` builtin."
  (handler-case (wait-for pid)
    (error () 127)))

(defun decode-wait-status (status)
  "Decode a raw wait(2) status into a shell exit code, portably across Linux
and macOS (both share the same status encoding). We avoid sb-posix's W*
macros, which aren't reliably exported on every platform/SBCL build.

Encoding:
  * low 7 bits  = terminating signal (0 => exited normally)
  * bit 7 (0x80) = core dumped flag
  * bits 8-15   = exit code, valid only when the signal bits are 0
  * a low byte of 0x7f indicates the child stopped (job control)"
  (let ((termsig (logand status #x7f)))
    (cond
      ;; stopped (0x7f in low 7 bits): treat like signalled for our purposes
      ((= termsig #x7f) (+ 128 (logand (ash status -8) #xff)))
      ;; exited normally
      ((zerop termsig) (logand (ash status -8) #xff))
      ;; killed by a signal
      (t (+ 128 termsig)))))

;;; ---------------------------------------------------------------------------
;;; Command substitution & eval helpers (referenced by expand.lisp / builtins)
;;; ---------------------------------------------------------------------------

(defun redirect-only-substitution (sh ast)
  "For `$(< FILE)' return (values EXPANDED-PATH T); otherwise (values NIL NIL).

The shape has to be exactly one simple command with no words, no assignments
and one `<' redirect. Matching on the source text instead would misread
`$(< a; echo b)' and `$(cat < a)', both of which are ordinary commands."
  (let ((entries (and (= 1 (length ast))
                      (eq (ast-type (first ast)) :list)
                      (complete-command-entries (first ast)))))
    (unless (and entries (= 1 (length entries))) (return-from redirect-only-substitution nil))
    (let ((cmd (car (first entries))))
      (unless (and (eq (ast-type cmd) :simple)
                   (null (simple-command-words cmd))
                   (null (simple-command-assignments cmd))
                   (= 1 (length (simple-command-redirects cmd))))
        (return-from redirect-only-substitution nil))
      (let ((r (first (simple-command-redirects cmd))))
        (unless (and (eq (redirect-op r) :<) (null (redirect-fd r)))
          (return-from redirect-only-substitution nil))
        (values (first (expand-word-to-fields sh (word-text (redirect-target r))
                                              :split nil))
                t)))))

(defun command-substitute (sh src)
  "Run SRC, capture its stdout, strip trailing newlines. Captures through a
temp file so large output can't deadlock on a bounded pipe buffer."
  (let ((ast (handler-case (parse-string src)
               (error () (return-from command-substitute "")))))
    ;; `$(< file)' and `` `< file` '' are not a command at all: ksh and bash
    ;; read the file directly, without forking anything. Falling through to
    ;; the general path applied the redirection to a command that produced no
    ;; output, so the substitution was empty -- silently, which is the worst
    ;; way for a widely used idiom to fail.
    (multiple-value-bind (path found) (redirect-only-substitution sh ast)
      (when found
        (return-from command-substitute
          (let ((text (and path (ignore-errors (slurp-file path)))))
            (cond (text (string-right-trim '(#\Newline) text))
                  (t
                   ;; The redirection still has to be REPORTED when it fails,
                   ;; or a typo'd filename yields an empty string with nothing
                   ;; said. Status is the substitution's, which a surrounding
                   ;; command overrides as usual.
                   (format *error-output* "sxsh: ~A: No such file or directory~%"
                           (or path ""))
                   (setf (shell-last-status sh) 1
                         (shell-last-cmdsub-status sh) 1)
                   ""))))))
    (let* ((path (format nil "/tmp/sxsh-cmdsub-~A-~A"
                         (sb-posix:getpid) (random 1000000)))
           (fd (sb-posix:open path (logior +o-wronly+ +o-creat+ +o-trunc+) #o600))
           (saved-out (sb-posix:dup 1)))
      (unwind-protect
           (progn
             (sb-posix:dup2 fd 1)
             (sb-posix:close fd)
             (let ((*standard-output*
                     (sb-sys:make-fd-stream 1 :output t :buffering :full)))
               (let ((snap (snapshot-shell sh)))
                 (unwind-protect
                      ;; `$(...)' is its own execution ENVIRONMENT, not merely
                      ;; its own control-flow scope. Catching the control-flow
                      ;; conditions but sharing the state meant everything it
                      ;; touched escaped: `x=1; echo $(x=2)' left x at 2, a
                      ;; function redefined inside one replaced the outer one,
                      ;; and a `cd' moved the whole shell.
                      (handler-case (run sh ast)
                        (shell-exit () nil)
                        (func-return () nil)
                        (loop-break () nil)
                        (loop-continue () nil))
                   (finish-output *standard-output*)
                   (restore-shell sh snap)))
               ;; POSIX: remember this substitution's exit status so a command
               ;; made only of assignments can adopt it as its own $?.
               (setf (shell-last-cmdsub-status sh) (shell-last-status sh))))
        (sb-posix:dup2 saved-out 1)
        (sb-posix:close saved-out))
      (unwind-protect
           (with-open-file (s path :if-does-not-exist nil)
             (if s
                 (let ((buf (make-string (file-length s))))
                   (let ((k (read-sequence buf s)))
                     (string-right-trim '(#\Newline) (subseq buf 0 k))))
                 ""))
        (ignore-errors (delete-file path))))))

;;; ---------------------------------------------------------------------------
;;; bash [[ ... ]]
;;;
;;; Not `test' with extra spelling. Inside [[ ]] there is no field splitting
;;; and no pathname expansion, `<' and `>' are string comparisons rather than
;;; redirections, the right operand of = / == / != is a PATTERN, and =~ is a
;;; regular expression whose capture groups land in $BASH_REMATCH. That is why
;;; the lexer hands us the raw text and the words are split here.
;;; ---------------------------------------------------------------------------

(defparameter +cond-unary-ops+
  '("-e" "-f" "-d" "-r" "-w" "-x" "-s" "-L" "-h" "-p" "-S" "-b" "-c" "-g" "-u"
    "-k" "-z" "-n" "-v" "-o" "-t")
  "Unary operators accepted inside [[ ]].")

(defparameter +cond-binary-ops+
  '("=" "==" "!=" "=~" "<" ">" "-eq" "-ne" "-lt" "-le" "-gt" "-ge"
    "-nt" "-ot" "-ef")
  "Binary operators accepted inside [[ ]].")

(defun exec-cond-expr (sh node)
  (exec-with-redirects
   sh (cond-expr-redirects node)
   (lambda ()
     (let ((words (split-array-words (cond-expr-text node))))
       (multiple-value-bind (value rest) (cond-parse-or sh words)
         (declare (ignore rest))
         (if value 0 1))))))

(defun cond-parse-or (sh words)
  (multiple-value-bind (left rest) (cond-parse-and sh words)
    (loop while (and rest (string= (first rest) "||"))
          do (multiple-value-bind (right more) (cond-parse-and sh (rest rest))
               ;; No short-circuit on evaluation order here: both sides are
               ;; pure tests, so evaluating the right side is harmless and
               ;; keeps the parse simple.
               (setf left (or left right) rest more)))
    (values left rest)))

(defun cond-parse-and (sh words)
  (multiple-value-bind (left rest) (cond-parse-unary sh words)
    (loop while (and rest (string= (first rest) "&&"))
          do (multiple-value-bind (right more) (cond-parse-unary sh (rest rest))
               (setf left (and left right) rest more)))
    (values left rest)))

(defun cond-parse-unary (sh words)
  (cond
    ((null words) (values nil nil))
    ((string= (first words) "!")
     (multiple-value-bind (v rest) (cond-parse-unary sh (rest words))
       (values (not v) rest)))
    ((string= (first words) "(")
     (multiple-value-bind (v rest) (cond-parse-or sh (rest words))
       (values v (if (and rest (string= (first rest) ")")) (rest rest) rest))))
    (t (cond-parse-primary sh words))))

(defun cond-parse-primary (sh words)
  (let ((w (first words)))
    (cond
      ;; unary operator
      ((and (member w +cond-unary-ops+ :test #'string=) (rest words))
       (values (cond-unary sh w (cond-word sh (second words)))
               (cddr words)))
      ;; binary operator
      ((and (rest words) (member (second words) +cond-binary-ops+ :test #'string=))
       (values (cond-binary sh (second words) (first words) (third words))
               (cdddr words)))
      ;; a bare word is true when non-empty
      (t (values (plusp (length (cond-word sh w))) (rest words))))))

(defun cond-word (sh raw)
  "Expand one operand: parameter/command/arithmetic expansion and quote
removal, but NO field splitting or globbing -- that is the whole point of the
[[ ]] form."
  (if raw (xchars->string (expand-pass sh raw)) ""))

(defun cond-unary (sh op operand)
  (cond
    ((string= op "-z") (zerop (length operand)))
    ((string= op "-n") (plusp (length operand)))
    ;; -v tests whether the NAME is set, so the operand is a name, not a value
    ((string= op "-v") (nth-value 1 (get-var sh operand)))
    ((string= op "-o") (opt sh (option-keyword operand)))
    (t
     ;; Everything else is a file test; reuse the `test' evaluator so the
     ;; two can never drift apart.
     (handler-case (eval-test (list op operand) sh) (error () nil)))))

(defun option-keyword (name)
  (let ((entry (option-by-name name)))
    (and entry (third entry))))

(defun cond-binary (sh op left right)
  (let ((l (cond-word sh left)))
    (cond
      ;; Pattern match, not string equality: the right side is a glob whose
      ;; metacharacters are live unless they were quoted in the source.
      ((member op '("=" "==") :test #'string=)
       (shell-pattern-match (xchars->pattern (expand-pass sh right)) l))
      ((string= op "!=")
       (not (shell-pattern-match (xchars->pattern (expand-pass sh right)) l)))
      ;; Regex, with the capture groups exposed as $BASH_REMATCH.
      ((string= op "=~")
       (let* ((pattern (cond-regex-operand sh right))
              (m (regex-match pattern l)))
         (when m
           (set-var sh "BASH_REMATCH" (array-from-list m)))
         (and m t)))
      ;; String ordering. bash compares by the current locale; we use the
      ;; code-point order, which agrees for the C locale.
      ((string= op "<") (string< l (cond-word sh right)))
      ((string= op ">") (string> l (cond-word sh right)))
      ;; The numeric operators evaluate their operands as ARITHMETIC here,
      ;; which `[ ]' does not: `e=1+2; [[ e -eq 3 ]]' is true, while
      ;; `[ e -eq 3 ]' is an "integer expression expected" error.
      ((member op '("-eq" "-ne" "-lt" "-le" "-gt" "-ge") :test #'string=)
       (let ((a (ignore-errors (eval-arith sh l)))
             (b (ignore-errors (eval-arith sh (cond-word sh right)))))
         (and a b
              (cond ((string= op "-eq") (= a b))
                    ((string= op "-ne") (/= a b))
                    ((string= op "-lt") (< a b))
                    ((string= op "-le") (<= a b))
                    ((string= op "-gt") (> a b))
                    ((string= op "-ge") (>= a b))))))
      ;; File comparisons: hand to the `test' evaluator.
      (t (handler-case (eval-test (list l op (cond-word sh right)))
           (error () nil))))))

(defun cond-regex-operand (sh raw)
  "Expand the right side of =~ WITHOUT quote removal of regex metacharacters.

A quoted section is matched literally -- `[[ $x =~ \"a.c\" ]]' wants a real
dot -- so quoted characters that mean something to the engine are escaped
rather than dropped."
  (with-output-to-string (out)
    (dolist (xc (expand-pass sh raw))
      (case (xchar-class xc)
        ((:anchor :field-sep))
        (t (let ((c (xchar-char xc)))
             (when (and (eq (xchar-class xc) :quoted)
                        (find c ".*+?[]()|^$\\{}"))
               (write-char #\\ out))
             (write-char c out)))))))
