;;;; shell/exec.lisp --- execute the parsed AST.
;;;;
;;;; External commands are launched with posix_spawnp (see spawn.lisp); no
;;;; fork/exec. Builtins, functions, and compound commands run in-process.
;;;; Pipelines wire children together with pipe(2) and per-stage file actions.

(in-package #:posh-shell)

;;; ---------------------------------------------------------------------------
;;; PATH search
;;; ---------------------------------------------------------------------------

(defun find-in-path (sh name &key allow-slash)
  "Resolve NAME to an executable path. If NAME contains a slash, use it as-is."
  (if (find #\/ name)
      (and (or allow-slash t) name)
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
      (dolist (node ast-list (shell-last-status sh))
        (exec-node sh node))
    (shell-exit (e)
      (setf (shell-last-status sh) (or (shell-exit-code e) 0))
      (shell-last-status sh))))

(defun run-string (sh src)
  (run sh (parse-string src)))

(defun run-trap (sh condition-name)
  "Run the trap action registered for CONDITION-NAME (already normalized), if
any. Errors in the trap are contained."
  (multiple-value-bind (action found) (gethash condition-name (shell-traps sh))
    (when (and found (plusp (length action)))
      (handler-case (run sh (parse-string action))
        (shell-exit (e) (setf (shell-last-status sh) (or (shell-exit-code e) 0)))
        (error () nil)))))

(defun run-exit-traps (sh)
  "Execute the EXIT trap, if set. Called once when the shell terminates.

The shell's exit status is the one already in effect when termination began;
commands run by the trap must not change it. Only an explicit `exit N' inside
the trap overrides it. Without this, `trap \"echo done\" EXIT; exit 3' exited 0,
because the trap's own `echo' overwrote the 3."
  (multiple-value-bind (action found) (gethash "EXIT" (shell-traps sh))
    (when (and found (plusp (length action)))
      (let ((saved (shell-last-status sh))
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
  (let ((line (posh::node-line node)))
    (when (plusp line)
      (setf (gethash "LINENO" (shell-vars sh))
            (cons (princ-to-string line) nil))))
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
                (:func     (exec-func-def sh node)))
            (shell-unset-var (e)
              (format *error-output* "posh: ~A~%" e)
              ;; set -u in a non-interactive shell is fatal
              (unless (shell-interactive sh)
                (signal 'shell-exit :code 1))
              1)
            (readonly-violation (e)
              (format *error-output* "posh: ~A~%" e)
              1))))
    (setf (shell-last-status sh) status)
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
            (handler-case (run sh (parse-string action))
              (shell-exit (e)
                (setf (shell-last-status sh) (or (shell-exit-code e) 0))
                (signal 'shell-exit :code (shell-last-status sh)))
              (error () nil))))))))

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
  (let ((n (length cmds)) (stages '()) (prev-read nil))
    ;; STAGES accumulates, in order, one entry per command:
    ;;   (:pid . PID)     for a spawned external stage
    ;;   (:status . CODE) for a builtin/compound run in-process
    (unwind-protect
         (loop for cmd in cmds for i from 0 do
           (let* ((last (= i (1- n)))
                  (rw (unless last (multiple-value-list (sb-posix:pipe)))))
             (let ((read-fd (first rw)) (write-fd (second rw)))
               (let ((stdin (or prev-read 0))
                     (stdout (if last 1 write-fd)))
                 (multiple-value-bind (pid inproc-status)
                     (spawn-stage sh cmd stdin stdout
                                  (remove nil (list prev-read read-fd write-fd)))
                   (if pid
                       (push (cons :pid pid) stages)
                       (push (cons :status inproc-status) stages))))
               ;; parent closes fds it no longer needs
               (when prev-read (sb-posix:close prev-read))
               (when write-fd (sb-posix:close write-fd))
               (setf prev-read read-fd))))
      (when prev-read (ignore-errors (sb-posix:close prev-read))))
    (setf stages (nreverse stages))
    ;; Reap every spawned child so none are left as zombies; remember the
    ;; status of the FINAL stage specifically -- that is the pipeline's status.
    (let ((final 0) (last-index (1- n)))
      (loop for stage in stages for i from 0 do
        (ecase (car stage)
          (:pid (let ((st (wait-for (cdr stage))))
                  (when (= i last-index) (setf final st))))
          (:status (when (= i last-index) (setf final (cdr stage))))))
      final)))

(defun spawn-stage (sh cmd stdin stdout close-in-child)
  "Run one pipeline stage. Returns (values pid nil) for external commands, or
(values nil status) if the stage was a builtin/compound run in-process.
STDIN/STDOUT are fds to attach. CLOSE-IN-CHILD lists pipe fds to close."
  (multiple-value-bind (externalp words) (external-simple-command-p sh cmd)
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
        (if (eql pid 127)
            (values nil 127)            ; lookup failed: no child to wait for
            (values pid nil)))
      ;; builtin / compound: run in-process with fds redirected temporarily
      (let ((saved-in (sb-posix:dup 0)) (saved-out (sb-posix:dup 1)))
        (unwind-protect
             (progn
               (unless (= stdin 0) (sb-posix:dup2 stdin 0))
               (unless (= stdout 1) (sb-posix:dup2 stdout 1))
               (values nil (exec-node sh cmd)))
          (sb-posix:dup2 saved-in 0) (sb-posix:close saved-in)
          (sb-posix:dup2 saved-out 1) (sb-posix:close saved-out))))))

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

(defparameter +declaration-utilities+ '("export" "readonly")
  "Utilities whose `name=value' operands are expanded as assignments.

POSIX 2.9.1: for these, a word of that form is treated the way a real
assignment would be, so the tilde after `=' (and after each unquoted `:')
expands. Without this `export PATH=~/bin' exported a literal ~.")

(defun assignment-operand-p (raw)
  "True if RAW has the shape name=value."
  (let ((eq (position #\= raw)))
    (and eq (plusp eq)
         (let ((name (subseq raw 0 eq)))
           (and (or (alpha-char-p (char name 0)) (char= (char name 0) #\_))
                (every (lambda (c) (or (alphanumericp c) (char= c #\_)))
                       name))))))

(defun expand-command-words (sh cmd)
  "Expand the word list of a simple command into argv (no assignments).
Applies alias substitution to the command name (first word)."
  (let ((argv '()) (declaration nil) (first t))
    (dolist (w (simple-command-words cmd))
      (let* ((raw (word-text w))
             (as-assignment (and declaration (assignment-operand-p raw)))
             (fields (expand-word-to-fields
                      sh raw
                      ;; Operands of export/readonly are expanded exactly like
                      ;; a real assignment: tilde after = and :, and NO field
                      ;; splitting or pathname expansion -- `export x=$v' with
                      ;; v="a b" must export the whole value, not just "a".
                      :assignment as-assignment
                      :split (not as-assignment)
                      :glob (not as-assignment))))
        (when first
          (setf declaration (and (member (first fields) +declaration-utilities+
                                         :test #'string=)
                                 t)
                first nil))
        (dolist (f fields) (push f argv))))
    (setf argv (nreverse argv))
    (apply-alias sh argv)))

(defun apply-alias (sh argv)
  "If the first word of ARGV is an alias, replace it with the alias body's
words (split on whitespace). Guards against self-referential aliases."
  (if (null argv)
      argv
      (let ((seen '()) (result argv))
        (loop
          (let ((head (first result)))
            (multiple-value-bind (body found) (gethash head (shell-aliases sh))
              (if (and found (not (member head seen :test #'string=)))
                  (progn
                    (push head seen)
                    ;; naive split of the alias body on spaces/tabs
                    (let ((parts (remove "" (split-string-ws body) :test #'string=)))
                      (setf result (append parts (rest result)))))
                  (return result))))))))

(defun split-string-ws (s)
  (let ((out '()) (start 0) (n (length s)))
    (dotimes (i n)
      (when (member (char s i) '(#\Space #\Tab))
        (push (subseq s start i) out) (setf start (1+ i))))
    (push (subseq s start) out)
    (nreverse out)))

(defun exec-simple (sh node)
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
      ;; function call
      ((gethash (first words) (shell-functions sh))
       (call-function sh (gethash (first words) (shell-functions sh)) words node))
      ;; builtin
      ((builtin-p (first words))
       (run-builtin sh (first words) (rest words) node))
      ;; external via posix_spawn
      (t
       (catch 'not-found
         (if (shell-job-control sh)
             ;; job control: put the child in its own group, give it the
             ;; terminal, wait, then reclaim -- so Ctrl-Z/Ctrl-C hit the child.
             (let ((pid (spawn-external sh node :words words
                                               :setpgroup t :pgroup 0)))
               (return-from exec-simple
                 (wait-foreground sh pid (ignore-errors (deparse node)))))
             (let ((pid (spawn-external sh node :words words)))
               (return-from exec-simple (wait-for pid)))))
       127))))

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
  (if (null exec-words)
      ;; permanent redirections: apply and do NOT restore
      (progn
        (multiple-value-bind (saved temps)
            (apply-redirects-in-process sh (simple-command-redirects node))
          (declare (ignore saved))     ; intentionally not restored
          (dolist (p temps) (ignore-errors (delete-file p))))
        0)
      ;; exec a command: spawn it, and on success replace the shell (exit with
      ;; its status). A failure to find the command is fatal to the shell.
      (let ((prog (find-in-path sh (first exec-words))))
        (if (null prog)
            (progn (format *error-output* "exec: ~A: not found~%" (first exec-words))
                   (signal 'shell-exit :code 127) 127)
            (let ((pid (spawn-external-words sh node exec-words)))
              (let ((status (wait-for pid)))
                (signal 'shell-exit :code status)
                status))))))

(defun spawn-external-words (sh node argv-words)
  "Like spawn-external but with an explicit ARGV-WORDS list (already expanded),
reusing NODE's assignments and redirections."
  (let* ((prog (or (find-in-path sh (first argv-words))
                   (progn (report-not-found sh node (first argv-words))
                          (throw 'not-found 127))))
         (temp-env (apply-assignments sh (simple-command-assignments node)))
         (env (merge-env (exported-environ sh) temp-env)))
    (multiple-value-bind (redir-actions temps)
        (build-spawn-file-actions sh (simple-command-redirects node))
      (unwind-protect
           (spawn prog argv-words :env env :file-actions redir-actions
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

(defun apply-assignments (sh assignments &key to-shell)
  "Evaluate NAME=VALUE assignments. When TO-SHELL, set in the shell (persist);
otherwise return a list of temporary K=V for a command environment."
  (let ((temp '()) (saved '()))
    (unwind-protect
         (dolist (a assignments)
           (let* ((name (assignment-name a))
                  (vw (assignment-value a))
                  (val (if vw
                           (first (expand-word-to-fields sh (word-text vw)
                                                         :split nil :glob nil
                                                         :assignment t))
                           "")))
             (setf val (or val ""))
             (cond
               (to-shell (set-var sh name val))
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

(defun run-builtin (sh name args node)
  "Run a builtin with redirections applied in-process."
  (apply-assignments sh (simple-command-assignments node) :to-shell t)
  (let (saved temps)
    (handler-case
        (multiple-value-setq (saved temps)
          (apply-redirects-in-process sh (simple-command-redirects node)))
      ;; A redirection we cannot set up fails this command with status 1
      ;; rather than aborting the shell.
      (redirect-error (e)
        (format *error-output* "posh: ~A~%" e)
        (return-from run-builtin 1)))
    (unwind-protect
         (handler-case
             (funcall (find-builtin name) sh args *standard-output*)
           (func-return (e) (cf-code e)))
      (finish-output *standard-output*)
      (restore-redirects saved)
      (dolist (p temps) (ignore-errors (delete-file p))))))

(defun child-sigdefaults (sh)
  "Signal numbers that must be reset to SIG_DFL in a spawned child.

The shell sets SIGTSTP/SIGTTIN/SIGTTOU to SIG_IGN for its own job-control
machinery, and SIG_IGN survives exec -- so without this a child would ignore
Ctrl-Z and could never be stopped. A signal the user explicitly ignored with
`trap '' SIG` is deliberately left out: POSIX requires that disposition to be
inherited by children."
  (when (shell-job-control sh)
    (loop for (name . num) in `(("TSTP" . ,sb-unix:sigtstp)
                                ("TTIN" . ,sb-unix:sigttin)
                                ("TTOU" . ,sb-unix:sigttou))
          unless (equal "" (gethash name (shell-traps sh)))
            collect num)))

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
                   (progn (report-not-found sh node (first words))
                          (return-from spawn-external
                            (signal-not-found)))))
         (temp-env (apply-assignments sh (simple-command-assignments node)))
         (env (merge-env (exported-environ sh) temp-env)))
    (multiple-value-bind (redir-actions temps)
        (build-spawn-file-actions sh (simple-command-redirects node))
      (let ((all-actions (append extra-actions redir-actions)))
        (unwind-protect
             (spawn prog words :env env :file-actions all-actions
                               :setpgroup setpgroup :pgroup pgroup
                               :sigdefault (child-sigdefaults sh))
          (dolist (p temps) (ignore-errors (delete-file p))))))))

(defun signal-not-found ()
  ;; a sentinel: caller treats a NIL pid as status 127; but our callers expect a
  ;; pid to wait on. Instead we throw to a handler in exec-simple.
  (throw 'not-found 127))

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

(defun call-function (sh def words node)
  "Invoke a shell function. Positional params become the call args; $0 stays."
  (let ((saved-pos (shell-positional sh)))
    (apply-assignments sh (simple-command-assignments node) :to-shell t)
    (set-positional sh (rest words))
    (multiple-value-bind (saved temps)
        (apply-redirects-in-process sh (simple-command-redirects node))
      (unwind-protect
           (handler-case (exec-node sh (function-def-body def))
             (func-return (e) (cf-code e)))
        (restore-redirects saved)
        (dolist (p temps) (ignore-errors (delete-file p)))
        (setf (shell-positional sh) saved-pos)))))

;;; ---------------------------------------------------------------------------
;;; Compound commands
;;; ---------------------------------------------------------------------------

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
     (format *error-output* "~A: command not found~%" name)
     (finish-output *error-output*)))
  (setf (shell-last-status sh) 127))

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
            (format *error-output* "posh: ~A~%" e)
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
            (handler-case (exec-node sh (subshell-body node))
              (shell-exit (e) (or (shell-exit-code e) 0)))
         (restore-shell sh snap))))))

(defun snapshot-shell (sh)
  (list (alexandria-copy-hash (shell-vars sh))
        (alexandria-copy-hash (shell-functions sh))
        (copy-seq (shell-positional sh))
        (ignore-errors (current-directory))))

(defun restore-shell (sh snap)
  (destructuring-bind (vars funcs pos cwd) snap
    (setf (shell-vars sh) vars
          (shell-functions sh) funcs
          (shell-positional sh) pos)
    (when cwd (ignore-errors (change-directory cwd)))))

(defun alexandria-copy-hash (ht)
  (let ((new (make-hash-table :test (hash-table-test ht))))
    (maphash (lambda (k v) (setf (gethash k new) v)) ht)
    new))

(defun exec-if (sh node)
  (if (zerop (without-errexit (exec-node sh (if-clause-condition node))))
      (exec-node sh (if-clause-then node))
      (let ((else (if-clause-else node)))
        (if else (exec-node sh else) 0))))

(defun exec-while (sh node until-p)
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
      (loop-break (e) (when (> (cf-n e) 1)
                        (signal 'loop-break :n (1- (cf-n e))))))
    status))

(defun exec-for (sh node)
  (let* ((name (for-clause-name node))
         (words-spec (for-clause-words node))
         (items (if (eq words-spec :default)
                    (coerce (shell-positional sh) 'list)
                    (loop for w in words-spec
                          append (expand-word-to-fields sh (word-text w)))))
         (status 0))
    (handler-case
        (dolist (item items)
          (run-pending-traps sh)
          (set-var sh name item)
          (handler-case
              (setf status (exec-node sh (for-clause-body node)))
            (loop-continue (e) (when (> (cf-n e) 1)
                                 (signal 'loop-continue :n (1- (cf-n e)))))))
      (loop-break (e) (when (> (cf-n e) 1)
                        (signal 'loop-break :n (1- (cf-n e))))))
    status))

(defun exec-case (sh node)
  (let ((word (first (expand-word-to-fields sh (word-text (case-clause-word node))
                                            :split nil :glob nil)))
        (status 0))
    (setf word (or word ""))
    (dolist (item (case-clause-items node) status)
      (when (some (lambda (pat)
                    (let ((p (first (expand-word-to-fields
                                     sh (word-text pat) :split nil :glob nil))))
                      (shell-pattern-match (or p "") word)))
                  (case-item-patterns item))
        (return (if (case-item-body item)
                    (exec-node sh (case-item-body item))
                    0))))))

;;; ---------------------------------------------------------------------------
;;; Background execution & waiting
;;; ---------------------------------------------------------------------------

(defun exec-async (sh node)
  "Run NODE in the background as a job, returning 0.

External simple commands and pipelines are spawned directly into their own
process group. Everything else -- builtins, functions, and compound commands --
becomes a genuine asynchronous job by re-executing this posh binary with `-c`
on the deparsed source (see ASYNC-COMPOUND); we cannot fork, so re-exec is the
only way to get a second process. If no re-exec is possible the command falls
back to running in-process, synchronously."
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

(defconstant +max-async-source+ 100000
  "Ceiling on the generated `-c` source. argv+env is bounded (ARG_MAX), and a
shell with a very large variable table could otherwise overflow it; past this
we run synchronously instead of failing to spawn.")

(defun self-exec-path ()
  "Path to this posh executable, or NIL if we are not a saved image.

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
  "Shell source reproducing enough of SH's state for a fresh posh to run a
background command faithfully: cwd, shell (non-exported) variables, function
definitions, and positional parameters. Exported variables need no prelude --
they travel in the child's environment."
  (with-output-to-string (s)
    (let ((cwd (ignore-errors (current-directory))))
      (when cwd (format s "cd ~A~%" (shell-quote cwd))))
    (maphash (lambda (k cell)
               (when (and (not (cdr cell))         ; not exported
                          (assignable-name-p k))
                 (format s "~A=~A~%" k (shell-quote (car cell)))))
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
            (let ((pid (spawn self (list "posh" "-c" src)
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

(defun command-substitute (sh src)
  "Run SRC, capture its stdout, strip trailing newlines. Captures through a
temp file so large output can't deadlock on a bounded pipe buffer."
  (let ((ast (handler-case (parse-string src)
               (error () (return-from command-substitute "")))))
    (let* ((path (format nil "/tmp/posh-cmdsub-~A-~A"
                         (sb-posix:getpid) (random 1000000)))
           (fd (sb-posix:open path (logior +o-wronly+ +o-creat+ +o-trunc+) #o600))
           (saved-out (sb-posix:dup 1)))
      (unwind-protect
           (progn
             (sb-posix:dup2 fd 1)
             (sb-posix:close fd)
             (let ((*standard-output*
                     (sb-sys:make-fd-stream 1 :output t :buffering :full)))
               (unwind-protect
                    (handler-case (run sh ast) (shell-exit () nil))
                 (finish-output *standard-output*))
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
