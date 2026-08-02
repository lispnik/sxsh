;;;; shell/jobs.lisp --- job control: job table, process groups, terminal.
;;;;
;;;; A JOB groups the process(es) started by one asynchronous (or suspended)
;;;; command into a single unit the user can refer to as %n. Each job runs in
;;;; its own process group (the pgid equals the first process's pid), created
;;;; via posix_spawn's SETPGROUP attribute, so signals and terminal control
;;;; apply to the whole job at once.

(in-package #:sxsh-shell)

(defstruct job
  id                                    ; %n job number
  pgid                                  ; process-group id (== leader pid)
  pids                                  ; list of member pids, in pipeline order
  state                                 ; :running :stopped :done
  status                                ; final shell exit status once :done
  command                               ; source text, for `jobs` display
  notified                              ; t once we've reported :done/:stopped
  reaped)                               ; members already collected by waitpid

;;; ---------------------------------------------------------------------------
;;; Terminal control FFI (tcsetpgrp / tcgetpgrp are not in sb-posix)
;;; ---------------------------------------------------------------------------

(sb-alien:define-alien-routine ("tcsetpgrp" %tcsetpgrp) sb-alien:int
  (fd sb-alien:int) (pgrp sb-alien:int))
(sb-alien:define-alien-routine ("tcgetpgrp" %tcgetpgrp) sb-alien:int
  (fd sb-alien:int))
(sb-alien:define-alien-routine ("getpgrp" %getpgrp) sb-alien:int)
(sb-alien:define-alien-routine ("killpg" %killpg) sb-alien:int
  (pgrp sb-alien:int) (sig sb-alien:int))

(defvar *tty-fd* nil
  "Private fd on the controlling terminal, opened lazily and kept for the
life of the shell.")

(defconstant +fd-cloexec+ 1)  ; FD_CLOEXEC: POSIX fixes this at 1 everywhere

(defun tty-fd ()
  "An fd referring to the controlling terminal.

This must be a fresh handle on /dev/tty rather than stderr: the shell's own
fd 2 can be redirected (`fg 2>/dev/null`, or any job-control builtin inside a
redirected compound), and terminal-ownership calls made on a redirected fd
either fail or, worse, act on the wrong terminal. Falls back to fd 2 when
there is no controlling terminal to open. The fd is close-on-exec so children
never inherit it."
  (or *tty-fd*
      (setf *tty-fd*
            (handler-case
                (let ((fd (sb-posix:open "/dev/tty" sb-posix:o-rdwr)))
                  (ignore-errors
                   (sb-posix:fcntl fd sb-posix:f-setfd +fd-cloexec+))
                  fd)
              (error () 2)))))

(defun tty-set-pgrp (pgrp)
  "Give terminal control to process group PGRP. Silently ignore failures (e.g.
when not attached to a tty)."
  (ignore-errors (%tcsetpgrp (tty-fd) pgrp)))

(defun tty-get-pgrp ()
  (let ((r (ignore-errors (%tcgetpgrp (tty-fd)))))
    (and r (>= r 0) r)))

(defun have-tty-p ()
  "True if we have a controlling terminal (so job control makes sense)."
  (handler-case (progn (sb-posix:tcgetattr (tty-fd)) t)
    (error () nil)))

;;; ---------------------------------------------------------------------------
;;; Monitor mode (set -m) -- whether job control is active
;;; ---------------------------------------------------------------------------

(defun enable-job-control (sh)
  "Take over job control: ignore the job-control signals in the shell itself,
record our own process group, and claim the terminal. Returns T on success,
NIL when there is no controlling terminal to claim.

The three dispositions MUST come from sb-unix rather than literals -- the
job-control signal numbers differ between Linux and macOS, and on macOS the
literal for SIGTSTP is in fact SIGCHLD. Ignoring SIGCHLD tells the kernel to
auto-reap children, after which every waitpid fails with ECHILD and no job
ever leaves the :running state. Children get these reset to SIG_DFL via
CHILD-SIGDEFAULTS, since SIG_IGN would otherwise be inherited across exec."
  (when (have-tty-p)
    (ignore-errors (sb-sys:enable-interrupt sb-unix:sigttou :ignore))
    (ignore-errors (sb-sys:enable-interrupt sb-unix:sigttin :ignore))
    (ignore-errors (sb-sys:enable-interrupt sb-unix:sigtstp :ignore))
    (install-interrupt-handlers)
    (let ((pgid (ignore-errors (sb-posix:getpgid 0))))
      (when pgid
        (setf (shell-pgid sh) pgid
              (shell-job-control sh) t)
        (tty-set-pgrp pgid)
        t))))

(defun install-interrupt-handlers ()
  "Stop Ctrl-C / Ctrl-\\ at the prompt from killing an interactive shell.
POSIX requires an interactive shell to survive SIGINT and return to the
prompt; with the default disposition the shell is simply terminated.

Deliberately HANDLERS rather than SIG_IGN. exec resets handled signals to
SIG_DFL automatically, so children stay interruptible with no SETSIGDEF
bookkeeping -- whereas SIG_IGN is inherited and would make every child immune
to Ctrl-C. (SIGTSTP/TTIN/TTOU still need SIG_IGN: a handler there would make
tcsetpgrp fail with EINTR rather than succeed, so those do go through
CHILD-SIGDEFAULTS.)"
  (dolist (num (list sb-unix:sigint sb-unix:sigquit))
    (ignore-errors
     (sb-sys:enable-interrupt
      num
      (lambda (signo info context)
        (declare (ignore info context))
        ;; Still a HANDLER rather than SIG_IGN, for the reason above. The flag
        ;; is what lets the line editor tell "the user pressed Ctrl-C" apart
        ;; from "the read failed": without it the editor sees only EINTR and
        ;; cannot discard the partly typed line.
        (when (= signo sb-unix:sigint)
          (setf *sigint-pending* t))
        nil)))))

(defun disable-job-control (sh)
  "Stand down from job control: give the terminal back to ourselves and restore
default dispositions for the job-control signals."
  (when (and (shell-job-control sh) (shell-pgid sh))
    (tty-set-pgrp (shell-pgid sh)))
  (dolist (num (list sb-unix:sigttou sb-unix:sigttin sb-unix:sigtstp
                     sb-unix:sigint sb-unix:sigquit))
    (ignore-errors (sb-sys:enable-interrupt num :default)))
  (setf (shell-job-control sh) nil))

(defun set-monitor (sh enable)
  "Implement `set -m` / `set +m`. POSIX defaults it on for interactive shells
and off otherwise; enabling it in a shell with no controlling terminal records
the option but leaves job control inactive."
  (setf (opt sh :monitor) (and enable t))
  (if enable
      (enable-job-control sh)
      (disable-job-control sh))
  (opt sh :monitor))

;;; ---------------------------------------------------------------------------
;;; Job table management
;;; ---------------------------------------------------------------------------

(defun add-job (sh pgid pids command &key (state :running))
  "Register a new job, returning it. Assigns the next job number."
  (let ((job (make-job :id (incf (shell-job-counter sh))
                       :pgid pgid :pids (copy-list pids)
                       :state state :command command)))
    (push job (shell-jobs sh))
    ;; Only a job that really has processes sets $!. A bookkeeping-only job
    ;; (one recorded for something that ran in-process) must not blank out the
    ;; pid of the last genuine background command.
    (when pids
      (setf (shell-last-bg-pid sh) (car (last pids))))
    job))

(defun find-job (sh spec)
  "Resolve a job spec to a JOB. SPEC may be an integer job id, a string like
\"%1\", \"%%\"/\"%+\" (current), \"%-\" (previous), or a pid."
  (let ((jobs (shell-jobs sh)))
    (cond
      ((null jobs) nil)
      ((null spec) (first jobs))                 ; current
      ((integerp spec) (find spec jobs :key #'job-id))
      ((stringp spec)
       (cond
         ((or (string= spec "%%") (string= spec "%+") (string= spec "%"))
          (first jobs))
         ((string= spec "%-") (second jobs))
         ((and (> (length spec) 1) (char= (char spec 0) #\%))
          (let ((n (ignore-errors (parse-integer spec :start 1))))
            (if n (find n jobs :key #'job-id)
                ;; %string : match by command prefix
                (find-if (lambda (j) (prefix-p (subseq spec 1) (job-command j)))
                         jobs))))
         ;; A bare integer is a JOB NUMBER first (`fg 1` == `fg %1`, as in every
         ;; other shell); only if no such job exists do we read it as a pid.
         (t (let ((n (ignore-errors (parse-integer spec))))
              (when n
                (or (find n jobs :key #'job-id)
                    (find-if (lambda (j) (member n (job-pids j))) jobs)))))))
      (t nil))))

(defun prefix-p (pre str)
  (and (<= (length pre) (length str))
       (string= pre str :end2 (length pre))))

(defun remove-job (sh job)
  (setf (shell-jobs sh) (remove job (shell-jobs sh))))

;;; ---------------------------------------------------------------------------
;;; Reaping: poll for state changes without blocking (WNOHANG|WUNTRACED)
;;; ---------------------------------------------------------------------------

(defconstant +wnohang+   1)
(defconstant +wuntraced+ 2)
;; WCONTINUED is one of the wait flags that does NOT agree across platforms:
;; 8 on Linux, 0x10 on macOS. (WNOHANG and WUNTRACED happen to be 1 and 2 on
;; both.) Getting this wrong makes waitpid reject the call with EINVAL.
(defconstant +wcontinued+ #+darwin 16 #-darwin 8)

(defun poll-jobs (sh)
  "Non-blocking check for job state changes; update the table. Returns nil."
  (dolist (job (shell-jobs sh))
    (case (job-state job)
      (:running (update-job-state sh job +wnohang+))
      ;; A stopped job can be resumed behind our back (`kill -CONT %1`, or a
      ;; SIGCONT from anywhere else); ask for WCONTINUED so the table notices.
      (:stopped (update-job-state sh job (logior +wnohang+ +wcontinued+))))))

(defun update-job-state (sh job flags)
  "waitpid each not-yet-reaped pid of JOB with FLAGS; update its state/status.

A member is only waited for once. A pid we already collected on an earlier
poll -- or one waitpid rejects with ECHILD because it is gone -- counts as
finished, not as still-running; treating ECHILD as \"running\" is what used to
leave multi-process pipelines stuck at :running forever."
  (let ((all-done t) (stopped nil) (continued nil)
        (last-pid (car (last (job-pids job)))))
    (dolist (pid (job-pids job))
      (unless (member pid (job-reaped job))
        (multiple-value-bind (wpid status)
            (handler-case (sb-posix:waitpid pid (logior flags +wuntraced+))
              (error () (values :gone 0)))    ; ECHILD: already gone
          (cond
            ((eq wpid :gone) (pushnew pid (job-reaped job)))
            ((zerop wpid) (setf all-done nil))  ; still running, not reaped
            ;; This test MUST precede the stopped one: on macOS a "continued"
            ;; status also carries 0x7f in its low byte, so checking stopped
            ;; first would read a just-resumed job as stopped.
            ((wait-continued-p status) (setf continued t all-done nil))
            ((wait-stopped-p status) (setf stopped t all-done nil))
            (t
             (pushnew pid (job-reaped job))
             ;; POSIX: a pipeline's status is that of its LAST stage. job-pids
             ;; is in pipeline order, so only that member sets job-status.
             (when (eql pid last-pid)
               (setf (job-status job) (decode-wait-status status))))))))
    (cond
      (stopped                       (setf (job-state job) :stopped))
      ((and all-done (not continued)) (setf (job-state job) :done))
      (continued                     (setf (job-state job) :running)))))

(defun wait-stopped-p (status)
  "True if a wait status indicates the child stopped (0x7f in low byte)."
  (= (logand status #xff) #x7f))

(defun wait-continued-p (status)
  "True if a wait status reports the child was resumed by SIGCONT.

The encoding is platform-specific: Linux uses the sentinel 0xffff, while macOS
reuses the stopped encoding with SIGCONT in the signal byte."
  #+darwin (and (= (logand status #xff) #x7f)
                (= (ash status -8) sb-unix:sigcont))
  #-darwin (= status #xffff))

;;; ---------------------------------------------------------------------------
;;; Notification / `jobs` display
;;; ---------------------------------------------------------------------------

(defun job-state-label (state)
  (ecase state (:running "Running") (:stopped "Stopped") (:done "Done")))

(defun print-job (sh job stream &key show-pgid)
  (let* ((jobs (shell-jobs sh))
         (marker (cond ((eq job (first jobs)) "+")
                       ((eq job (second jobs)) "-")
                       (t " "))))
    (if show-pgid
        (format stream "[~D]~A ~D ~A~28T~A~%"
                (job-id job) marker (job-pgid job)
                (job-state-label (job-state job)) (job-command job))
        (format stream "[~D]~A  ~A~28T~A~%"
                (job-id job) marker
                (job-state-label (job-state job)) (job-command job)))))

(defun notify-finished-jobs (sh stream)
  "Report and remove jobs that have finished since last check (interactive)."
  (dolist (job (reverse (shell-jobs sh)))
    (when (and (eq (job-state job) :done) (not (job-notified job)))
      (setf (job-notified job) t)
      (print-job sh job stream)
      (remove-job sh job))))

;;; ---------------------------------------------------------------------------
;;; Foreground / background transitions
;;; ---------------------------------------------------------------------------

(defun wait-for-job (sh job)
  "Block until JOB finishes (or a member stops); update and (if done) remove
it. Returns the job's shell exit status."
  (let ((status (or (job-status job) 0))
        (stopped nil)
        (last-pid (car (last (job-pids job)))))
    (dolist (pid (job-pids job))
      (unless (member pid (job-reaped job))
        (multiple-value-bind (wpid st)
            (handler-case (sb-posix:waitpid pid +wuntraced+)
              (error () (values :gone 0)))
          (cond
            ((eq wpid :gone) (pushnew pid (job-reaped job)))
            ((wait-stopped-p st) (setf stopped t))
            (t (pushnew pid (job-reaped job))
               (when (eql pid last-pid)
                 (setf status (decode-wait-status st))))))))
    (cond
      (stopped (setf (job-state job) :stopped)
               (when (shell-interactive sh)
                 (format *error-output* "~%[~D]+ Stopped~28T~A~%"
                         (job-id job) (job-command job))))
      (t (setf (job-state job) :done (job-status job) status)
         (remove-job sh job)))
    status))

(defun continue-job (job)
  "Send SIGCONT to a stopped job's process group."
  (when (job-pgid job)
    ;; sb-unix:sigcont, never a literal: SIGCONT is 18 on Linux but 19 on
    ;; macOS, where 18 is SIGTSTP -- the old literal stopped the job again
    ;; instead of resuming it.
    (ignore-errors (%killpg (job-pgid job) sb-unix:sigcont))))

(defun fg-give-terminal (sh job)
  "Hand the controlling terminal to JOB's process group (interactive only)."
  (when (and (shell-job-control sh) (job-pgid job))
    (tty-set-pgrp (job-pgid job))))

(defun fg-reclaim-terminal (sh)
  "Return terminal control to the shell after a foreground job finishes."
  (when (and (shell-job-control sh) (shell-pgid sh))
    (tty-set-pgrp (shell-pgid sh))))
