;;;; shell/state.lisp --- shell runtime state.

(in-package #:sxsh-shell)

(defstruct (shell (:constructor %make-shell))
  ;; variable table: name -> (value . exported-p)
  (vars (make-hash-table :test 'equal))
  ;; shell functions: name -> function-def AST node
  (functions (make-hash-table :test 'equal))
  ;; positional parameters $1 $2 ... as a vector of strings
  (positional #() :type vector)
  ;; $0
  (name "sxsh")
  ;; $? of the last command
  (last-status 0 :type integer)
  ;; $$ (our pid)
  (pid 0)
  ;; $! most recent background pid
  (last-bg-pid nil)
  ;; exit status of the most recent command substitution performed while
  ;; expanding the current simple command's words/assignments; NIL if none.
  ;; POSIX: a command consisting only of assignments takes its status from
  ;; the last command substitution in the assignments.
  (last-cmdsub-status nil)
  ;; set -e, set -x, etc.
  (options (make-hash-table :test 'eq))
  ;; whether we are interactive
  (interactive nil)
  ;; readonly variable names (set of strings)
  (readonly (make-hash-table :test 'equal))
  ;; aliases: name -> replacement string
  (aliases (make-hash-table :test 'equal))
  ;; traps: condition/signal name -> action string (e.g. "EXIT" -> "cleanup")
  (traps (make-hash-table :test 'equal))
  ;; all background pids started, for `wait` with no arguments
  (bg-pids '())
  ;; job-control table: list of JOB structs (most-recent first)
  (jobs '())
  ;; monotonically increasing job number counter
  (job-counter 0)
  ;; the shell's own process-group id (for restoring terminal control)
  (pgid nil)
  ;; whether job control is active (interactive + controlling tty)
  (job-control nil)
  ;; the logical working directory, tracked by `cd' and exported as $PWD.
  ;; Kept as shell state rather than read back out of $PWD so that assigning
  ;; to PWD cannot make `pwd' report a directory we are not in.
  (logical-cwd nil)
  ;; signals received since the last trap check (list of signal-name strings),
  ;; to be handled between commands
  (pending-signals '())
  ;; control-flow signals for break/continue/return handled via catch tags
  )

(defvar *trap-entry-status* nil
  "The exit status in effect when the current trap action began, or NIL.

POSIX `exit': \"When exit is executed in a trap action, the last command is
considered to be the command that executed immediately preceding the trap
action.\" So a bare `exit' inside a trap reports the status the shell was
terminating with, not that of the trap's own last command. autoconf relies on
this -- its EXIT trap ends in a bare `exit', and without it every failing
configure run reported success.")

(define-condition shell-exit (condition)
  ((code :initarg :code :reader shell-exit-code :initform 0)))

(define-condition shell-unset-var (error)
  ((name :initarg :name :reader shell-unset-var-name))
  (:report (lambda (c s)
             (format s "~A: parameter not set" (shell-unset-var-name c)))))

(defun make-shell (&key (interactive nil))
  (let ((sh (%make-shell :interactive interactive
                         :pid (sb-posix:getpid))))
    ;; seed variables from the real environment, marked exported
    (dolist (kv (sb-ext:posix-environ))
      (let ((eq (position #\= kv)))
        (when eq
          (setf (gethash (subseq kv 0 eq) (shell-vars sh))
                (cons (subseq kv (1+ eq)) t)))))
    ;; sensible defaults
    (unless (nth-value 1 (get-var sh "IFS"))
      (set-var sh "IFS" (format nil " ~C~C" #\Tab #\Newline)))
    (set-var sh "PS1" (if interactive "$ " ""))
    (set-var sh "PS2" "> ")
    ;; POSIX-mandated variables the shell itself must provide.
    (set-var sh "PPID" (princ-to-string (sb-posix:getppid)))
    ;; getopts starts at the first operand; scripts test and reset OPTIND, so
    ;; leaving it unset made `[ $OPTIND -eq 1 ]' fail before any getopts call.
    (unless (nth-value 1 (get-var sh "OPTIND"))
      (set-var sh "OPTIND" "1"))
    ;; Trust an inherited $PWD only if it really names our current directory;
    ;; otherwise start from the physical path.
    (let ((inherited (nth-value 0 (get-var sh "PWD"))))
      (setf (shell-logical-cwd sh)
            (if (and inherited (plusp (length inherited))
                     (char= (char inherited 0) #\/)
                     (not (path-has-dot-components-p inherited))
                     (same-directory-p inherited))
                inherited
                (current-directory))))
    (set-var sh "PWD" (shell-logical-cwd sh) :export t)
    sh))

;;; ---------------------------------------------------------------------------
;;; working directory
;;;
;;; Always go through these two. sb-posix:chdir changes the process working
;;; directory but leaves *default-pathname-defaults* untouched, and CL resolves
;;; "." against the latter -- so (truename ".") keeps reporting whatever
;;; directory the image started in, no matter how many times the shell has cd'd.
;;; Using it for the cwd made `pwd`, `$PWD`, `cd -` and the subshell cwd
;;; snapshot all report a stale directory after the first `cd`.
;;; ---------------------------------------------------------------------------

(defun current-directory ()
  "The process's actual working directory, without a trailing slash."
  (let ((d (sb-posix:getcwd)))
    (if (and (> (length d) 1) (char= (char d (1- (length d))) #\/))
        (subseq d 0 (1- (length d)))
        d)))

(defun change-directory (path)
  "chdir to PATH, keeping *default-pathname-defaults* in step so Lisp-side
pathname operations resolve against the same place. Returns the new cwd."
  (sb-posix:chdir path)
  (let ((new (current-directory)))
    (setf *default-pathname-defaults*
          (pathname (concatenate 'string new "/")))
    new))

(defun canonicalize-logical (path)
  "Lexically canonicalize an absolute PATH: collapse empty components, drop
`.', and remove `..' together with the component before it -- WITHOUT
resolving symbolic links.

That last part is the whole point of a logical path: POSIX `cd -L' (the
default) requires `cd link/..' to return where the user came from, not to
where the link pointed."
  (let ((out '()) (part (make-string-output-stream)))
    (flet ((finish-part ()
             (let ((p (get-output-stream-string part)))
               (cond ((or (string= p "") (string= p ".")))
                     ((string= p "..") (when out (pop out)))
                     (t (push p out))))))
      (loop for ch across path
            do (if (char= ch #\/) (finish-part) (write-char ch part)))
      (finish-part))
    (if out
        (with-output-to-string (s)
          (dolist (p (nreverse out)) (write-char #\/ s) (write-string p s)))
        "/")))

(defun path-has-dot-components-p (path)
  "True if PATH contains a . or .. component."
  (let ((parts '()) (part (make-string-output-stream)))
    (flet ((finish-part () (push (get-output-stream-string part) parts)))
      (loop for ch across path
            do (if (char= ch #\/) (finish-part) (write-char ch part)))
      (finish-part))
    (some (lambda (p) (or (string= p ".") (string= p ".."))) parts)))

(defun same-directory-p (path)
  "True if PATH names the same directory as the process cwd (device+inode)."
  (handler-case
      (let ((a (sb-posix:stat path))
            (b (sb-posix:stat ".")))
        (and (= (sb-posix:stat-dev a) (sb-posix:stat-dev b))
             (= (sb-posix:stat-ino a) (sb-posix:stat-ino b))))
    (error () nil)))

(defun logical-pwd (sh)
  "The logical working directory: $PWD when it is usable, else the real one.

POSIX requires $PWD to be an absolute pathname, free of . and .. components,
that actually names the current directory -- so a stale or fabricated PWD is
ignored rather than believed."
  (let ((p (shell-logical-cwd sh)))
    (if (and p (plusp (length p)) (char= (char p 0) #\/)
             (not (path-has-dot-components-p p))
             (same-directory-p p))
        p
        (current-directory))))

(defun shell-quote (s)
  "Wrap S in single quotes so it survives re-parsing verbatim."
  (with-output-to-string (o)
    (write-char #\' o)
    (loop for c across s
          do (if (char= c #\')
                 (write-string "'\\''" o)   ; close, escaped quote, reopen
                 (write-char c o)))
    (write-char #\' o)))

;;; ---------------------------------------------------------------------------
;;; variable access
;;; ---------------------------------------------------------------------------

(defun get-var (sh name)
  "Return (values value found-p exported-p)."
  (multiple-value-bind (cell found) (gethash name (shell-vars sh))
    (if found
        (values (car cell) t (cdr cell))
        (values nil nil nil))))

(define-condition readonly-violation (error)
  ((name :initarg :name :reader readonly-violation-name))
  (:report (lambda (c s)
             (format s "~A: is read only" (readonly-violation-name c)))))

(defun readonly-p (sh name) (nth-value 1 (gethash name (shell-readonly sh))))

(defun set-var (sh name value &key export)
  (when (readonly-p sh name)
    (error 'readonly-violation :name name))
  (let ((cell (gethash name (shell-vars sh))))
    (setf (gethash name (shell-vars sh))
          ;; `set -a' (allexport) marks every assignment for export.
          (cons value (or export
                          (opt sh :allexport)
                          (and cell (cdr cell))))))
  value)

(defun mark-readonly (sh name)
  (setf (gethash name (shell-readonly sh)) t))

(defun export-var (sh name)
  (let ((cell (gethash name (shell-vars sh))))
    (setf (gethash name (shell-vars sh))
          (cons (if cell (car cell) "") t))))

(defun unset-var (sh name)
  (when (readonly-p sh name)
    (error 'readonly-violation :name name))
  (remhash name (shell-vars sh)))

(defun exported-environ (sh)
  "Build the child environment (list of K=V) from exported variables."
  (let ((out '()))
    (maphash (lambda (k cell)
               (when (cdr cell)
                 (push (format nil "~A=~A" k (car cell)) out)))
             (shell-vars sh))
    out))

;;; ---------------------------------------------------------------------------
;;; positional parameters
;;; ---------------------------------------------------------------------------

(defun positional-ref (sh n)
  "$n for n>=1, or NIL."
  (let ((v (shell-positional sh)))
    (if (<= 1 n (length v)) (aref v (1- n)) nil)))

(defun set-positional (sh list)
  (setf (shell-positional sh) (coerce list 'vector)))

;;; ---------------------------------------------------------------------------
;;; options (set -e / -x / -u / -f ...)
;;; ---------------------------------------------------------------------------

(defun opt (sh key) (gethash key (shell-options sh)))
(defun (setf opt) (val sh key) (setf (gethash key (shell-options sh)) val))
