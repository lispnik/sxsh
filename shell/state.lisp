;;;; shell/state.lisp --- shell runtime state.

(in-package #:sxsh-shell)

(defparameter +shopts-default-on+ '("globskipdots")
  "shopt names bash enables by default. SHOPT-P reads presence in the table,
so a default-on option has to be seeded there rather than inferred.")

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
  ;; One frame per active function call, innermost first. A frame is a list of
  ;; (NAME . SAVED) recording what `local NAME' shadowed, where SAVED is the
  ;; previous (value . exported-p) cell or :UNSET. Scoping is dynamic, as in
  ;; every shell that has `local': a function called from here still sees this
  ;; function's locals, so this is a save/restore stack, not a lexical chain.
  (local-frames '())
  ;; Wall-clock base for bash's $SECONDS; assigning to SECONDS moves it.
  (start-time (get-universal-time))
  ;; Names whose bash dynamic behaviour has been destroyed by `unset'. In bash
  ;; RANDOM and SECONDS are special until unset, after which they are ordinary
  ;; variables for the life of the shell -- assigning to them does not bring
  ;; the magic back.
  (dynamic-off (make-hash-table :test 'equal))
  ;; Names declared with `declare -i': every assignment to one is evaluated
  ;; as an arithmetic expression rather than stored verbatim.
  (int-vars (make-hash-table :test 'equal))
  ;; Command history, oldest first. POSIX requires it for `fc'; bash also
  ;; exposes it through `history'. Only an interactive shell records anything.
  (history (make-array 0 :adjustable t :fill-pointer t))
  ;; Number of the OLDEST entry still held. History numbers keep climbing as
  ;; entries are dropped, so `fc -l' shows stable numbers within a session.
  (history-base 1)
  ;; bash `shopt' options. Separate from SHELL-OPTIONS, which holds the
  ;; POSIX `set -o' flags -- bash keeps the two namespaces apart and so do we.
  (shopts (let ((h (make-hash-table :test 'equal)))
            ;; bash has these ON by default; every other shopt starts off.
            (dolist (n +shopts-default-on+ h) (setf (gethash n h) t))))
  ;; bash namerefs (`declare -n r=v'): NAME -> the name it stands for. Reads
  ;; and writes through the alias reach the target variable.
  (namerefs (make-hash-table :test 'equal))
  ;; The directory stack, holding only the entries BELOW the current directory,
  ;; nearest first. `dirs' splices the current directory on at the front, so
  ;; the top of the stack is $PWD by construction and `cd' replaces it without
  ;; knowing this slot exists.
  (dir-stack '())
  ;; control-flow signals for break/continue/return handled via catch tags
  )

(defvar *procsub-fds* '()
  "Descriptors opened for process substitutions in the command being built.

Closed by EXEC-SIMPLE once the command has finished. Leaving them open leaks a
descriptor per `<(...)' and, worse, keeps the pipe's read end alive so a
consumer that waits for EOF never sees it.")

(defvar *sigint-pending* nil
  "Set by the SIGINT handler so the line editor can tell an interrupt apart
from a read failure. Defined here rather than in term.lisp because jobs.lisp
installs the handler and loads earlier.")
(defvar *winch-pending* nil)
(defvar *cont-pending* nil)

(defvar *assignment-error-fatal* t
  "Whether a variable assignment error ends a non-interactive shell.

POSIX 2.8.1 makes it fatal, and it is: `readonly r=1; r=2' must not reach the
next command. But the abort is contained by some constructs -- bash carries on
after `eval \"r=2\"' and after the prefix form `r=2 cmd', failing only that
command. Those bind this to NIL rather than each catching SHELL-EXIT, which
would also swallow a legitimate `exit' inside the same construct.")

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
    ;; Alias substitution happens in the PARSER (POSIX 2.3.1), which must not
    ;; depend on the shell, so the table is reached through a closure. One
    ;; shell per process, so a global is the right scope.
    (setf sxsh:*alias-lookup*
          (lambda (name) (gethash name (shell-aliases sh))))
    ;; Without this the saved image starts from the same random state every
    ;; run, so $RANDOM would produce an identical sequence in every shell.
    (setf *random-state* (make-random-state t))
    ;; seed variables from the real environment, marked exported
    (dolist (kv (sb-ext:posix-environ))
      (let ((eq (position #\= kv)))
        (when eq
          (setf (gethash (subseq kv 0 eq) (shell-vars sh))
                (cons (subseq kv (1+ eq)) t)))))
    ;; sensible defaults
    (unless (nth-value 1 (get-var sh "IFS"))
      (set-var sh "IFS" (format nil " ~C~C" #\Tab #\Newline)))
    ;; Line editing is on by default for an interactive shell, as in bash.
    (when interactive (setf (gethash :emacs (shell-options sh)) t))
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
    (cond
      ((and p (plusp (length p)) (char= (char p 0) #\/)
            (not (path-has-dot-components-p p))
            (same-directory-p p))
       p)
      ;; The directory may simply be GONE -- `cd d; rmdir ../d' -- in which
      ;; case getcwd fails and there is no real path to fall back to. Every
      ;; shell keeps reporting the logical one rather than erroring, which
      ;; also leaves $OLDPWD usable for the `cd' back out.
      ((or (ignore-errors (current-directory))
           (and p (plusp (length p)) p)
           "")))))

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

;;; ---------------------------------------------------------------------------
;;; Arrays (bash; not POSIX)
;;;
;;; A variable cell is (VALUE . EXPORTED-P) where VALUE is normally a string.
;;; For an array it is an SH-ARRAY instead, so nothing that only reads scalars
;;; needs to change -- GET-VAR keeps returning a string. Both flavours use a
;;; hash table because bash arrays are sparse: `a[5]=x' on an empty array
;;; leaves indices 0-4 genuinely absent, not empty strings.
;;; ---------------------------------------------------------------------------

(defstruct (sh-array (:constructor %make-sh-array (kind)))
  (kind :indexed)                       ; :indexed or :assoc
  (table (make-hash-table :test 'equal)))

(defun make-sh-array (&optional (kind :indexed)) (%make-sh-array kind))

(defun array-key (arr key)
  "Normalise a subscript: integers for an indexed array, strings for assoc."
  (if (eq (sh-array-kind arr) :assoc)
      (princ-to-string key)
      (if (integerp key) key (or (ignore-errors (parse-integer key)) 0))))

(defun array-get (arr key)
  (gethash (array-key arr key) (sh-array-table arr)))

(defun array-set (arr key value)
  (setf (gethash (array-key arr key) (sh-array-table arr)) value))

(defun array-unset (arr key)
  (remhash (array-key arr key) (sh-array-table arr)))

(defun array-keys (arr)
  "Subscripts in order: numeric for an indexed array, insertion-independent
lexicographic for an associative one (bash's order is unspecified there)."
  (let ((keys '()))
    (maphash (lambda (k v) (declare (ignore v)) (push k keys))
             (sh-array-table arr))
    (if (eq (sh-array-kind arr) :assoc)
        (sort keys #'string<)
        (sort keys #'<))))

(defun array-values (arr)
  (mapcar (lambda (k) (array-get arr k)) (array-keys arr)))

(defun array-from-list (values &optional (kind :indexed))
  (let ((arr (make-sh-array kind)))
    (loop for v in values for i from 0 do (array-set arr i v))
    arr))

(defun array-next-index (arr)
  "Where an append lands: one past the highest index, 0 when empty."
  (let ((keys (array-keys arr)))
    (if keys (1+ (reduce #'max keys)) 0)))

(defun var-array (sh name)
  "The SH-ARRAY stored under NAME, or NIL if the value is a scalar/absent."
  (let ((cell (gethash name (shell-vars sh))))
    (and cell (sh-array-p (car cell)) (car cell))))

(defun scalar-of (value)
  "The scalar view of a cell value. bash: `$a' on an array is `${a[0]}'."
  (if (sh-array-p value)
      (or (array-get value 0) "")
      value))

(defun resolve-nameref (sh name)
  "Follow a nameref chain to the name it ultimately designates.

Depth-limited: `declare -n a=b; declare -n b=a' is a cycle, and bash reports
it rather than hanging. We simply stop and use the last name reached."
  (let ((seen '()))
    (loop repeat 32
          for target = (gethash name (shell-namerefs sh))
          while (and target (not (member target seen :test #'string=)))
          do (push name seen) (setf name target))
    name))

(defun get-var (sh name)
  "Return (values value found-p exported-p)."
  (let ((name (resolve-nameref sh name)))
    (multiple-value-bind (cell found) (gethash name (shell-vars sh))
      (cond
        ;; `$a' on an array means `${a[0]}', so an array with no element 0 is
        ;; UNSET as far as the scalar view is concerned -- `set -u; declare -a
        ;; x; echo $x' is an unbound-variable error in bash, and `${x+SET}' is
        ;; empty. Reporting it as found-but-empty made both silently succeed.
        ((and found (sh-array-p (car cell)) (null (array-get (car cell) 0)))
         (values nil nil (cdr cell)))
        (found (values (scalar-of (car cell)) t (cdr cell)))
        (t (values nil nil nil))))))

(define-condition readonly-violation (error)
  ((name :initarg :name :reader readonly-violation-name))
  (:report (lambda (c s)
             (format s "~A: is read only" (readonly-violation-name c)))))

(defun readonly-p (sh name) (nth-value 1 (gethash name (shell-readonly sh))))

(defun set-var (sh name value &key export)
  ;; An assignment through a nameref lands on the target, not the alias.
  (setf name (resolve-nameref sh name))
  (when (readonly-p sh name)
    (error 'readonly-violation :name name))
  ;; bash: assigning to RANDOM SEEDS the generator rather than replacing it,
  ;; and assigning to SECONDS moves its origin. Both stay dynamic afterwards,
  ;; so neither value is stored -- `RANDOM=5; echo $RANDOM' prints a random
  ;; number, not 5.
  (unless (nth-value 1 (gethash name (shell-dynamic-off sh)))
    (cond
      ((string= name "RANDOM")
       (setf *random-state* (sb-ext:seed-random-state
                             (or (ignore-errors (parse-integer value)) 0)))
       (return-from set-var value))
      ((string= name "SECONDS")
       (setf (shell-start-time sh)
             (- (get-universal-time)
                (or (ignore-errors (parse-integer value)) 0)))
       (return-from set-var value))))
  (let ((cell (gethash name (shell-vars sh))))
    (setf (gethash name (shell-vars sh))
          ;; `set -a' (allexport) marks every assignment for export.
          (cons value (or export
                          (opt sh :allexport)
                          (and cell (cdr cell))))))
  value)

;;; ---------------------------------------------------------------------------
;;; History
;;; ---------------------------------------------------------------------------

(defun history-limit (sh)
  "$HISTSIZE, defaulting to 500 as bash does. A non-numeric value means the
default; zero or less disables recording entirely."
  (let ((v (nth-value 0 (get-var sh "HISTSIZE"))))
    (or (and v (ignore-errors (parse-integer (string-trim " " v)))) 500)))

(defun history-file (sh)
  (let ((v (nth-value 0 (get-var sh "HISTFILE"))))
    (if (and v (plusp (length v)))
        v
        (let ((home (nth-value 0 (get-var sh "HOME"))))
          (and home (concatenate 'string home "/.sxsh_history"))))))

(defun history-add (sh line)
  "Record LINE, trimming to $HISTSIZE. Consecutive duplicates and blank lines
are dropped, which is what makes the list usable rather than a transcript."
  (let ((text (string-trim '(#\Space #\Tab #\Newline) line))
        (limit (history-limit sh)))
    (when (and (plusp (length text)) (plusp limit))
      (let ((h (shell-history sh)))
        (unless (and (plusp (fill-pointer h))
                     (string= text (aref h (1- (fill-pointer h)))))
          (vector-push-extend text h)
          ;; Drop from the front, advancing the base so numbering is stable.
          (loop while (> (fill-pointer h) limit)
                do (replace h h :start2 1)
                   (decf (fill-pointer h))
                   (incf (shell-history-base sh))))))))

(defun history-count (sh) (fill-pointer (shell-history sh)))

(defun history-number (sh index)
  "History number of the entry at INDEX (0-based)."
  (+ (shell-history-base sh) index))

(defun history-index (sh number)
  "Index of the entry with history NUMBER, or NIL if it has been dropped."
  (let ((i (- number (shell-history-base sh))))
    (and (<= 0 i) (< i (history-count sh)) i)))

(defun history-resolve (sh spec default)
  "Resolve an `fc' operand: a positive number is a history number, a negative
one counts back from the most recent, and a string selects the most recent
command STARTING WITH it. Returns an index, or NIL."
  (cond
    ((null spec) default)
    ((ignore-errors (parse-integer (string-trim " " spec)))
     (let ((n (parse-integer (string-trim " " spec))))
       (if (minusp n)
           (let ((i (+ (history-count sh) n)))
             (and (<= 0 i) (< i (history-count sh)) i))
           (history-index sh n))))
    (t
     ;; Most recent command starting with SPEC.
     (loop for i from (1- (history-count sh)) downto 0
           when (let ((e (aref (shell-history sh) i)))
                  (and (>= (length e) (length spec))
                       (string= spec e :end2 (length spec))))
             do (return i)))))

(defun history-load (sh)
  "Read $HISTFILE at startup. A missing or unreadable file is not an error."
  (let ((path (history-file sh)))
    (when (and path (probe-file path))
      (ignore-errors
       (with-open-file (in path :if-does-not-exist nil)
         (when in
           (loop for line = (read-line in nil nil)
                 while line do (history-add sh line))))))))

(defun history-save (sh)
  "Write $HISTFILE on exit. Best effort: a read-only home is not fatal."
  (let ((path (history-file sh)))
    (when (and path (plusp (history-limit sh)))
      (ignore-errors
       (with-open-file (out path :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
         (loop for e across (shell-history sh) do (write-line e out)))))))

(defun push-local-frame (sh)
  (push '() (shell-local-frames sh)))

(defun pop-local-frame (sh)
  "Restore everything the innermost frame shadowed, then discard it."
  (let ((frame (pop (shell-local-frames sh))))
    ;; Restore in reverse order of declaration: a name declared `local' twice
    ;; in one function saved the outer binding first, and that is the one that
    ;; has to end up in the table.
    (dolist (entry frame)
      (destructuring-bind (name . saved) entry
        (if (eq saved :unset)
            (remhash name (shell-vars sh))
            (setf (gethash name (shell-vars sh)) saved))))))

(defun declare-local (sh name)
  "Record NAME as local to the current function and remove its current value.

`local x' with no assignment leaves x unset rather than inheriting the outer
value: that is what bash and zsh do, and it is the reading that makes `local'
useful as a declaration. dash inherits instead, but a script that relies on
that is relying on the least portable of the three behaviours."
  (let ((frame (first (shell-local-frames sh))))
    ;; Only the first `local x' in a frame saves; a second one would otherwise
    ;; save the local binding and restore that on return instead of the
    ;; caller's value.
    (unless (assoc name frame :test #'string=)
      (multiple-value-bind (cell found) (gethash name (shell-vars sh))
        (push (cons name (if found cell :unset))
              (first (shell-local-frames sh))))))
  (remhash name (shell-vars sh))
  name)

(defun in-function-p (sh) (not (null (shell-local-frames sh))))

(defun mark-readonly (sh name)
  (setf (gethash name (shell-readonly sh)) t))

(defun export-var (sh name)
  (let ((cell (gethash name (shell-vars sh))))
    (setf (gethash name (shell-vars sh))
          (cons (if cell (car cell) "") t))))

(defvar *temp-frames* '()
  "Stack of active command-scoped assignment frames, innermost first.

Each frame is a one-element box holding a list of (NAME VALUE FOUND EXPORTED)
records of what a `VAR=x cmd' prefix shadowed. UNSET consults it so that
unsetting a temporarily-bound name reveals what it covered rather than deleting
both: in `x=global; x=temp f', an `unset x' inside f leaves x at `global'.")

(defun pop-temp-binding (sh name)
  "If NAME is bound by an active command-scoped frame, put back what it
shadowed, drop the record so the frame does not restore it a second time, and
return T."
  (dolist (frame *temp-frames* nil)
    (let ((entry (assoc name (car frame) :test #'string=)))
      (when entry
        (destructuring-bind (n value found exported) entry
          (declare (ignore n))
          (if found
              (setf (gethash name (shell-vars sh)) (cons value exported))
              (remhash name (shell-vars sh))))
        (setf (car frame) (remove entry (car frame) :test #'eq))
        (return t)))))

(defun unset-var (sh name)
  (when (readonly-p sh name)
    (error 'readonly-violation :name name))
  ;; Unsetting a dynamic variable makes it ordinary from here on, as in bash.
  (setf (gethash name (shell-dynamic-off sh)) t)
  (unless (pop-temp-binding sh name)
    (remhash name (shell-vars sh))))

(defun exported-environ (sh)
  "Build the child environment (list of K=V) from exported variables."
  (let ((out '()))
    (maphash (lambda (k cell)
               ;; bash cannot export an array through the environment, and
               ;; neither can we -- there is nowhere to put the subscripts.
               (when (and (cdr cell) (not (sh-array-p (car cell))))
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
