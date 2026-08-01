;;;; shell/redir.lisp --- turn REDIRECT AST nodes into actions.
;;;;
;;;; Two consumers:
;;;;   * external commands: we build a list of FILE-ACTION recipes for
;;;;     posix_spawn (plus a list of temp files to clean up, e.g. here-docs).
;;;;   * builtins / compound commands running in-process: we apply the
;;;;     redirections to real fds with dup2, remembering saved fds to restore.

(in-package #:sxsh-shell)

(defconstant +o-rdonly+ sb-posix:o-rdonly)
(defconstant +o-wronly+ sb-posix:o-wronly)
(defconstant +o-rdwr+   sb-posix:o-rdwr)
(defconstant +o-creat+  sb-posix:o-creat)
(defconstant +o-trunc+  sb-posix:o-trunc)
(defconstant +o-append+ sb-posix:o-append)
(defconstant +o-excl+   sb-posix:o-excl)
(defconstant +mode+ #o666)

(defun existing-non-regular-p (path)
  "True if PATH exists and is not a regular file (a device, fifo, socket...)."
  (handler-case
      (let ((st (sb-posix:stat path)))
        (not (sb-posix:s-isreg (sb-posix:stat-mode st))))
    (error () nil)))

(defun noclobber-flags (sh path)
  "Extra open(2) flags for a plain `>' redirection.

With `set -C' POSIX forbids `>' from truncating an existing regular file, so
the open must fail instead: O_EXCL turns that into EEXIST. `>|' bypasses it.

The restriction is specifically about REGULAR files -- `set -C; echo > /dev/null'
must still work -- so a device or fifo keeps the ordinary flags. Applying
O_EXCL unconditionally broke writing to /dev/null under -C."
  (if (and (opt sh :noclobber) (not (existing-non-regular-p path)))
      +o-excl+
      +o-trunc+))

(define-condition redirect-error (error)
  ((path :initarg :path :reader redirect-error-path)
   (detail :initarg :detail :reader redirect-error-detail))
  (:report (lambda (c s)
             (format s "~A: ~A" (redirect-error-path c)
                     (redirect-error-detail c)))))

(defun errno-message (condition)
  "A plain strerror-style message for a failed syscall.

PRINC-TO-STRING on the condition yields SBCL's own wording -- `Error in
SB-POSIX::OPEN-WITH-MODE: No such file or directory (2)' -- which leaks the
implementation into what is supposed to be a shell diagnostic."
  (let ((errno (ignore-errors (sb-posix:syscall-errno condition))))
    (or (and errno (ignore-errors (sb-int:strerror errno)))
        (princ-to-string condition))))

(defun open-for-redirect (path flags mode)
  "sb-posix:open, but failures become a shell diagnostic rather than a raw
Lisp condition."
  (handler-case (sb-posix:open path flags mode)
    (error (e)
      (error 'redirect-error
             :path path
             :detail (if (and (logtest flags +o-excl+)
                              (probe-file path))
                         ;; the set -C case, which is not really an error the
                         ;; user needs errno for
                         "cannot overwrite existing file"
                         (errno-message e))))))

(defun default-fd (op)
  "The fd a redirection applies to when no explicit IO_NUMBER is given."
  (case op
    ((:< :<< :<<- :<> :<&) 0)
    (t 1)))

(defun heredoc-tempfile (sh redirect)
  "Write a here-doc body to a temp file, return its path. The body may need
parameter/command/arith expansion unless the delimiter was quoted."
  (destructuring-bind (delim body quoted strip) (redirect-heredoc redirect)
    (declare (ignore delim strip))
    (let ((text (if quoted body
                    (xchars->string (expand-heredoc-body sh body))))
          (path (format nil "/tmp/sxsh-heredoc-~A-~A"
                        (sb-posix:getpid) (random 1000000))))
      (with-open-file (s path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
        (write-string text s))
      path)))

;;; ---------------------------------------------------------------------------
;;; For external commands: build FILE-ACTION recipes
;;; ---------------------------------------------------------------------------

(defun redirect->file-actions (sh redirect)
  "Return (values list-of-file-action temp-path-or-nil)."
  (let* ((op (redirect-op redirect))
         (fd (or (redirect-fd redirect) (default-fd op)))
         (target-word (word-text (redirect-target redirect))))
    (ecase op
      (:< (let ((path (single-expand sh target-word)))
            ;; posix_spawn performs this open in the child, so a failure comes
            ;; back only as a generic spawn error naming the *program*. Check
            ;; it here so the diagnostic names the file, as every shell does,
            ;; and so the command fails rather than the shell aborting.
            (handler-case (sb-posix:close (sb-posix:open path +o-rdonly+ 0))
              (error (e)
                (error 'redirect-error :path path :detail (errno-message e))))
            (values (list (fa-open fd path +o-rdonly+ 0)) nil)))
      (:> (let ((path (single-expand sh target-word)))
            (values (list (fa-open fd path (logior +o-wronly+ +o-creat+
                                                   (noclobber-flags sh path))
                                   +mode+)) nil)))
      (:>\| (let ((path (single-expand sh target-word)))
              (values (list (fa-open fd path (logior +o-wronly+ +o-creat+ +o-trunc+)
                                     +mode+)) nil)))
      (:>> (let ((path (single-expand sh target-word)))
             (values (list (fa-open fd path (logior +o-wronly+ +o-creat+ +o-append+)
                                    +mode+)) nil)))
      (:<> (let ((path (single-expand sh target-word)))
             (values (list (fa-open fd path (logior +o-rdwr+ +o-creat+) +mode+)) nil)))
      ((:<< :<<-)
       (let ((path (heredoc-tempfile sh redirect)))
         (values (list (fa-open fd path +o-rdonly+ 0)) path)))
      (:<& (values (dup-actions fd (single-expand sh target-word)) nil))
      (:>& (values (dup-actions fd (single-expand sh target-word)) nil)))))

(defun dup-actions (fd target)
  "Handle n<&m, n>&m, n<&- (close). TARGET is an already-expanded fd number
or '-'. It must be expanded first: `exec 3>&$fd' is ordinary shell, and
classifying the raw word rejected it as a bad descriptor."
  (cond
    ((string= target "-") (list (fa-close fd)))
    ((and (plusp (length target)) (every #'digit-char-p target))
     (list (fa-dup2 (parse-integer target) fd)))
    (t (error 'redirect-error :path target
                              :detail "bad file descriptor in redirection"))))

(defun single-expand (sh word-text)
  "Expand a redirection target to a single field (no splitting; globbing only
if it yields exactly one match)."
  (let ((fields (expand-word-to-fields sh word-text :split nil :glob t)))
    (cond ((= 1 (length fields)) (first fields))
          ((null fields) "")
          (t (error "ambiguous redirect: ~A" word-text)))))

(defun build-spawn-file-actions (sh redirects)
  "Return (values file-actions temp-paths) for a list of REDIRECT nodes."
  (let ((actions '()) (temps '()))
    (dolist (r redirects)
      (multiple-value-bind (acts temp) (redirect->file-actions sh r)
        (setf actions (nconc actions acts))
        (when temp (push temp temps))))
    (values actions temps)))

;;; ---------------------------------------------------------------------------
;;; For in-process (builtins / compound commands): apply and restore
;;; ---------------------------------------------------------------------------

(defstruct saved-fd orig-fd backup-fd close-orig)

(defun apply-redirects-in-process (sh redirects)
  "Apply REDIRECTS to this process's fds. Returns a list of SAVED-FD to pass to
RESTORE-REDIRECTS. Also returns temp paths to delete."
  (let ((saved '()) (temps '()))
    (handler-case
        (dolist (r redirects)
          (let* ((op (redirect-op r))
                 (fd (or (redirect-fd r) (default-fd op))))
            ;; back up the target fd
            (let ((backup (ignore-errors (sb-posix:dup fd))))
              (push (make-saved-fd :orig-fd fd :backup-fd backup) saved))
            (ecase op
              ((:< :> :>\| :>> :<>)
               (let* ((path (single-expand sh (word-text (redirect-target r))))
                      (flags (ecase op
                               (:< +o-rdonly+)
                               (:> (logior +o-wronly+ +o-creat+
                                           (noclobber-flags sh path)))
                               (:>\| (logior +o-wronly+ +o-creat+ +o-trunc+))
                               (:>> (logior +o-wronly+ +o-creat+ +o-append+))
                               (:<> (logior +o-rdwr+ +o-creat+))))
                      (newfd (open-for-redirect path flags +mode+)))
                 (unless (= newfd fd)
                   (sb-posix:dup2 newfd fd) (sb-posix:close newfd))))
              ((:<< :<<-)
               (let* ((path (heredoc-tempfile sh r))
                      (newfd (open-for-redirect path +o-rdonly+ 0)))
                 (push path temps)
                 (unless (= newfd fd) (sb-posix:dup2 newfd fd) (sb-posix:close newfd))))
              ((:<& :>&)
               ;; expanded, so `exec 3>&$fd' works
               (let ((tgt (single-expand sh (word-text (redirect-target r)))))
                 (cond ((string= tgt "-") (ignore-errors (sb-posix:close fd)))
                       ((and (plusp (length tgt)) (every #'digit-char-p tgt))
                        (sb-posix:dup2 (parse-integer tgt) fd))
                       (t (error 'redirect-error
                                 :path tgt
                                 :detail "bad file descriptor in redirection"))))))))
      (error (e)
        (restore-redirects saved)
        (dolist (p temps) (ignore-errors (delete-file p)))
        (error e)))
    (values saved temps)))

(defun restore-redirects (saved)
  (dolist (s saved)
    (let ((backup (saved-fd-backup-fd s)) (orig (saved-fd-orig-fd s)))
      (if backup
          (progn (ignore-errors (sb-posix:dup2 backup orig))
                 (ignore-errors (sb-posix:close backup)))
          (ignore-errors (sb-posix:close orig))))))
