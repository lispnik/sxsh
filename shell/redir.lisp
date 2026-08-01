;;;; shell/redir.lisp --- turn REDIRECT AST nodes into actions.
;;;;
;;;; Two consumers:
;;;;   * external commands: we build a list of FILE-ACTION recipes for
;;;;     posix_spawn (plus a list of temp files to clean up, e.g. here-docs).
;;;;   * builtins / compound commands running in-process: we apply the
;;;;     redirections to real fds with dup2, remembering saved fds to restore.

(in-package #:posh-shell)

(defconstant +o-rdonly+ sb-posix:o-rdonly)
(defconstant +o-wronly+ sb-posix:o-wronly)
(defconstant +o-rdwr+   sb-posix:o-rdwr)
(defconstant +o-creat+  sb-posix:o-creat)
(defconstant +o-trunc+  sb-posix:o-trunc)
(defconstant +o-append+ sb-posix:o-append)
(defconstant +o-excl+   sb-posix:o-excl)
(defconstant +mode+ #o666)

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
          (path (format nil "/tmp/posh-heredoc-~A-~A"
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
            (values (list (fa-open fd path +o-rdonly+ 0)) nil)))
      (:> (let ((path (single-expand sh target-word)))
            (values (list (fa-open fd path (logior +o-wronly+ +o-creat+ +o-trunc+)
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
      (:<& (values (dup-actions fd target-word) nil))
      (:>& (values (dup-actions fd target-word) nil)))))

(defun dup-actions (fd target-word)
  "Handle n<&m, n>&m, n<&- (close). TARGET-WORD is a digit string or '-'."
  (cond
    ((string= target-word "-") (list (fa-close fd)))
    ((every #'digit-char-p target-word)
     (list (fa-dup2 (parse-integer target-word) fd)))
    (t (error "bad file descriptor in redirection: ~A" target-word))))

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
                               ((:> :>\|) (logior +o-wronly+ +o-creat+ +o-trunc+))
                               (:>> (logior +o-wronly+ +o-creat+ +o-append+))
                               (:<> (logior +o-rdwr+ +o-creat+))))
                      (newfd (sb-posix:open path flags +mode+)))
                 (unless (= newfd fd)
                   (sb-posix:dup2 newfd fd) (sb-posix:close newfd))))
              ((:<< :<<-)
               (let* ((path (heredoc-tempfile sh r))
                      (newfd (sb-posix:open path +o-rdonly+ 0)))
                 (push path temps)
                 (unless (= newfd fd) (sb-posix:dup2 newfd fd) (sb-posix:close newfd))))
              ((:<& :>&)
               (let ((tgt (word-text (redirect-target r))))
                 (cond ((string= tgt "-") (ignore-errors (sb-posix:close fd)))
                       ((every #'digit-char-p tgt)
                        (sb-posix:dup2 (parse-integer tgt) fd))
                       (t (error "bad fd: ~A" tgt))))))))
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
