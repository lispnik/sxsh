;;;; spawn.lisp --- posix_spawn(3) FFI layer for the shell executor.
;;;;
;;;; We bind posix_spawnp and the file-actions / attributes API directly via
;;;; SB-ALIEN, rather than fork()+exec(). The opaque structs
;;;; posix_spawn_file_actions_t and posix_spawnattr_t are allocated as raw
;;;; byte blocks sized generously for glibc; their internals are only touched
;;;; through the libc accessor functions.

(in-package #:posh-shell)

;;; The spawn objects are opaque. Their representation differs by platform:
;;;   * Linux/glibc: the object IS the struct, stored inline
;;;     (posix_spawn_file_actions_t = 80 bytes, posix_spawnattr_t = 336).
;;;   * macOS/Darwin: the object is a POINTER; posix_spawn*_init() malloc()s
;;;     the real struct and stores the pointer here (so only 8 bytes are used).
;;; In both cases we hand the libc init function a pointer to a zeroed block
;;; and let it populate the block; we never inspect the internals. We allocate
;;; a block large enough for the inline-struct case, with headroom, which also
;;; trivially covers the pointer case.
(defconstant +file-actions-size+
  #+darwin 64                           ; pointer-sized object; padded
  #-darwin 256)                         ; inline struct (glibc 80) + headroom
(defconstant +spawnattr-size+
  #+darwin 64                           ; pointer-sized object; padded
  #-darwin 512)                         ; inline struct (glibc 336) + headroom

(defconstant +posix-spawn-setpgroup+  2)
(defconstant +posix-spawn-setsigdef+  4)
(defconstant +posix-spawn-setsigmask+ 8)

;;; ---------------------------------------------------------------------------
;;; libc function declarations
;;; ---------------------------------------------------------------------------

(sb-alien:define-alien-routine ("posix_spawnp" %posix-spawnp) sb-alien:int
  (pid (* sb-alien:int))
  (file sb-alien:c-string)
  (file-actions (* t))
  (attrp (* t))
  (argv (* sb-alien:c-string))
  (envp (* sb-alien:c-string)))

(sb-alien:define-alien-routine ("posix_spawn_file_actions_init" %fa-init) sb-alien:int
  (fa (* t)))
(sb-alien:define-alien-routine ("posix_spawn_file_actions_destroy" %fa-destroy) sb-alien:int
  (fa (* t)))
(sb-alien:define-alien-routine ("posix_spawn_file_actions_adddup2" %fa-adddup2) sb-alien:int
  (fa (* t)) (fildes sb-alien:int) (newfildes sb-alien:int))
(sb-alien:define-alien-routine ("posix_spawn_file_actions_addopen" %fa-addopen) sb-alien:int
  (fa (* t)) (fildes sb-alien:int) (path sb-alien:c-string)
  (oflag sb-alien:int) (mode sb-alien:unsigned-int))
(sb-alien:define-alien-routine ("posix_spawn_file_actions_addclose" %fa-addclose) sb-alien:int
  (fa (* t)) (fildes sb-alien:int))

(sb-alien:define-alien-routine ("posix_spawnattr_init" %attr-init) sb-alien:int
  (attr (* t)))
(sb-alien:define-alien-routine ("posix_spawnattr_destroy" %attr-destroy) sb-alien:int
  (attr (* t)))
(sb-alien:define-alien-routine ("posix_spawnattr_setflags" %attr-setflags) sb-alien:int
  (attr (* t)) (flags sb-alien:short))
(sb-alien:define-alien-routine ("posix_spawnattr_setpgroup" %attr-setpgroup) sb-alien:int
  (attr (* t)) (pgroup sb-alien:int))
(sb-alien:define-alien-routine ("posix_spawnattr_setsigdefault" %attr-setsigdefault)
    sb-alien:int
  (attr (* t)) (sigset (* t)))

(sb-alien:define-alien-routine ("sigemptyset" %sigemptyset) sb-alien:int
  (set (* t)))
(sb-alien:define-alien-routine ("sigaddset" %sigaddset) sb-alien:int
  (set (* t)) (signo sb-alien:int))

;;; sigset_t is 4 bytes on macOS and 128 on Linux; as with the spawn objects we
;;; allocate a generously sized zeroed block and only touch it through libc.
(defconstant +sigset-size+ 256)

;;; ---------------------------------------------------------------------------
;;; File-action recipe -- a small Lisp description compiled into libc calls
;;; just before the spawn, so redirection setup stays declarative.
;;; ---------------------------------------------------------------------------

(defstruct file-action
  kind                                  ; :dup2 :open :close
  a b oflag mode)

(defun fa-dup2 (oldfd newfd) (make-file-action :kind :dup2 :a oldfd :b newfd))
(defun fa-open (fd path oflag mode) (make-file-action :kind :open :a fd :b path
                                                      :oflag oflag :mode mode))
(defun fa-close (fd) (make-file-action :kind :close :a fd))

;;; ---------------------------------------------------------------------------
;;; argv / envp marshalling
;;; ---------------------------------------------------------------------------

(defun make-c-string-array (strings)
  "Allocate a NULL-terminated (* c-string) array. Caller frees with
free-c-string-array."
  (let* ((n (length strings))
         (arr (sb-alien:make-alien sb-alien:c-string (1+ n))))
    (loop for s in strings for i from 0
          do (setf (sb-alien:deref arr i) s))
    (setf (sb-alien:deref arr n) nil)
    arr))

(defun free-c-string-array (arr)
  (sb-alien:free-alien arr))

;;; ---------------------------------------------------------------------------
;;; The spawn entry point
;;; ---------------------------------------------------------------------------

(define-condition spawn-error (error)
  ((code :initarg :code :reader spawn-error-code)
   (file :initarg :file :reader spawn-error-file))
  (:report (lambda (c s)
             (format s "posix_spawnp failed for ~S: ~A (error ~D)"
                     (spawn-error-file c)
                     (or (ignore-errors (sb-int:strerror (spawn-error-code c)))
                         "spawn error")
                     (spawn-error-code c)))))

(defun spawn (path argv &key (env (current-environ)) file-actions
                             pgroup (setpgroup nil) sigdefault)
  "Spawn PATH (searched in $PATH via posix_spawnp) with ARGV (list of strings,
argv[0] included) and ENV (list of \"K=V\" strings). FILE-ACTIONS is a list of
FILE-ACTION structs applied in order in the child. When SETPGROUP is true, the
child is placed in process group PGROUP (0 = new group led by the child).
SIGDEFAULT is a list of signal numbers to reset to SIG_DFL in the child.
Returns the child PID.

SIGDEFAULT matters for job control. A SIG_IGN disposition is inherited across
exec, so the SIGTSTP/SIGTTIN/SIGTTOU that the shell ignores for its own sake
would otherwise be ignored by every child too -- making Ctrl-Z a no-op on the
foreground job. Installed *handlers* need no such treatment: exec resets those
to default automatically."
  (sb-alien:with-alien ((pid sb-alien:int)
                        (fa (array sb-alien:char #.+file-actions-size+))
                        (attr (array sb-alien:char #.+spawnattr-size+))
                        (sigs (array sb-alien:char #.+sigset-size+)))
    (let ((fa* (sb-alien:cast fa (* t)))
          (attr* (sb-alien:cast attr (* t)))
          (sigs* (sb-alien:cast sigs (* t)))
          (argv-arr nil) (env-arr nil)
          (fa-inited nil) (attr-inited nil)
          (flags 0))
      (unwind-protect
           (progn
             ;; zero the opaque blocks before init (hygiene; required on
             ;; platforms where the object is a pointer that init overwrites)
             (dotimes (k +file-actions-size+) (setf (sb-alien:deref fa k) 0))
             (dotimes (k +spawnattr-size+) (setf (sb-alien:deref attr k) 0))
             (dotimes (k +sigset-size+) (setf (sb-alien:deref sigs k) 0))
             (let ((rc (%fa-init fa*)))
               (unless (zerop rc) (error 'spawn-error :code rc :file path)))
             (setf fa-inited t)
             (let ((rc (%attr-init attr*)))
               (unless (zerop rc) (error 'spawn-error :code rc :file path)))
             (setf attr-inited t)
             ;; compile file actions
             (dolist (act file-actions)
               (ecase (file-action-kind act)
                 (:dup2 (%fa-adddup2 fa* (file-action-a act) (file-action-b act)))
                 (:close (%fa-addclose fa* (file-action-a act)))
                 (:open (%fa-addopen fa* (file-action-a act) (file-action-b act)
                                     (file-action-oflag act) (file-action-mode act)))))
             ;; process-group attribute for job control
             (when setpgroup
               (setf flags (logior flags +posix-spawn-setpgroup+))
               (%attr-setpgroup attr* (or pgroup 0)))
             ;; restore default dispositions for signals the shell ignores
             (when sigdefault
               (%sigemptyset sigs*)
               (dolist (s sigdefault) (%sigaddset sigs* s))
               (%attr-setsigdefault attr* sigs*)
               (setf flags (logior flags +posix-spawn-setsigdef+)))
             (unless (zerop flags)
               (%attr-setflags attr* flags))
             (setf argv-arr (make-c-string-array argv)
                   env-arr (make-c-string-array env))
             (let ((rc (%posix-spawnp (sb-alien:addr pid) path fa* attr*
                                      argv-arr env-arr)))
               (unless (zerop rc)
                 (error 'spawn-error :code rc :file path))
               pid))
        (when argv-arr (free-c-string-array argv-arr))
        (when env-arr (free-c-string-array env-arr))
        (when fa-inited (%fa-destroy fa*))
        (when attr-inited (%attr-destroy attr*))))))

;;; ---------------------------------------------------------------------------
;;; Environment access
;;; ---------------------------------------------------------------------------

(defun current-environ ()
  "The current process environment as a list of \"K=V\" strings."
  (sb-ext:posix-environ))
