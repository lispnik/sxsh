;;;; shell/term.lisp --- raw-mode terminal I/O for the line editor.
;;;;
;;;; Everything here talks to the terminal device and nothing knows about the
;;;; shell. The only thing borrowed from elsewhere is TTY-FD (jobs.lisp), which
;;;; is a private fd on /dev/tty rather than 0/1/2 -- those can be redirected,
;;;; and RUN-BUILTIN rebinds *STANDARD-OUTPUT*.
;;;;
;;;; The rule that keeps this safe: no path may leave raw mode set. Every entry
;;;; goes through WITH-RAW-TERMINAL, whose cleanup runs on any exit including a
;;;; thrown condition, and *SAVED-TERMIOS* lets the shell's outermost handler
;;;; restore the terminal even if that somehow fails. A shell that exits leaving
;;;; the terminal raw gives the user a session with no echo and no line
;;;; discipline, and it survives the shell's death.

(in-package #:sxsh-shell)

(defconstant +escape-timeout-ms+ 50
  "How long to wait for the rest of an escape sequence before concluding the
user pressed a bare ESC. readline's keyseq-timeout default; short enough not
to be felt, long enough that a slow terminal's arrow key still arrives whole.")

(defconstant +fallback-columns+ 80)
(defconstant +min-columns+ 20)
(defconstant +max-columns+ 1000)

;;; ---------------------------------------------------------------------------
;;; Terminal size
;;; ---------------------------------------------------------------------------

;;; TIOCGWINSZ is a computed _IOR value and differs per platform -- exactly the
;;; class of constant that CLAUDE.md warns about, and the same trap as
;;; +wcontinued+ in jobs.lisp. Neither sb-posix nor sb-unix exports it, so it
;;; is spelled out here per platform rather than guessed.
(defconstant +tiocgwinsz+
  #+darwin #x40087468
  #-darwin #x5413)

(sb-alien:define-alien-type nil
  (sb-alien:struct winsize
                   (ws-row sb-alien:unsigned-short)
                   (ws-col sb-alien:unsigned-short)
                   (ws-xpixel sb-alien:unsigned-short)
                   (ws-ypixel sb-alien:unsigned-short)))

(sb-alien:define-alien-routine ("ioctl" %ioctl-winsize) sb-alien:int
  (fd sb-alien:int)
  (request sb-alien:unsigned-long)
  (ws (* (sb-alien:struct winsize))))

(defun terminal-size ()
  "Return (values rows columns), or NIL if the terminal will not say.

Order: $COLUMNS/$LINES if a caller set them, then the ioctl, then nothing --
the caller substitutes a default. A failed ioctl leaves the struct untouched,
so its contents must not be trusted; that is why the result is range-checked
rather than returned as-is."
  (handler-case
      (sb-alien:with-alien ((ws (sb-alien:struct winsize)))
        (setf (sb-alien:slot ws 'ws-col) 0
              (sb-alien:slot ws 'ws-row) 0)
        (let ((r (%ioctl-winsize (tty-fd) +tiocgwinsz+ (sb-alien:addr ws))))
          (when (and r (zerop r))
            (let ((cols (sb-alien:slot ws 'ws-col))
                  (rows (sb-alien:slot ws 'ws-row)))
              (when (plusp cols)
                (values (max 1 rows) cols))))))
    (error () nil)))

(defun terminal-columns ()
  "Usable width, always a sane number.

Clamped: a garbage value from a failed ioctl must never become the wrap width,
because the redraw does column arithmetic with it and a wrong answer scrambles
the display in a way the user cannot escape except with C-l."
  (let ((n (or (let ((v (sb-posix:getenv "COLUMNS")))
                 (and v (ignore-errors (parse-integer v))))
               (nth-value 1 (terminal-size))
               +fallback-columns+)))
    (max +min-columns+ (min +max-columns+ n))))

;;; ---------------------------------------------------------------------------
;;; Raw mode
;;; ---------------------------------------------------------------------------

(defvar *saved-termios* nil
  "Terminal settings captured on entering raw mode, for the outermost handler
to restore if the normal cleanup could not run.")

(defun raw-mode-apply (fd)
  "Put FD in raw mode and return the settings to restore afterwards.

Two TCGETATTR calls, deliberately. sb-posix has no COPY-TERMIOS and no
MAKE-TERMIOS, so mutating the object we intend to restore from would destroy
the only record of how the terminal was configured -- silently, and
permanently for that terminal session."
  (let ((saved (sb-posix:tcgetattr fd))
        (work (sb-posix:tcgetattr fd)))
    (setf (sb-posix:termios-lflag work)
          (logandc2 (sb-posix:termios-lflag work)
                    ;; ISIG is deliberately KEPT: Ctrl-C at the prompt stays a
                    ;; real SIGINT to the process group, which is far less code
                    ;; than handling VINTR ourselves and behaves identically
                    ;; whether or not the editor is running.
                    (logior sb-posix:icanon sb-posix:echo sb-posix:iexten)))
    (setf (sb-posix:termios-iflag work)
          (logandc2 (sb-posix:termios-iflag work)
                    ;; IXON off so C-s/C-q reach us as keys; ICRNL off so Enter
                    ;; arrives as CR and is distinguishable from C-j (both are
                    ;; then mapped to accept-line).
                    (logior sb-posix:ixon sb-posix:icrnl)))
    ;; OPOST is left ON. The redraw always writes explicit CR LF, so the code
    ;; is correct either way and does not fight a user's stty settings.
    (let ((cc (sb-posix:termios-cc work)))
      (setf (aref cc sb-posix:vmin) 1     ; block until at least one byte
            (aref cc sb-posix:vtime) 0))  ; ...with no inter-byte timer
    (sb-posix:tcsetattr fd sb-posix:tcsanow work)
    saved))

(defun restore-terminal (fd saved)
  (when saved
    (ignore-errors (sb-posix:tcsetattr fd sb-posix:tcsanow saved))))

(defun restore-terminal-if-raw ()
  "Last-ditch restore, called from the shell's outermost cleanup."
  (when *saved-termios*
    (ignore-errors (restore-terminal (tty-fd) *saved-termios*))
    (setf *saved-termios* nil)))

(defmacro with-raw-terminal (&body body)
  "Run BODY with the terminal in raw mode, restoring it however BODY exits.

Returns :NO-TTY without running BODY when there is no terminal to configure,
so callers can fall back to the line-at-a-time reader."
  (let ((fd (gensym "FD")) (saved (gensym "SAVED")))
    `(let* ((,fd (tty-fd))
            (,saved (handler-case (raw-mode-apply ,fd) (error () nil))))
       (if (null ,saved)
           :no-tty
           (progn
             (setf *saved-termios* ,saved)
             (unwind-protect
                  (progn ,@body)
               ;; Always end on a fresh line: however the edit finished, the
               ;; shell's next output must start at column 0.
               (term-write (string #\Return))
               (term-write (string #\Newline))
               (term-write +paste-off+)
               (term-flush)
               (restore-terminal ,fd ,saved)
               (setf *saved-termios* nil)))))))

;;; ---------------------------------------------------------------------------
;;; Buffered output
;;; ---------------------------------------------------------------------------

(defvar *term-out* (make-array 256 :element-type 'character
                                   :adjustable t :fill-pointer 0)
  "Accumulates a whole repaint, so the terminal sees one write and the user
sees no flicker.")

(defun term-write (string)
  (loop for c across string do (vector-push-extend c *term-out*)))

(defun term-flush ()
  (when (plusp (fill-pointer *term-out*))
    (let ((octets (sb-ext:string-to-octets (coerce *term-out* 'string)
                                           :external-format :utf-8)))
      (sb-sys:with-pinned-objects (octets)
        (ignore-errors
         (sb-unix:unix-write (tty-fd) (sb-sys:vector-sap octets) 0
                             (length octets)))))
    (setf (fill-pointer *term-out*) 0)))

(defun term-beep () (term-write (string (code-char 7))))

;;; ---------------------------------------------------------------------------
;;; Signals the editor cares about
;;; ---------------------------------------------------------------------------

(defvar *sigint-pending* nil)
(defvar *winch-pending* nil)
(defvar *cont-pending* nil)

(defparameter +paste-on+ (format nil "~C[?2004h" #\Escape))
(defparameter +paste-off+ (format nil "~C[?2004l" #\Escape))

(defun install-editor-handlers ()
  "Install SIGWINCH/SIGCONT handlers. SIGINT is left to jobs.lisp, which
already installs one -- it now sets *SIGINT-PENDING* rather than doing nothing.

Signal NUMBERS always come from sb-unix, never literals: SIGCONT is 18 on
Linux and 19 on macOS, and this shell has been bitten by exactly that before."
  (ignore-errors
   (sb-sys:enable-interrupt sb-unix:sigwinch
                            (lambda (&rest _) (declare (ignore _))
                              (setf *winch-pending* t))))
  (ignore-errors
   (sb-sys:enable-interrupt sb-unix:sigcont
                            (lambda (&rest _) (declare (ignore _))
                              (setf *cont-pending* t)))))

;;; ---------------------------------------------------------------------------
;;; Input
;;; ---------------------------------------------------------------------------

(defun read-input-byte ()
  "One byte from the terminal. Returns an integer, or a keyword:
:EOF, :INTERRUPT, :WINCH, :CONT.

Two failure modes worth naming, because both are worse than a wrong keystroke:

A zero-byte read is EOF and is NEVER retried. If VMIN/VTIME were ever
misconfigured a retry loop here would spin at 100% CPU; treating 0 as EOF
turns that into a clean exit instead of a hung machine.

EINTR is NOT end of input. A signal arriving mid-read makes unix-read fail,
and reporting that as EOF would make every Ctrl-C quit the shell. This is the
same trap FD-READ-LINE documents."
  (let ((buf (make-array 1 :element-type '(unsigned-byte 8))))
    (loop
      (when *sigint-pending* (setf *sigint-pending* nil) (return :interrupt))
      (when *winch-pending* (setf *winch-pending* nil) (return :winch))
      (when *cont-pending* (setf *cont-pending* nil) (return :cont))
      (let ((n (sb-sys:with-pinned-objects (buf)
                 (multiple-value-bind (r errno)
                     (sb-unix:unix-read (tty-fd) (sb-sys:vector-sap buf) 1)
                   (cond
                     (r r)
                     ((eql errno sb-unix:eintr) :retry)
                     (t 0))))))
        (cond
          ((eq n :retry))               ; loop: a pending flag will be seen above
          ((zerop n) (return :eof))
          (t (return (aref buf 0))))))))

(defun decode-utf8 (lead)
  "Read the continuation bytes for LEAD and return one character.

Decoding on input means one buffer character is one codepoint, so editing
commands cannot cut a multi-byte character in half -- byte-at-a-time editing
corrupts any non-ASCII filename the moment you press backspace.

Out of scope, and worth knowing: East Asian wide characters and combining
marks are all treated as one column, so the cursor drifts on CJK input."
  (let* ((n (cond ((< lead #x80) 0)
                  ((= (logand lead #xE0) #xC0) 1)
                  ((= (logand lead #xF0) #xE0) 2)
                  ((= (logand lead #xF8) #xF0) 3)
                  (t 0)))
         (code (case n
                 (0 lead)
                 (1 (logand lead #x1F))
                 (2 (logand lead #x0F))
                 (t (logand lead #x07)))))
    (dotimes (i n)
      (let ((b (read-input-byte)))
        (unless (integerp b) (return-from decode-utf8 (code-char lead)))
        (setf code (logior (ash code 6) (logand b #x3F)))))
    (or (ignore-errors (code-char code)) #\?)))

(defun read-csi ()
  "Consume a CSI/SS3 body and return a key keyword, or NIL if unrecognised.

Unrecognised sequences return NIL and the caller DISCARDS them. Falling
through and inserting the bytes as text is how a mouse click or an unknown
function key ends up scribbling `[<0;44;12M' into the command line."
  (let ((params (make-array 0 :element-type 'character
                              :adjustable t :fill-pointer 0))
        (final nil))
    (loop
      (let ((b (read-input-byte)))
        (unless (integerp b) (return-from read-csi nil))
        (cond
          ;; parameter bytes 0x30-0x3F, intermediates 0x20-0x2F
          ((or (<= #x30 b #x3F) (<= #x20 b #x2F))
           (vector-push-extend (code-char b) params))
          ((<= #x40 b #x7E) (setf final (code-char b)) (return))
          (t (return-from read-csi nil)))))
    (let ((p (coerce params 'string)))
      (cond
        ((and (char= final #\~) (string= p "200")) :paste-start)
        ((and (char= final #\~) (string= p "201")) :paste-end)
        ((char= final #\A) :up)
        ((char= final #\B) :down)
        ((char= final #\C) (if (search "5" p) :ctrl-right :right))
        ((char= final #\D) (if (search "5" p) :ctrl-left :left))
        ((char= final #\H) :home)
        ((char= final #\F) :end)
        ((and (char= final #\~) (member p '("1" "7") :test #'string=)) :home)
        ((and (char= final #\~) (member p '("4" "8") :test #'string=)) :end)
        ((and (char= final #\~) (string= p "3")) :delete)
        ((and (char= final #\~) (string= p "5")) :page-up)
        ((and (char= final #\~) (string= p "6")) :page-down)
        (t nil)))))

(defun read-key ()
  "One key. Returns a character, a keyword, (:meta . char), or one of
:EOF :INTERRUPT :WINCH :CONT.

A lone ESC must not hang: after the escape byte we poll briefly, and if
nothing follows we report :ESCAPE rather than blocking until the user happens
to press another key."
  (let ((b (read-input-byte)))
    (cond
      ((not (integerp b)) b)
      ((= b 27)
       (if (not (sb-unix:unix-simple-poll (tty-fd) :input +escape-timeout-ms+))
           :escape
           (let ((b2 (read-input-byte)))
             (cond
               ((not (integerp b2)) b2)
               ((or (= b2 91) (= b2 79))     ; [ or O
                (or (read-csi) :unknown))
               (t (cons :meta (decode-utf8 b2)))))))
      ((= b 127) :backspace)
      ((>= b #x80) (decode-utf8 b))
      (t (code-char b)))))
