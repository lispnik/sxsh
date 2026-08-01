;;;; shell/builtins.lisp --- shell built-in utilities.
;;;;
;;;; A builtin is a function (sh args stdout-stream) -> integer exit status.
;;;; STDOUT-STREAM lets builtins participate in redirection/pipelines when we
;;;; run them with fds already dup'd; we mostly just write to the real fd 1 via
;;;; a stream over it, but for capture (command substitution) we pass a string
;;;; stream.

(in-package #:posh-shell)

(defvar *builtins* (make-hash-table :test 'equal))

(defmacro define-builtin (name (sh args out) &body body)
  `(setf (gethash ,name *builtins*)
         (lambda (,sh ,args ,out)
           (declare (ignorable ,sh ,args ,out))
           (block builtin ,@body))))

(defun builtin-p (name) (nth-value 1 (gethash name *builtins*)))
(defun find-builtin (name) (gethash name *builtins*))

;;; control-flow conditions used by break/continue/return/exit
(define-condition loop-break   () ((n :initarg :n :reader cf-n :initform 1)))
(define-condition loop-continue() ((n :initarg :n :reader cf-n :initform 1)))
(define-condition func-return  () ((code :initarg :code :reader cf-code :initform 0)))

;;; ---------------------------------------------------------------------------

(define-builtin ":" (sh args out) 0)
(define-builtin "true" (sh args out) 0)
(define-builtin "false" (sh args out) 1)

(define-builtin "echo" (sh args out)
  (let ((newline t) (interpret nil) (args args))
    ;; support -n ; (-e/-E accepted, default no-escape per XSI-agnostic choice)
    (loop while (and args (let ((a (first args)))
                            (and (>= (length a) 2) (char= (char a 0) #\-)
                                 (every (lambda (c) (member c '(#\n #\e #\E))) (subseq a 1)))))
          do (let ((a (pop args)))
               (loop for c across (subseq a 1) do
                 (case c (#\n (setf newline nil))
                         (#\e (setf interpret t))
                         (#\E (setf interpret nil))))))
    (let ((text (format nil "~{~A~^ ~}" args)))
      (write-string (if interpret (interpret-escapes text) text) out)
      (when newline (write-char #\Newline out)))
    0))

(defun interpret-escapes (s)
  (with-output-to-string (o)
    (let ((i 0) (n (length s)))
      (loop while (< i n) do
        (let ((c (char s i)))
          (if (and (char= c #\\) (< (1+ i) n))
              (progn
                (incf i)
                (case (char s i)
                  (#\n (write-char #\Newline o))
                  (#\t (write-char #\Tab o))
                  (#\r (write-char #\Return o))
                  (#\\ (write-char #\\ o))
                  (#\a (write-char (code-char 7) o))
                  (#\b (write-char (code-char 8) o))
                  (#\f (write-char (code-char 12) o))
                  (#\v (write-char (code-char 11) o))
                  (#\0 (write-char (code-char 0) o))
                  (t (write-char #\\ o) (write-char (char s i) o)))
                (incf i))
              (progn (write-char c o) (incf i))))))))

(define-builtin "printf" (sh args out)
  ;; `--' ends the options; the next word is the format even if it starts
  ;; with a dash.
  (when (and args (string= (first args) "--")) (pop args))
  (when args
    (let ((fmt (first args)) (rest (rest args)))
      (multiple-value-bind (text status) (posix-printf fmt rest)
        (write-string text out)
        (return-from builtin status))))
  0)

(defun printf-escape-at (s i n &key (octal t))
  "Decode the backslash escape starting at S[i] (which is a backslash).
Returns (values string next-index). Sharing one decoder between the format
scanner and %b is what keeps them consistent -- an earlier version tried to
expand a fixed-size window of the format and miscounted how much it had
consumed, silently swallowing the character after each escape."
  (if (>= (1+ i) n)
      (values "\\" (1+ i))
      (let ((e (char s (1+ i))))
        (case e
          (#\n (values (string #\Newline) (+ i 2)))
          (#\t (values (string #\Tab) (+ i 2)))
          (#\r (values (string #\Return) (+ i 2)))
          (#\\ (values "\\" (+ i 2)))
          (#\a (values (string (code-char 7)) (+ i 2)))
          (#\b (values (string (code-char 8)) (+ i 2)))
          (#\f (values (string (code-char 12)) (+ i 2)))
          (#\v (values (string (code-char 11)) (+ i 2)))
          (#\e (values (string (code-char 27)) (+ i 2)))
          (t
           (if (and octal (digit-char-p e 8))
               (let ((j (+ i 1)) (count 0))
                 (when (char= e #\0) (incf j))    ; optional leading 0
                 (let ((start j))
                   (loop while (and (< j n) (< count 3)
                                    (digit-char-p (char s j) 8))
                         do (incf j) (incf count))
                   (if (> j start)
                       (values (string (code-char (parse-integer s :start start
                                                                   :end j
                                                                   :radix 8)))
                               j)
                       (values (string #\Nul) j))))
               (values (concatenate 'string "\\" (string e)) (+ i 2))))))))

(defun printf-escapes (s &key (octal t))
  "Process backslash escapes in a printf format string (and in %b arguments).
OCTAL enables \\NNN / \\0NNN, which POSIX specifies for the format string."
  (with-output-to-string (o)
    (let ((i 0) (n (length s)))
      (loop while (< i n) do
        (let ((c (char s i)))
          (if (char= c #\\)
              (multiple-value-bind (str next) (printf-escape-at s i n :octal octal)
                (write-string str o)
                (setf i next))
              (progn (write-char c o) (incf i)))))))) 

(defun printf-pad (str width left-align zero-pad numericp)
  "Apply a field WIDTH to STR."
  (let ((w (or width 0)))
    (if (<= w (length str))
        str
        (let ((fill (- w (length str))))
          (cond
            (left-align (concatenate 'string str
                                     (make-string fill :initial-element #\Space)))
            ((and zero-pad numericp)
             ;; a sign stays in front of the zero padding
             (if (and (plusp (length str)) (member (char str 0) '(#\- #\+)))
                 (concatenate 'string (subseq str 0 1)
                              (make-string fill :initial-element #\0)
                              (subseq str 1))
                 (concatenate 'string (make-string fill :initial-element #\0) str)))
            (t (concatenate 'string (make-string fill :initial-element #\Space)
                            str)))))))

(defun printf-integer (arg)
  "Convert a printf argument to an integer, returning (values int okp).
POSIX: a value that is not a valid number is a diagnostic and a non-zero exit,
but printf still emits something (0) and carries on with the remaining
arguments."
  (let* ((s (string-trim '(#\Space #\Tab) (princ-to-string arg))))
    (cond
      ((string= s "") (values 0 t))
      ;; 'x or "x is the numeric value of the character
      ((and (> (length s) 1) (member (char s 0) '(#\' #\")))
       (values (char-code (char s 1)) t))
      (t (multiple-value-bind (v end)
             (ignore-errors (parse-integer s :junk-allowed t))
           (if (and v (= end (length s)))
               (values v t)
               (progn
                 (format *error-output* "printf: ~A: invalid number~%" s)
                 (values (or v 0) nil))))))))

(defun posix-printf (fmt args)
  "A printf supporting %s %b %c %d %i %o %u %x %X %% with flags, field width
and precision (including the `*' forms), recycling the format over remaining
arguments. Returns (values text status)."
  (let ((status 0))
    (values
     (with-output-to-string (o)
       (labels
           ((one-pass (args)
              (let ((i 0) (n (length fmt)) (used nil))
                (flet ((next-arg ()
                         (if args (progn (setf used t) (pop args)) "")))
                  (loop while (< i n) do
                    (let ((c (char fmt i)))
                      (cond
                        ((char= c #\\)
                         (multiple-value-bind (str next)
                             (printf-escape-at fmt i n)
                           (write-string str o)
                           (setf i next)))
                        ((char= c #\%)
                         (incf i)
                         (cond
                           ((and (< i n) (char= (char fmt i) #\%))
                            (write-char #\% o) (incf i))
                           (t
                            (let ((left nil) (zero nil) (plus nil) (space nil)
                                  (width nil) (precision nil))
                              ;; flags
                              (loop while (and (< i n)
                                               (member (char fmt i)
                                                       '(#\- #\0 #\+ #\Space #\#)))
                                    do (case (char fmt i)
                                         (#\- (setf left t))
                                         (#\0 (setf zero t))
                                         (#\+ (setf plus t))
                                         (#\Space (setf space t)))
                                       (incf i))
                              ;; width
                              (cond
                                ((and (< i n) (char= (char fmt i) #\*))
                                 (setf width (printf-integer (next-arg)))
                                 (when (and width (minusp width))
                                   (setf left t width (abs width)))
                                 (incf i))
                                (t (let ((start i))
                                     (loop while (and (< i n)
                                                      (digit-char-p (char fmt i)))
                                           do (incf i))
                                     (when (> i start)
                                       (setf width (parse-integer fmt :start start
                                                                      :end i))))))
                              ;; precision
                              (when (and (< i n) (char= (char fmt i) #\.))
                                (incf i)
                                (cond
                                  ((and (< i n) (char= (char fmt i) #\*))
                                   (setf precision (printf-integer (next-arg)))
                                   (incf i))
                                  (t (let ((start i))
                                       (loop while (and (< i n)
                                                        (digit-char-p (char fmt i)))
                                             do (incf i))
                                       (setf precision
                                             (if (> i start)
                                                 (parse-integer fmt :start start
                                                                    :end i)
                                                 0))))))
                              (when (< i n)
                                (let* ((conv (char fmt i))
                                       (arg (if (char= conv #\%) "" (next-arg))))
                                  (incf i)
                                  (multiple-value-bind (body numericp)
                                      (printf-convert conv arg precision plus
                                                      space (lambda ()
                                                              (setf status 1)))
                                    (write-string
                                     (printf-pad body width left zero numericp)
                                     o))))))))
                        (t (write-char c o) (incf i)))))
                  (values args used)))))
         (multiple-value-bind (remaining used) (one-pass args)
           (loop while (and remaining used) do
             (multiple-value-setq (remaining used) (one-pass remaining)))))) 
     status)))

(defun printf-float (arg fail)
  "Parse a printf argument as a float, diagnosing junk like the integer path."
  (let ((s (string-trim '(#\Space #\Tab) (princ-to-string arg))))
    (if (string= s "")
        0.0d0
        (let ((v (ignore-errors
                  (let ((*read-default-float-format* 'double-float))
                    (with-standard-io-syntax
                      (let ((*read-default-float-format* 'double-float))
                        (read-from-string s)))))))
          (if (numberp v)
              (coerce v 'double-float)
              (progn (format *error-output* "printf: ~A: invalid number~%" s)
                     (funcall fail)
                     0.0d0))))))

(defun printf-convert (conv arg precision plus space fail)
  "Render one conversion. Returns (values string numericp)."
  (flet ((int ()
           (multiple-value-bind (v ok) (printf-integer arg)
             (unless ok (funcall fail))
             v)))
    (case conv
      (#\s (let ((s (princ-to-string arg)))
             (values (if precision (subseq s 0 (min precision (length s))) s) nil)))
      (#\b (let ((s (printf-escapes (princ-to-string arg))))
             (values (if precision (subseq s 0 (min precision (length s))) s) nil)))
      (#\c (let ((s (princ-to-string arg)))
             (values (if (plusp (length s)) (string (char s 0)) "") nil)))
      ((#\d #\i) (let ((v (int)))
                   (values (format nil "~:[~;+~]~A"
                                   (and plus (>= v 0))
                                   (if (and space (not plus) (>= v 0))
                                       (format nil " ~D" v)
                                       (princ-to-string v)))
                           t)))
      (#\u (values (princ-to-string (abs (int))) t))
      ((#\f #\F)
       (values (format nil "~,vF" (or precision 6) (printf-float arg fail)) t))
      ((#\e #\E)
       (let ((str (format nil "~,v,2,,,,'eE" (or precision 6)
                          (printf-float arg fail))))
         (values (if (char= conv #\E) (string-upcase str) str) t)))
      ((#\g #\G)
       ;; %g drops trailing zeros; approximate with the shortest sensible form
       (let* ((v (printf-float arg fail))
              (str (if (= v (truncate v))
                       (princ-to-string (truncate v))
                       (string-right-trim "0" (format nil "~,vF"
                                                      (or precision 6) v)))))
         (values str t)))
      (#\o (values (format nil "~O" (int)) t))
      (#\x (values (format nil "~(~X~)" (int)) t))
      (#\X (values (format nil "~:@(~X~)" (int)) t))
      (t (values (princ-to-string arg) nil)))))

(define-builtin "pwd" (sh args out)
  ;; -L (default) prints the logical path, preserving symlinks; -P prints the
  ;; path with every symlink resolved.
  (let ((physical nil))
    (dolist (a args)
      (cond ((string= a "--") (return))
            ((string= a "-P") (setf physical t))
            ((string= a "-L") (setf physical nil))
            ((and (> (length a) 1) (char= (char a 0) #\-))
             (format *error-output* "pwd: ~A: invalid option~%" a)
             (return-from builtin 2))))
    (write-line (if physical (current-directory) (logical-pwd sh)) out))
  0)

(define-builtin "cd" (sh args out)
  (let ((physical nil) (rest args))
    (loop while (and rest (member (first rest) '("-L" "-P") :test #'string=))
          do (setf physical (string= (pop rest) "-P")))
    ;; `--' ends the options; what follows is the operand even if it looks
    ;; like one. Without this `cd -- /tmp' counted two arguments and failed.
    (when (and rest (string= (first rest) "--")) (pop rest))
    (when (cdr rest)
      (format *error-output* "cd: too many arguments~%")
      (return-from builtin 2))
    (let* ((arg (first rest))
           (from-cdpath nil)
           (to-oldpwd (equal arg "-"))
           (target (cond ((null arg) (or (nth-value 0 (get-var sh "HOME")) "/"))
                         (to-oldpwd
                          (or (nth-value 0 (get-var sh "OLDPWD"))
                              (progn (format *error-output* "cd: OLDPWD not set~%")
                                     (return-from builtin 1))))
                         (t arg)))
           (old (logical-pwd sh)))
      ;; CDPATH: a relative operand that is not . or .. is looked for in each
      ;; listed directory, and a match found there is echoed (POSIX 2.14 cd).
      (when (and arg (not to-oldpwd)
                 (not (char= (char target 0) #\/))
                 (not (member target '("." "..") :test #'string=))
                 (not (and (> (length target) 1)
                           (member (subseq target 0 2) '("./" "../")
                                   :test #'string=))))
        (let ((cdpath (nth-value 0 (get-var sh "CDPATH"))))
          (when (and cdpath (plusp (length cdpath)))
            (dolist (dir (split-string cdpath #\:))
              (let ((candidate (if (string= dir "")
                                   target
                                   (concatenate 'string dir "/" target))))
                (when (directoryp candidate)
                  (setf target candidate from-cdpath t)
                  (return)))))))
      (handler-case
          (let ((new
                  (if physical
                      (change-directory target)
                      ;; Logical mode: resolve the target against $PWD and
                      ;; canonicalize lexically, then chdir to THAT. Falling
                      ;; back to the raw target keeps us working when the
                      ;; lexical path does not exist (symlink chains where
                      ;; link/.. is not the same as the physical parent).
                      (let* ((raw (if (and (plusp (length target))
                                           (char= (char target 0) #\/))
                                      target
                                      (concatenate 'string old "/" target)))
                             (logical (canonicalize-logical raw)))
                        ;; Canonicalising first would let `cd BAD/..' succeed by
                        ;; cancelling a component that does not exist. POSIX
                        ;; requires every component of the operand to resolve.
                        (unless (directoryp raw)
                          (error "~A: No such file or directory" target))
                        (handler-case (progn (change-directory logical) logical)
                          (error () (change-directory target)))))))
            (setf (shell-logical-cwd sh) new)
            (set-var sh "OLDPWD" old :export t)
            (set-var sh "PWD" new :export t)
            (when (or to-oldpwd from-cdpath) (write-line new out))
            0)
        (error (e) (format *error-output* "cd: ~A~%" e) 1)))))

(define-builtin "export" (sh args out)
  ;; `export -p' is the POSIX-specified way to list exported variables in a
  ;; form that can be re-read by the shell; bare `export' is the same listing.
  (when (and args (string= (first args) "-p"))
    (setf args (rest args))
    (unless args
      (maphash (lambda (k cell)
                 (when (cdr cell)
                   (format out "export ~A=~A~%" k (shell-quote (car cell)))))
               (shell-vars sh))
      (return-from builtin 0)))
  (if (null args)
      (progn (maphash (lambda (k cell)
                        (when (cdr cell)
                          (format out "export ~A=~A~%" k (shell-quote (car cell)))))
                      (shell-vars sh))
             0)
      (progn
        (dolist (a args)
          (let ((eq (position #\= a)))
            (if eq
                (set-var sh (subseq a 0 eq) (subseq a (1+ eq)) :export t)
                (export-var sh a))))
        0)))

(define-builtin "unset" (sh args out)
  (let ((mode :auto) (names args))
    (loop while (and names (member (first names) '("-f" "-v") :test #'string=))
          do (setf mode (if (string= (pop names) "-f") :func :var)))
    (dolist (a names)
      (case mode
        (:func (remhash a (shell-functions sh)))
        (:var  (ignore-errors (unset-var sh a)))
        (:auto (if (gethash a (shell-functions sh))
                   (remhash a (shell-functions sh))
                   (ignore-errors (unset-var sh a)))))))
  0)

(define-builtin "shift" (sh args out)
  (let ((n (if args (parse-integer (first args)) 1))
        (v (shell-positional sh)))
    (if (<= n (length v))
        (progn (setf (shell-positional sh) (subseq v n)) 0)
        1)))

(define-builtin "exit" (sh args out)
  (signal 'shell-exit :code (if args (parse-integer (first args) :junk-allowed t)
                                (shell-last-status sh)))
  0)

(define-builtin "return" (sh args out)
  (signal 'func-return :code (if args (parse-integer (first args) :junk-allowed t)
                                 (shell-last-status sh)))
  0)

(define-builtin "break" (sh args out)
  (signal 'loop-break :n (if args (parse-integer (first args)) 1)) 0)
(define-builtin "continue" (sh args out)
  (signal 'loop-continue :n (if args (parse-integer (first args)) 1)) 0)

(define-builtin "read" (sh args out)
  ;; read [-r] [-p prompt] [-s] name...
  (let ((raw-mode nil) (silent nil) (prompt nil) (names args))
    (loop while (and names (plusp (length (first names)))
                     (char= (char (first names) 0) #\-)
                     (> (length (first names)) 1))
          do (let ((opt (first names)))
               (cond
                 ((string= opt "-r") (setf raw-mode t) (pop names))
                 ((string= opt "-s") (setf silent t) (pop names))
                 ((string= opt "-p")
                  (pop names)
                  (setf prompt (pop names)))
                 ((and (> (length opt) 2) (string= (subseq opt 0 2) "-p"))
                  (setf prompt (subseq opt 2)) (pop names))
                 (t (return)))))
    (when prompt (write-string prompt *error-output*) (finish-output *error-output*))
    (multiple-value-bind (line escaped eof-no-newline)
        (read-one-logical-line *standard-input* raw-mode)
      (when (eq line :eof) (return-from builtin 1))
      (when silent (terpri *error-output*))
      ;; POSIX: input ending before a newline still assigns, but the status is
      ;; non-zero so `while read line' terminates on a file with no final
      ;; newline instead of looping on the last record.
      (let ((status (if eof-no-newline 1 0)))
        (if (null names)
            (progn (set-var sh "REPLY" line) status)
            (let* ((ifs (current-ifs sh))
                   (parts (split-on-ifs line ifs (length names) escaped)))
              (loop for nm in names for i from 0
                    do (set-var sh nm (or (nth i parts) "")))
              status))))))

(defun read-one-logical-line (stream raw-mode)
  "Read one logical line for the `read' builtin.

Returns (values text escaped-positions eof-without-newline), or :eof as the
first value at end of input.

Without -r, a backslash removes the special meaning of the next character
(POSIX 2.14 `read'): `a\bc' reads as `abc', and a backslash-newline pair is a
line continuation that keeps reading. The positions of characters that were
escaped are reported because they must NOT be treated as IFS delimiters
afterwards -- `read x' on `a\ b' yields the single field `a b'.

EOF-WITHOUT-NEWLINE drives the exit status: POSIX requires a non-zero status
when input ends before a newline, even though the fields are still assigned."
  (let ((text (make-string-output-stream))
        (escaped '())
        (pos 0)
        (eof-no-newline nil)
        (got-any nil))
    (labels ((emit (ch esc)
               (write-char ch text)
               (when esc (push pos escaped))
               (incf pos)))
      (loop
        (multiple-value-bind (line missing-newline) (read-line stream nil :eof)
          (when (eq line :eof)
            (unless got-any
              (return-from read-one-logical-line (values :eof nil nil)))
            (setf eof-no-newline t)
            (return))
          (setf got-any t)
          (when missing-newline (setf eof-no-newline t))
          (cond
            (raw-mode
             (loop for ch across line do (emit ch nil))
             (return))
            (t
             (let ((i 0) (n (length line)) (continues nil))
               (loop while (< i n) do
                 (let ((c (char line i)))
                   (cond
                     ((and (char= c #\\) (< (1+ i) n))
                      (emit (char line (1+ i)) t)
                      (incf i 2))
                     ((char= c #\\)      ; trailing backslash: line continuation
                      (setf continues t)
                      (incf i))
                     (t (emit c nil) (incf i)))))
               (when (or (not continues) missing-newline) (return))))))))
    (values (get-output-stream-string text) (nreverse escaped) eof-no-newline)))

(defun split-on-ifs (line ifs maxfields &optional escaped)
  "Split LINE into at most MAXFIELDS fields using POSIX IFS rules.

IFS holds two kinds of character and they behave differently (POSIX 2.6.5):

  * IFS *whitespace* (space/tab/newline appearing in IFS) -- leading and
    trailing runs are discarded, and any run of it delimits one field.
  * IFS *delimiters* (every other character in IFS) -- each single occurrence
    delimits a field, so `a::b' with IFS=':' yields three fields, the middle
    one empty.

Treating IFS as whitespace-only, as this once did, meant `IFS=: read x y z'
performed no splitting at all and dumped the whole line into x.

Once MAXFIELDS-1 fields have been taken the rest of the line goes to the final
field verbatim (delimiters included), minus trailing IFS whitespace.

ESCAPED lists character positions that were backslash-escaped on input; those
are never delimiters, so `read x y' on `a\\ b c' gives x=\"a b\" and y=\"c\"."
  (let* ((ws '()) (delims '()))
    (loop for c across ifs
          do (if (member c '(#\Space #\Tab #\Newline))
                 (pushnew c ws)
                 (pushnew c delims)))
    (let ((ws-str (coerce ws 'string))
          (fields '()) (i 0) (n (length line)) (count 0))
      (flet ((ifs-char-p (c &optional (at -1))
               (and (not (member at escaped))
                    (or (member c ws) (member c delims))))
             (skip-ws () (loop while (and (< i n) (member (char line i) ws)
                                          (not (member i escaped)))
                               do (incf i))))
        (skip-ws)                       ; leading IFS whitespace is discarded
        (loop
          (when (>= i n) (return))
          (when (>= (1+ count) maxfields) (return))
          (let ((start i))
            (loop while (and (< i n) (not (ifs-char-p (char line i) i)))
                  do (incf i))
            (push (subseq line start i) fields)
            (incf count))
          ;; A delimiter is: an optional IFS-whitespace run, then at most one
          ;; non-whitespace IFS character, then another optional run.
          (skip-ws)
          (when (and (< i n) (member (char line i) delims)
                     (not (member i escaped)))
            (incf i)
            (skip-ws)))
        ;; whatever is left belongs to the last requested field
        (when (< i n)
          (push (string-right-trim ws-str (subseq line i)) fields)))
      (nreverse fields))))

(defparameter +shell-options+
  ;; (letter long-name keyword) -- POSIX 2.14 `set'. The long name is what
  ;; `set -o' reports and accepts; the letter is the short form.
  '((#\a "allexport" :allexport)
    (#\b "notify"    :notify)
    (#\C "noclobber" :noclobber)
    (#\e "errexit"   :errexit)
    (#\f "noglob"    :noglob)
    (#\h "hashall"   :hashall)
    (#\m "monitor"   :monitor)
    (#\n "noexec"    :noexec)
    (#\u "nounset"   :nounset)
    (#\v "verbose"   :verbose)
    (#\x "xtrace"    :xtrace)
    ;; -o only, no single-letter form
    (nil "ignoreeof" :ignoreeof)
    (nil "nolog"     :nolog)
    (nil "vi"        :vi)))

(defun option-by-letter (c) (find c +shell-options+ :key #'first))
(defun option-by-name (name) (find name +shell-options+ :key #'second
                                                        :test #'string=))

(defun set-option (sh keyword enable)
  "Apply one option. Monitor mode has side effects beyond the flag itself."
  (if (eq keyword :monitor)
      (set-monitor sh enable)
      (setf (opt sh keyword) enable)))

(defun print-options (sh out plus-form)
  "`set -o' lists options as a table; `set +o' lists them as commands that can
be re-read to restore the current settings."
  (dolist (entry +shell-options+)
    (destructuring-bind (letter name keyword) entry
      (declare (ignore letter))
      (if plus-form
          (format out "set ~:[+~;-~]o ~A~%" (opt sh keyword) name)
          (format out "~A~15T~:[off~;on~]~%" name (opt sh keyword)))))
  0)

(define-builtin "set" (sh args out)
  (cond
    ((null args)
     (maphash (lambda (k cell) (format out "~A=~A~%" k (car cell))) (shell-vars sh))
     0)
    (t
     (let ((rest args) (saw-params nil))
       (loop while rest do
         (let ((a (first rest)))
           (cond
             ((string= a "--") (pop rest) (setf saw-params t) (return))
             ((and (> (length a) 1) (member (char a 0) '(#\- #\+)))
              (let ((enable (char= (char a 0) #\-))
                    (letters (subseq a 1)))
                ;; -o / +o : long-form option, or list when no name follows
                (if (string= letters "o")
                    (let ((name (second rest)))
                      (cond
                        ((null name)
                         (print-options sh out (not enable))
                         (pop rest))
                        (t
                         (let ((entry (option-by-name name)))
                           (unless entry
                             (format *error-output*
                                     "set: ~A: invalid option name~%" name)
                             (return-from builtin 2))
                           (set-option sh (third entry) enable))
                         (pop rest) (pop rest))))
                    (progn
                      (loop for c across letters do
                        (let ((entry (option-by-letter c)))
                          (cond
                            (entry (set-option sh (third entry) enable))
                            (t (format *error-output*
                                       "set: ~C: invalid option~%" c)
                               (return-from builtin 2)))))
                      (pop rest)))))
             (t (setf saw-params t) (return)))))
       (when saw-params (set-positional sh rest)))
     0)))

(define-builtin "eval" (sh args out)
  (let ((src (format nil "~{~A~^ ~}" args)))
    (if (string= (string-trim " " src) "") 0
        (run-string-capturing sh src out))))

(define-builtin "." (sh args out)
  ;; POSIX: when the operand contains no slash the shell searches $PATH for it,
  ;; exactly as it would for a command. FIND-IN-PATH already does both halves;
  ;; the previous :allow-slash call short-circuited every name to itself, so a
  ;; bare `. helpers.sh' only ever worked from the right directory.
  (if (null args) (progn (format *error-output* ".: filename argument required~%") 2)
      (let ((path (find-source-file sh (first args))))
        (if (and path (probe-file path))
            (run-string-capturing sh (slurp-file path) out)
            (progn (format *error-output* ".: ~A: not found~%" (first args)) 1)))))

(setf (gethash "source" *builtins*) (gethash "." *builtins*))

(define-builtin "type" (sh args out)
  (dolist (a args)
    (cond ((builtin-p a) (format out "~A is a shell builtin~%" a))
          ((gethash a (shell-functions sh)) (format out "~A is a function~%" a))
          (t (let ((p (find-in-path sh a)))
               (if p (format out "~A is ~A~%" a p)
                   (format out "~A: not found~%" a))))))
  0)

;;; ---------------------------------------------------------------------------
;;; Tier-2 POSIX builtins
;;; ---------------------------------------------------------------------------

;;; alias / unalias -------------------------------------------------------
(define-builtin "alias" (sh args out)
  (if (null args)
      (progn (maphash (lambda (k v) (format out "alias ~A='~A'~%" k v))
                      (shell-aliases sh))
             0)
      (let ((status 0))
        (dolist (a args status)
          (let ((eq (position #\= a)))
            (if eq
                (setf (gethash (subseq a 0 eq) (shell-aliases sh))
                      (subseq a (1+ eq)))
                (multiple-value-bind (v found) (gethash a (shell-aliases sh))
                  (if found (format out "alias ~A='~A'~%" a v)
                      (progn (format *error-output* "alias: ~A: not found~%" a)
                             (setf status 1))))))))))

(define-builtin "unalias" (sh args out)
  (let ((status 0))
    (if (and args (string= (first args) "-a"))
        (clrhash (shell-aliases sh))
        (dolist (a args)
          (unless (remhash a (shell-aliases sh))
            (format *error-output* "unalias: ~A: not found~%" a)
            (setf status 1))))
    status))

;;; readonly --------------------------------------------------------------
(define-builtin "readonly" (sh args out)
  (if (or (null args) (and (= 1 (length args)) (string= (first args) "-p")))
      (progn (maphash (lambda (k v) (declare (ignore v))
                        (multiple-value-bind (val found) (get-var sh k)
                          (if found
                              (format out "readonly ~A=~A~%" k val)
                              (format out "readonly ~A~%" k))))
                      (shell-readonly sh))
             0)
      (progn
        (dolist (a args)
          (let ((eq (position #\= a)))
            (if eq
                (let ((name (subseq a 0 eq)))
                  (set-var sh name (subseq a (1+ eq)))
                  (mark-readonly sh name))
                (mark-readonly sh a))))
        0)))

;;; command -- run a command bypassing functions; -v/-V to describe ---------
;;; The actual "run external/builtin bypassing function lookup" behavior is
;;; handled in the executor; here we implement -v and -V, and for the plain
;;; form we signal the executor via a throw.
(define-builtin "command" (sh args out)
  (let ((verbose nil) (args args))
    (loop while (and args (member (first args) '("-v" "-V" "-p") :test #'string=))
          do (let ((o (pop args)))
               (cond ((string= o "-v") (setf verbose :v))
                     ((string= o "-V") (setf verbose :big-v)))))
    (cond
      ((null args) 0)
      (verbose
       (let ((name (first args)))
         (cond
           ((builtin-p name)
            (if (eq verbose :v) (format out "~A~%" name)
                (format out "~A is a shell builtin~%" name))
            0)
           (t (let ((p (find-in-path sh name)))
                (cond (p (if (eq verbose :v) (format out "~A~%" p)
                             (format out "~A is ~A~%" name p))
                         0)
                      (t (when (eq verbose :big-v)
                           (format *error-output* "command: ~A: not found~%" name))
                         1)))))))
      ;; plain `command cmd args`: ask the executor to run bypassing functions
      (t (throw 'run-command-bypass args)))))

;;; getopts name optstring [args...] --------------------------------------
(define-builtin "getopts" (sh args out)
  (when (< (length args) 2)
    (format *error-output* "getopts: usage: getopts optstring name [args]~%")
    (return-from builtin 2))
  (let* ((optstring (first args))
         (name (second args))
         (explicit (cddr args))
         (params (if explicit explicit
                     (coerce (shell-positional sh) 'list)))
         (optind (or (ignore-errors (parse-integer
                                     (or (nth-value 0 (get-var sh "OPTIND")) "1")))
                     1))
         (silent (and (plusp (length optstring)) (char= (char optstring 0) #\:))))
    ;; OPTIND is 1-based index into params of the NEXT arg to process
    (labels ((cur-arg () (nth (1- optind) params)))
      (let ((arg (cur-arg)))
        (when (or (null arg)
                  (zerop (length arg))
                  (char/= (char arg 0) #\-)
                  (string= arg "-"))
          ;; POSIX: when option parsing ends, getopts returns > 0 AND sets the
          ;; name to `?'. Leaving it unchanged made `while getopts ...' loops
          ;; that inspect the variable afterwards see a stale option letter.
          (set-var sh name "?")
          (set-var sh "OPTIND" (princ-to-string optind))
          (return-from builtin 1))
        (when (string= arg "--")
          (set-var sh "OPTIND" (princ-to-string (1+ optind)))
          (return-from builtin 1))
        ;; process the first option char after '-'. We track sub-position in
        ;; OPTITER (posh-internal) for bundled options like -abc.
        (let* ((subpos (or (ignore-errors
                            (parse-integer (or (nth-value 0 (get-var sh "_OPTITER"))
                                               "1")))
                           1))
               (optchar (and (< subpos (length arg)) (char arg subpos))))
          (if (null optchar)
              (progn (set-var sh "OPTIND" (princ-to-string (1+ optind)))
                     (set-var sh "_OPTITER" "1")
                     (funcall (find-builtin "getopts") sh args out))
              (let ((spec (position optchar optstring)))
                (cond
                  ((null spec)
                   (set-var sh name "?")
                   (unless silent
                     (format *error-output* "getopts: illegal option -- ~A~%" optchar))
                   (when silent (set-var sh "OPTARG" (string optchar)))
                   (advance-getopts sh arg subpos optind)
                   0)
                  ;; option takes an argument?
                  ((and (< (1+ spec) (length optstring))
                        (char= (char optstring (1+ spec)) #\:))
                   (let ((rest (subseq arg (1+ subpos))))
                     (if (plusp (length rest))
                         (progn (set-var sh name (string optchar))
                                (set-var sh "OPTARG" rest)
                                (set-var sh "OPTIND" (princ-to-string (1+ optind)))
                                (set-var sh "_OPTITER" "1")
                                0)
                         (let ((next (nth optind params)))
                           (if next
                               (progn (set-var sh name (string optchar))
                                      (set-var sh "OPTARG" next)
                                      (set-var sh "OPTIND"
                                               (princ-to-string (+ 2 optind)))
                                      (set-var sh "_OPTITER" "1")
                                      0)
                               (progn
                                 (set-var sh name (if silent ":" "?"))
                                 (if silent (set-var sh "OPTARG" (string optchar))
                                     (format *error-output*
                                             "getopts: option requires an argument -- ~A~%"
                                             optchar))
                                 (set-var sh "OPTIND" (princ-to-string (1+ optind)))
                                 (set-var sh "_OPTITER" "1")
                                 0))))))
                  ;; simple flag
                  (t (set-var sh name (string optchar))
                     (set-var sh "OPTARG" "")
                     (advance-getopts sh arg subpos optind)
                     0)))))))))

(defun advance-getopts (sh arg subpos optind)
  "Advance either the sub-position within a bundled option word, or OPTIND."
  (if (< (1+ subpos) (length arg))
      (set-var sh "_OPTITER" (princ-to-string (1+ subpos)))
      (progn (set-var sh "OPTIND" (princ-to-string (1+ optind)))
             (set-var sh "_OPTITER" "1"))))

;;; trap [action] condition... --------------------------------------------
(defun trap-display-name (cond-name)
  "How a condition is shown by `trap'. Signals get their full SIGxxx name;
EXIT is not a signal and keeps its bare name."
  (if (string= cond-name "EXIT") "EXIT" (concatenate 'string "SIG" cond-name)))

(defun trap-sort-key (cond-name)
  "Ordering key for a trap listing: EXIT is condition 0, signals sort by number."
  (if (string= cond-name "EXIT") 0 (or (signal-number cond-name) 1000)))

(defun print-traps (sh out &optional conds)
  "List traps in signal-number order, EXIT first.

Hash-table order is arbitrary, so the listing came out differently from run to
run -- useless for a script that diffs `trap' output, and not what any other
shell does."
  (flet ((show (k v) (format out "trap -- '~A' ~A~%" v (trap-display-name k))))
    (if conds
        (dolist (c conds)
          (let ((k (normalize-signal c)))
            (multiple-value-bind (v found) (gethash k (shell-traps sh))
              (when found (show k v)))))
        (let ((entries '()))
          (maphash (lambda (k v) (push (cons k v) entries)) (shell-traps sh))
          (dolist (e (sort entries #'< :key (lambda (e) (trap-sort-key (car e)))))
            (show (car e) (cdr e))))))
  0)

(defun valid-condition-p (name)
  "True if NAME designates a trappable condition (a signal or EXIT)."
  (let ((norm (normalize-signal name)))
    (or (string= norm "EXIT") (and (signal-number norm) t))))

(define-builtin "trap" (sh args out)
  ;; `--' is accepted and ignored, as for any POSIX utility.
  (when (and args (string= (first args) "--")) (pop args))
  (cond
    ((null args) (print-traps sh out))
    ((string= (first args) "-p") (print-traps sh out (rest args)))
    ;; `trap EXIT' / `trap 0 2' -- operands only, no action. POSIX says a
    ;; leading unsigned integer means every operand is a condition; shells
    ;; extend that to names. Treating the first operand as an action here
    ;; meant `trap EXIT' silently kept the handler it was meant to remove.
    ((every #'valid-condition-p args)
     (dolist (c args)
       (let ((sig (normalize-signal c)))
         (remhash sig (shell-traps sh))
         (uninstall-signal-handler sig)))
     0)
    (t
     (let ((action (first args)) (conds (rest args)))
       (when (null conds)
         (format *error-output* "trap: usage: trap [action] condition ...~%")
         (return-from builtin 2))
       (dolist (c conds)
         (unless (valid-condition-p c)
           (format *error-output* "trap: ~A: bad signal specification~%" c)
           (return-from builtin 1)))
       (dolist (c conds)
         (let ((sig (normalize-signal c)))
           (cond
             ((string= action "-")
              (remhash sig (shell-traps sh))
              (uninstall-signal-handler sig))
             ((string= action "")
              ;; ignore the signal
              (setf (gethash sig (shell-traps sh)) "")
              (install-signal-handler sh sig))
             (t (setf (gethash sig (shell-traps sh)) action)
                (install-signal-handler sh sig)))))
       0))))

(defparameter +signal-names+
  ;; name -> number, for the common signals a shell traps. Every entry comes
  ;; from sb-unix: the job-control signals in particular are numbered
  ;; differently on Linux and macOS, so literals silently trap the wrong one.
  `(("HUP" . ,sb-unix:sighup) ("INT" . ,sb-unix:sigint)
    ("QUIT" . ,sb-unix:sigquit) ("ILL" . ,sb-unix:sigill)
    ("TRAP" . ,sb-unix:sigtrap) ("ABRT" . 6)
    ("FPE" . ,sb-unix:sigfpe) ("KILL" . ,sb-unix:sigkill)
    ("BUS" . ,sb-unix:sigbus) ("SEGV" . ,sb-unix:sigsegv)
    ("SYS" . ,sb-unix:sigsys) ("PIPE" . ,sb-unix:sigpipe)
    ("ALRM" . ,sb-unix:sigalrm) ("TERM" . ,sb-unix:sigterm)
    ("URG" . ,sb-unix:sigurg) ("STOP" . ,sb-unix:sigstop)
    ("TSTP" . ,sb-unix:sigtstp) ("CONT" . ,sb-unix:sigcont)
    ("CHLD" . ,sb-unix:sigchld) ("TTIN" . ,sb-unix:sigttin)
    ("TTOU" . ,sb-unix:sigttou) ("IO" . ,sb-unix:sigio)
    ("XCPU" . ,sb-unix:sigxcpu) ("XFSZ" . ,sb-unix:sigxfsz)
    ("VTALRM" . ,sb-unix:sigvtalrm) ("PROF" . ,sb-unix:sigprof)
    ("WINCH" . ,sb-unix:sigwinch)
    ("USR1" . ,sb-unix:sigusr1) ("USR2" . ,sb-unix:sigusr2)))

(defun signal-number (name)
  "Map a normalized signal NAME to its number, or NIL for pseudo-conditions
like EXIT."
  (cdr (assoc name +signal-names+ :test #'string=)))

(defun install-signal-handler (sh sig-name)
  "Install a handler for SIG-NAME (normalized) that records the signal as
pending so the shell can run its trap action at the next safe point. EXIT and
unknown names are not real signals -- nothing to install."
  (let ((num (signal-number sig-name)))
    (when num
      (sb-sys:enable-interrupt
       num
       (lambda (signo info context)
         (declare (ignore signo info context))
         (pushnew sig-name (shell-pending-signals sh) :test #'string=))))))

(defun uninstall-signal-handler (sig-name)
  "Restore the default action for SIG-NAME."
  (let ((num (signal-number sig-name)))
    (when num
      (sb-sys:enable-interrupt num :default))))

(defun normalize-signal (name)
  "Normalize a signal/condition designator to a canonical string, e.g.
'0'/'EXIT' -> \"EXIT\"; 'sigint'/'INT'/'2' -> \"INT\"."
  (let ((u (string-upcase name)))
    (cond
      ((string= u "0") "EXIT")
      ((string= u "EXIT") "EXIT")
      ((and (>= (length u) 3) (string= (subseq u 0 3) "SIG")) (subseq u 3))
      ;; numeric signal designator. The length test matters: EVERY is true of
      ;; the empty string, so `trap "" INT' reached PARSE-INTEGER with "".
      ((and (plusp (length u)) (every #'digit-char-p u))
       (let ((n (parse-integer u)))
         (or (car (rassoc n +signal-names+)) u)))
      (t u))))

;;; wait [pid|%job...] ----------------------------------------------------
(define-builtin "wait" (sh args out)
  (let ((status 0))
    (if args
        (dolist (a args)
          (let ((job (and (plusp (length a)) (char= (char a 0) #\%)
                          (find-job sh a))))
            (cond
              (job (setf status (wait-for-job sh job)))
              (t (let ((pid (ignore-errors (parse-integer a))))
                   (when pid
                     (let ((j (find-if (lambda (jj) (member pid (job-pids jj)))
                                       (shell-jobs sh))))
                       (if j (setf status (wait-for-job sh j))
                           (setf status (posh-shell::wait-and-decode pid))))
                     (setf (shell-bg-pids sh) (remove pid (shell-bg-pids sh)))))))))
        ;; no args: wait for all jobs and loose bg pids
        (progn
          (dolist (job (copy-list (shell-jobs sh)))
            (when (eq (job-state job) :running)
              (setf status (wait-for-job sh job))))
          (dolist (pid (shell-bg-pids sh))
            (setf status (posh-shell::wait-and-decode pid)))
          (setf (shell-bg-pids sh) '())))
    status))

;;; kill [-s sig | -SIG] pid|%job ... / kill -l [status] ------------------
;;;
;;; kill must be a builtin, not /bin/kill: only the shell knows the job table,
;;; so only the shell can resolve `%1' -- and a job spec names a whole process
;;; group, which is what makes `kill %1' stop a pipeline rather than one stage.

(defun parse-signal-spec (spec)
  "Resolve a signal designator (\"TERM\", \"SIGTERM\", \"15\") to a number, or
NIL if unknown."
  (if (every #'digit-char-p spec)
      (let ((n (ignore-errors (parse-integer spec))))
        (and n (<= 0 n 64) n))
      (signal-number (normalize-signal spec))))

(defun kill-target (sh spec sig)
  "Send SIG to SPEC: a job spec (%1, %%, %prefix), a pid, or a negative pid
meaning that process group. Returns T on success, NIL after reporting."
  (handler-case
      (cond
        ((and (plusp (length spec)) (char= (char spec 0) #\%))
         (let ((job (find-job sh spec)))
           (cond
             ((null job)
              (format *error-output* "kill: ~A: no such job~%" spec) nil)
             ;; whole process group, so every stage of a pipeline gets it
             ((job-pgid job) (%killpg (job-pgid job) sig) t)
             (t (dolist (p (job-pids job) t) (sb-posix:kill p sig))))))
        (t (let ((pid (parse-integer spec)))
             (if (minusp pid)
                 (progn (%killpg (- pid) sig) t)
                 (progn (sb-posix:kill pid sig) t)))))
    (error (e)
      (format *error-output* "kill: ~A: ~A~%" spec e)
      nil)))

(defun kill-list-signals (args out)
  (cond
    ((null args)
     (format out "~{~A~^ ~}~%" (mapcar #'car +signal-names+))
     0)
    (t (let ((n (ignore-errors (parse-integer (first args)))))
         ;; a wait-status over 128 designates the signal that terminated it
         (when (and n (> n 128)) (setf n (- n 128)))
         (let ((name (and n (car (rassoc n +signal-names+)))))
           (cond (name (write-line name out) 0)
                 (t (format *error-output* "kill: ~A: invalid signal~%"
                            (first args))
                    1)))))))

(define-builtin "kill" (sh args out)
  (let ((sig (signal-number "TERM")) (rest args))
    (when (and rest (string= (first rest) "-l"))
      (return-from builtin (kill-list-signals (cdr rest) out)))
    (loop while rest do
      (let ((a (first rest)))
        (cond
          ((string= a "--") (pop rest) (return))
          ((string= a "-s")
           (pop rest)
           (let ((n (and rest (parse-signal-spec (first rest)))))
             (unless n
               (format *error-output* "kill: ~A: invalid signal~%"
                       (or (first rest) ""))
               (return-from builtin 1))
             (setf sig n)
             (pop rest)))
          ((and (> (length a) 1) (char= (char a 0) #\-))
           (let ((n (parse-signal-spec (subseq a 1))))
             (unless n
               (format *error-output* "kill: ~A: invalid signal~%" (subseq a 1))
               (return-from builtin 1))
             (setf sig n)
             (pop rest)))
          (t (return)))))
    (cond
      ((null rest)
       (format *error-output*
               "kill: usage: kill [-s signal | -signal] pid | %job ...~%")
       2)
      (t (let ((status 0))
           (dolist (tgt rest status)
             (unless (kill-target sh tgt sig) (setf status 1))))))))

;;; jobs [-l] -------------------------------------------------------------
(define-builtin "jobs" (sh args out)
  (poll-jobs sh)
  (let ((show-pgid (and args (string= (first args) "-l"))))
    (dolist (job (reverse (shell-jobs sh)))
      (print-job sh job out :show-pgid show-pgid))
    ;; done jobs are reported once then forgotten
    (dolist (job (copy-list (shell-jobs sh)))
      (when (eq (job-state job) :done) (remove-job sh job))))
  0)

;;; fg [%job] -------------------------------------------------------------
(define-builtin "fg" (sh args out)
  (let ((job (find-job sh (first args))))
    (if (null job)
        (progn (format *error-output* "fg: no such job~%") 1)
        (progn
          (format out "~A~%" (job-command job))
          ;; resume if stopped, then wait in the foreground
          (when (eq (job-state job) :stopped)
            (continue-job job)
            (setf (job-state job) :running))
          (fg-give-terminal sh job)
          (prog1 (wait-for-job sh job)
            (fg-reclaim-terminal sh))))))

;;; bg [%job] -------------------------------------------------------------
(define-builtin "bg" (sh args out)
  (let ((job (find-job sh (first args))))
    (if (null job)
        (progn (format *error-output* "bg: no such job~%") 1)
        (progn
          (when (eq (job-state job) :stopped)
            (continue-job job)
            (setf (job-state job) :running))
          (format out "[~D] ~A~%" (job-id job) (job-command job))
          0))))

;;; umask [mask] ----------------------------------------------------------
(define-builtin "umask" (sh args out)
  (if (null args)
      (progn (let ((m (sb-posix:umask 0)))
               (sb-posix:umask m)     ; restore
               (format out "~4,'0O~%" m))
             0)
      (let ((spec (first args)))
        (handler-case
            (let ((val (parse-integer spec :radix 8)))
              (sb-posix:umask val) 0)
          (error () (format *error-output* "umask: ~A: invalid mask~%" spec) 1)))))

;;; hash -- command location cache; we keep a minimal real cache -----------
(define-builtin "hash" (sh args out)
  (cond
    ((null args)
     ;; print nothing meaningful (empty cache is acceptable)
     0)
    ((string= (first args) "-r") 0)     ; clear cache: no-op (we don't cache)
    (t (let ((status 0))
         (dolist (a args status)
           (unless (find-in-path sh a)
             (format *error-output* "hash: ~A: not found~%" a)
             (setf status 1)))))))

;;; times -----------------------------------------------------------------
(define-builtin "times" (sh args out)
  ;; POSIX: two lines. Line 1 = shell (RUSAGE_SELF) user & system time,
  ;; line 2 = children (RUSAGE_CHILDREN). The kernel accumulates children's
  ;; CPU time as they are reaped, so this reflects all waited-for children.
  (multiple-value-bind (self-u self-s) (rusage-times sb-unix:rusage_self)
    (multiple-value-bind (chld-u chld-s) (rusage-times sb-unix:rusage_children)
      (format out "~A ~A~%~A ~A~%"
              (format-cpu-time self-u) (format-cpu-time self-s)
              (format-cpu-time chld-u) (format-cpu-time chld-s))))
  0)

(defun rusage-times (who)
  "Return (values user-seconds system-seconds) for WHO (RUSAGE_SELF or
RUSAGE_CHILDREN) as rationals. SBCL's unix-getrusage yields user microseconds
as its 1st value and system microseconds as its 3rd."
  (multiple-value-bind (ok utime-us s2 stime-us) (sb-unix:unix-getrusage who)
    (declare (ignore s2))
    (if ok
        (values (/ utime-us 1000000) (/ stime-us 1000000))
        (values 0 0))))

(defun format-cpu-time (seconds)
  "Format SECONDS (a rational) as POSIX times output: 'MmS.SSSs'."
  (multiple-value-bind (m s) (floor seconds 60)
    (format nil "~Dm~,3Fs" m (float s 1.0d0))))

;;; exec -- replace the shell process, or apply permanent redirections -----
;;; Handled by the executor (needs the redirect nodes and to not return); the
;;; builtin form here is only reached with no command, so it applies its
;;; redirections permanently by throwing to the executor.
(define-builtin "exec" (sh args out)
  (throw 'exec-builtin args))

;;; test / [ --------------------------------------------------------------

(define-builtin "test" (sh args out)
  (handler-case (if (eval-test args) 0 1)
    (test-usage-error (e)
      (format *error-output* "test: ~A~%" e)
      2)))
(define-builtin "[" (sh args out)
  (let ((a args))
    (unless (and a (string= (car (last a)) "]"))
      (format *error-output* "[: missing ]~%") (return-from builtin 2))
    (handler-case (if (eval-test (butlast a)) 0 1)
      (test-usage-error (e)
        (format *error-output* "[: ~A~%" e)
        2))))

(define-condition test-usage-error (error)
  ((detail :initarg :detail :reader test-usage-detail))
  (:report (lambda (c s) (format s "~A" (test-usage-detail c)))))

(defun test-access (path mode)
  (handler-case (progn (sb-posix:access path mode) t)
    (error () nil)))

(defun test-symlink-p (path)
  (handler-case (sb-posix:s-islnk (sb-posix:stat-mode (sb-posix:lstat path)))
    (error () nil)))

(defun eval-test (args)
  "Evaluate a test expression (subset of POSIX test)."
  (labels ((str-nonempty (s) (and s (plusp (length s))))
           (num (s)
             ;; POSIX: a non-integer operand to -eq and friends is a usage
             ;; error (status 2), not the integer 0. Silently coercing meant
             ;; `[ $x -eq 1 ]' with x unset or textual quietly compared 0.
             (let ((v (ignore-errors
                       (multiple-value-bind (v end)
                           (parse-integer (string-trim " " s) :junk-allowed t)
                         (and v (= end (length (string-trim " " s))) v)))))
               (or v (error 'test-usage-error
                            :detail (format nil "~A: integer expression expected"
                                            s))))))
    (case (length args)
      (0 nil)
      (1 (str-nonempty (first args)))
      (2 (let ((op (first args)) (a (second args)))
           (cond ((string= op "!") (not (str-nonempty a)))
                 ((string= op "-n") (str-nonempty a))
                 ((string= op "-z") (not (str-nonempty a)))
                 ((string= op "-e") (and (probe-file a) t))
                 ((string= op "-f") (and (probe-file a)
                                         (not (directoryp a))))
                 ((string= op "-d") (directoryp a))
                 ((string= op "-r") (test-access a sb-posix:r-ok))
                 ((string= op "-w") (test-access a sb-posix:w-ok))
                 ((string= op "-x") (test-access a sb-posix:x-ok))
                 ((or (string= op "-L") (string= op "-h")) (test-symlink-p a))
                 ((string= op "-t")
                  (let ((fd (ignore-errors (parse-integer a))))
                    (and fd (plusp (sb-unix:unix-isatty fd)))))
                 ((string= op "-s") (let ((f (probe-file a)))
                                      (and f (plusp (with-open-file (s f) (file-length s))))))
                 (t (str-nonempty (second args))))))
      (3 (let ((a (first args)) (op (second args)) (b (third args)))
           (cond
             ((string= op "=")  (string= a b))
             ((string= op "==") (string= a b))
             ((string= op "!=") (not (string= a b)))
             ((string= op "-eq") (= (num a) (num b)))
             ((string= op "-ne") (/= (num a) (num b)))
             ((string= op "-lt") (< (num a) (num b)))
             ((string= op "-le") (<= (num a) (num b)))
             ((string= op "-gt") (> (num a) (num b)))
             ((string= op "-ge") (>= (num a) (num b)))
             ((string= a "!") (not (eval-test (rest args))))
             (t nil))))
      (t (cond
           ((string= (first args) "!") (not (eval-test (rest args))))
           ;; a -a b / a -o b (deprecated but common)
           ((member "-a" args :test #'string=)
            (let ((pos (position "-a" args :test #'string=)))
              (and (eval-test (subseq args 0 pos))
                   (eval-test (subseq args (1+ pos))))))
           ((member "-o" args :test #'string=)
            (let ((pos (position "-o" args :test #'string=)))
              (or (eval-test (subseq args 0 pos))
                  (eval-test (subseq args (1+ pos))))))
           (t nil))))))

(defun directoryp (path)
  (let ((tn (ignore-errors (truename path))))
    (and tn (null (pathname-name tn)) (null (pathname-type tn)))))
