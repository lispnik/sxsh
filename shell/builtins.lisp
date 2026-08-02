;;;; shell/builtins.lisp --- shell built-in utilities.
;;;;
;;;; A builtin is a function (sh args stdout-stream) -> integer exit status.
;;;; STDOUT-STREAM lets builtins participate in redirection/pipelines when we
;;;; run them with fds already dup'd; we mostly just write to the real fd 1 via
;;;; a stream over it, but for capture (command substitution) we pass a string
;;;; stream.

(in-package #:sxsh-shell)

(defvar *builtins* (make-hash-table :test 'equal))

(defvar *builtin-help* (make-hash-table :test 'equal)
  "NAME -> (synopsis summary description), populated by DEFINE-BUILTIN.

The text lives at each builtin's definition site so it cannot drift far from
the argument parsing it describes; TEST-SHELL asserts that this table and
*BUILTINS* have identical key sets, so a new builtin without help fails the
suite rather than surprising someone at the prompt.

Every word of it is written from THIS shell's source. bash's help strings are
GPL and sxsh is MIT, and they would be wrong anyway: our `cd' has no -@, our
`read -n' counts characters after escape processing, our `echo -e' set is its
own. Document what the code accepts.")

(defmacro define-builtin (name (sh args out) &body body)
  "Define a builtin. BODY may begin with :SYNOPSIS/:SUMMARY/:DESCRIPTION pairs,
which are recorded for `help' and stripped before the body proper."
  (let ((help '()))
    (loop while (and (keywordp (first body))
                     (member (first body) '(:synopsis :summary :description))
                     (rest body))
          do (setf (getf help (pop body)) (pop body)))
    `(progn
       ,@(when help
           `((setf (gethash ,name *builtin-help*)
                   (list ,(getf help :synopsis)
                         ,(getf help :summary)
                         ,(getf help :description)))))
       (setf (gethash ,name *builtins*)
             (lambda (,sh ,args ,out)
               (declare (ignorable ,sh ,args ,out))
               (block builtin ,@body))))))

(defun builtin-p (name) (nth-value 1 (gethash name *builtins*)))
(defun find-builtin (name) (gethash name *builtins*))

(defun copy-builtin-help (from to &key synopsis)
  "Give TO its own help record, based on FROM's. Aliases share a function but
need an entry of their own, and `[' needs a different synopsis from `test'."
  (let ((h (gethash from *builtin-help*)))
    (when h
      (setf (gethash to *builtin-help*)
            (list (or synopsis (first h)) (second h) (third h))))))

;;; control-flow conditions used by break/continue/return/exit
(define-condition loop-break   () ((n :initarg :n :reader cf-n :initform 1)))
(define-condition loop-continue() ((n :initarg :n :reader cf-n :initform 1)))
(define-condition func-return  () ((code :initarg :code :reader cf-code :initform 0)))

;;; ---------------------------------------------------------------------------

(define-builtin ":" (sh args out)
  :synopsis ":"
  :summary "Null command."
  :description "Expands its arguments and performs any redirections, then does nothing else.

Exit Status:
Always zero."
  0)
(define-builtin "true" (sh args out)
  :synopsis "true"
  :summary "Return a successful result."
  :description "Does nothing and succeeds. Arguments are ignored.

Exit Status:
Always zero."
  0)
(define-builtin "false" (sh args out)
  :synopsis "false"
  :summary "Return an unsuccessful result."
  :description "Does nothing and fails. Arguments are ignored.

Exit Status:
Always 1."
  1)

(define-builtin "echo" (sh args out)
  :synopsis "echo [-neE] [ARG ...]"
  :summary "Write arguments to standard output."
  :description "Writes ARGs to standard output, separated by single spaces and followed by a
newline.

Options:
  -n  do not append the trailing newline
  -e  interpret the backslash escapes below
  -E  do not interpret backslash escapes (the default)

With -e the escapes are:
  \\a alert    \\b backspace   \\e escape    \\f form feed
  \\n newline  \\r carriage return   \\t tab   \\v vertical tab
  \\\\ backslash
  \\c stop output here and suppress the trailing newline
  \\0NNN  the byte whose value is the 1-3 octal digits NNN (the leading
         zero is required; a bare \\NNN is literal)
  \\xHH   the byte whose value is the 1-2 hex digits HH
  \\uHHHH, \\UHHHHHHHH  the character with that codepoint

Options are only recognised while every character after the `-' is one of
n, e or E; anything else is written out as an ordinary argument.

Exit Status:
Zero unless a write fails."
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
      (if interpret
          (multiple-value-bind (decoded stopped) (interpret-escapes text)
            (write-string decoded out)
            ;; \c stops processing AND suppresses the trailing newline.
            (when stopped (setf newline nil)))
          (write-string text out))
      (when newline (write-char #\Newline out)))
    0))

(defun interpret-escapes (s)
  "Decode `echo -e' escapes. Returns (values string stopped-p).

Shares DECODE-ESCAPE-AT with printf; the one difference is that echo requires
the leading zero on an octal escape -- `\\0044' is a byte but `\\44' is literal --
whereas the printf FORMAT string accepts a bare `\\44'."
  (let ((stopped nil))
    (values
     (with-output-to-string (o)
       (let ((i 0) (n (length s)))
         (loop while (< i n) do
           (let ((c (char s i)))
             (if (char= c #\\)
                 (multiple-value-bind (str next)
                     (decode-escape-at s i n :octal :leading-zero)
                   (when (eq str :stop) (setf stopped t) (return))
                   (write-string str o)
                   (setf i next))
                 (progn (write-char c o) (incf i)))))))
     stopped)))

(define-builtin "printf" (sh args out)
  :synopsis "printf [-v VAR] FORMAT [ARGUMENT ...]"
  :summary "Format and print arguments."
  :description "Writes ARGUMENTs formatted under the control of FORMAT.

Options:
  -v VAR  assign the result to shell variable VAR instead of printing it

FORMAT is reused until all ARGUMENTs are consumed; missing arguments are
treated as empty or zero. In addition to the C conversions (%d %i %o %u %x %X
%c %s %f %e %g %%) with the usual flags, width and precision:
  %b  the argument with backslash escapes interpreted, as `echo -e' does
  %q  the argument quoted so it can be reused as shell input

A `*' width or precision takes its value from the next argument.

Exit Status:
Zero unless an invalid format or number is given, or a write fails."
  ;; bash `-v NAME': put the result in a variable instead of on stdout.
  (let ((target nil))
    (loop while (and args (>= (length (first args)) 2)
                     (string= (subseq (first args) 0 2) "-v"))
          do (let ((a (pop args)))
               (setf target (if (> (length a) 2) (subseq a 2) (pop args)))))
    ;; `--' ends the options; the next word is the format even if it starts
    ;; with a dash.
    (when (and args (string= (first args) "--")) (pop args))
    (when args
      (let ((fmt (first args)) (rest (rest args)))
        (multiple-value-bind (text status) (posix-printf fmt rest)
          (if target
              (set-var sh target text)
              (write-string text out))
          (return-from builtin status)))))
  0)

(defparameter +printf-q-escape+ " !\"$&'()*,;<>?[\\]^`{|}"
  "Characters bash's %q escapes with a backslash. Determined by probing bash
rather than guessed -- the set is not the same as the shell metacharacters,
and notably excludes # ~ = % and :.")

(defun printf-quote (s)
  "bash %q: render S so it re-parses to itself.

Empty becomes '', anything containing a control character uses the $'...'
form, and otherwise each special character is backslash-escaped. That last
choice is what makes the output match bash textually; SHELL-QUOTE's
single-quoted form would be equally correct shell but a different string."
  (cond
    ((string= s "") "''")
    ((some (lambda (c) (< (char-code c) 32)) s)
     (with-output-to-string (o)
       (write-string "$'" o)
       (loop for c across s do
         (case c
           (#\Newline (write-string "\\n" o))
           (#\Tab (write-string "\\t" o))
           (#\Return (write-string "\\r" o))
           (t (if (< (char-code c) 32)
                  (format o "\\x~2,'0x" (char-code c))
                  (progn (when (find c "'\\") (write-char #\\ o))
                         (write-char c o))))))
       (write-char #\' o)))
    (t (with-output-to-string (o)
         (loop for c across s do
           (when (find c +printf-q-escape+) (write-char #\\ o))
           (write-char c o))))))

(defun hex-run (s start n limit)
  "End index of at most LIMIT hex digits beginning at START."
  (let ((j start))
    (loop while (and (< j n) (< (- j start) limit)
                     (digit-char-p (char s j) 16))
          do (incf j))
    j))

(defun decode-escape-at (s i n &key (octal t))
  "Decode the backslash escape starting at S[i] (which is a backslash).
Returns (values string next-index), or (values :stop nil) for \\c.

OCTAL selects the octal syntax, which is the ONE place echo and printf differ:
  t              a bare \\NNN or \\0NNN   (the printf format string)
  :leading-zero  only \\0NNN              (echo -e, and printf %b)
  nil            no octal escapes

Sharing one decoder between the format scanner, %b and echo is what keeps them
consistent -- an earlier version tried to expand a fixed-size window of the
format and miscounted how much it had consumed, silently swallowing the
character after each escape.

\\x, \\u and \\U differ in kind: \\xHH and the octal forms name a BYTE, so their
value is masked to 8 bits, while \\uHHHH names a codepoint that is encoded on
output. bash draws the same distinction."
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
          (#\c (values :stop nil))
          ;; \x with no digit at all stays literal, so `echo -e \x' prints \x.
          (#\x (let ((j (hex-run s (+ i 2) n 2)))
                 (if (> j (+ i 2))
                     (values (string (code-char
                                      (logand (parse-integer s :start (+ i 2)
                                                               :end j :radix 16)
                                              #xFF)))
                             j)
                     (values "\\x" (+ i 2)))))
          ((#\u #\U)
           (let* ((limit (if (char= e #\u) 4 8))
                  (j (hex-run s (+ i 2) n limit)))
             (if (> j (+ i 2))
                 (values (string (code-char (parse-integer s :start (+ i 2)
                                                             :end j :radix 16)))
                         j)
                 (values (concatenate 'string "\\" (string e)) (+ i 2)))))
          (t
           (if (and octal (digit-char-p e 8)
                    (or (not (eq octal :leading-zero)) (char= e #\0)))
               (let ((j (1+ i)) (count 0))
                 (when (char= e #\0) (incf j))    ; the leading 0 is not a digit
                 (let ((start j))
                   (loop while (and (< j n) (< count 3)
                                    (digit-char-p (char s j) 8))
                         do (incf j) (incf count))
                   (if (> j start)
                       ;; \0400 and \0777 exceed a byte; bash keeps the low 8
                       ;; bits rather than clamping or erroring.
                       (values (string (code-char
                                        (logand (parse-integer s :start start
                                                                 :end j :radix 8)
                                                #xFF)))
                               j)
                       (values (string #\Nul) j))))
               (values (concatenate 'string "\\" (string e)) (+ i 2))))))))

(defun printf-escape-at (s i n &key (octal t))
  "Backwards-compatible name used by the printf format scanner."
  (multiple-value-bind (str next) (decode-escape-at s i n :octal octal)
    (if (eq str :stop) (values "" n) (values str next))))

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
      (#\q (values (printf-quote (princ-to-string arg)) nil))
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
  :synopsis "pwd [-LP]"
  :summary "Print the name of the current working directory."
  :description "Options:
  -L  print the logical path, keeping any symbolic links traversed to get
      here (the default)
  -P  print the physical path, with all symbolic links resolved

Exit Status:
Zero unless an invalid option is given or the directory cannot be read."
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
  :synopsis "cd [-L|-P] [DIR]"
  :summary "Change the shell working directory."
  :description "Changes the current directory to DIR. With no DIR, $HOME is used. A DIR of
`-' means $OLDPWD, and the new directory is printed.

Options:
  -L  treat symbolic links in the path as-is, so `cd ..' from a symlinked
      directory returns to where you came from (the default)
  -P  resolve symbolic links first, so `..' is the physical parent

If DIR is relative, is not `.' or `..', and does not begin with `./' or
`../', the directories in $CDPATH are searched for it; a directory found that
way is printed.

$PWD and $OLDPWD are updated on success.

Exit Status:
Zero if the directory is changed; non-zero otherwise. An empty DIR is an
error rather than a synonym for $HOME."
  (let ((physical nil) (rest args))
    (loop while (and rest (member (first rest) '("-L" "-P") :test #'string=))
          do (setf physical (string= (pop rest) "-P")))
    ;; `--' ends the options; what follows is the operand even if it looks
    ;; like one. Without this `cd -- /tmp' counted two arguments and failed.
    (when (and rest (string= (first rest) "--")) (pop rest))
    (when (cdr rest)
      (format *error-output* "cd: too many arguments~%")
      (return-from builtin 2))
    ;; An explicitly empty operand is an error, not a no-op and not $HOME.
    ;; It also cannot reach the CDPATH test below, which indexes character 0.
    (when (equal (first rest) "")
      (format *error-output* "cd: null directory~%")
      (return-from builtin 1))
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
  :synopsis "export [-p] [NAME[=VALUE] ...]"
  :summary "Mark names for export to the environment of later commands."
  :description "Marks each NAME for export, so that it appears in the environment of every
command run afterwards. With NAME=VALUE, the assignment is performed first.

Options:
  -p  list the exported names and their values, in a form that can be reused
      as input

`export' is a declaration utility: the value in NAME=VALUE is expanded as an
assignment would be, so it is not field-split or glob-expanded, and a tilde
after the `=' or after an unquoted `:' is expanded.

Exit Status:
Zero unless an invalid name is given."
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
  :synopsis "unset [-fv] [NAME ...]"
  :summary "Unset values and attributes of variables and functions."
  :description "Removes each NAME.

Options:
  -f  treat every NAME as a function
  -v  treat every NAME as a variable
With neither, a function is removed only if no variable of that name exists.

`unset ARRAY[N]' removes one element and leaves the array, which becomes
sparse rather than renumbered; `unset ARRAY[@]' removes the whole array.

Unsetting a name that a `VAR=value command' prefix is currently shadowing
reveals the value underneath rather than removing both.

Exit Status:
Zero unless a NAME is readonly."
  (let ((mode :auto) (names args))
    (loop while (and names (member (first names) '("-f" "-v") :test #'string=))
          do (setf mode (if (string= (pop names) "-f") :func :var)))
    (dolist (a names)
      ;; bash `unset a[i]' removes one element and leaves the array; the
      ;; subscript makes the array sparse rather than renumbering.
      (multiple-value-bind (base sub) (split-subscript a)
        (let ((arr (and sub (not (eq mode :func)) (var-array sh base))))
          (cond
            (arr (if (member sub '("@" "*") :test #'string=)
                     (ignore-errors (unset-var sh base))
                     (array-unset arr (array-subscript sh arr sub))))
            (t (case mode
                 (:func (remhash a (shell-functions sh)))
                 (:var  (ignore-errors (unset-var sh a)))
                 (:auto (if (gethash a (shell-functions sh))
                            (remhash a (shell-functions sh))
                            (ignore-errors (unset-var sh a)))))))))))
  0)

(defun declare-flags (args)
  "Split leading -x/+x option words off ARGS. Returns (values flags rest)."
  (let ((flags '()))
    (loop while (and args (> (length (first args)) 1)
                     (member (char (first args) 0) '(#\- #\+)))
          do (let ((a (pop args)))
               (loop for i from 1 below (length a)
                     do (push (char a i) flags))))
    (values flags args)))

(defun declare-builtin (sh args localp)
  "bash `declare'/`typeset'/`local' with -a, -A, -i, -r, -x.

Only the flags that change storage are acted on: -A must create the variable
as associative BEFORE anything is assigned, because an assoc subscript is a
word while an indexed one is arithmetic. -i is accepted and recorded as a
no-op beyond evaluating the RHS arithmetically."
  (multiple-value-bind (flags rest) (declare-flags args)
    (when (and localp (not (in-function-p sh)))
      (format *error-output* "local: can only be used in a function~%")
      (return-from declare-builtin 1))
    (dolist (a rest 0)
      (let ((eq (position #\= a)))
        (let* ((name (if eq (subseq a 0 eq) a))
               (value (and eq (subseq a (1+ eq))))
               (appendp (and eq (> eq 0) (char= (char a (1- eq)) #\+))))
          (when appendp (setf name (subseq name 0 (1- (length name)))))
          (when localp (declare-local sh (nth-value 0 (split-subscript name))))
          (cond
            ;; -A/-a with no value: create an empty array of the right kind.
            ;; On a name that is ALREADY an array this only declares the type;
            ;; it must not reset the contents, so a redundant `declare -a arr'
            ;; part-way through a script does not silently empty it.
            ((and (null value) (or (member #\A flags) (member #\a flags)))
             (unless (var-array sh name)
               (set-var sh name (make-sh-array (if (member #\A flags)
                                                   :assoc :indexed)))))
            ((member #\n flags))          ; handled below, as a reference
            (value
             (when (and (member #\A flags) (not (var-array sh name)))
               (set-var sh name (make-sh-array :assoc)))
             (assign-one sh name value appendp))
            (t nil))
          ;; -i is an ATTRIBUTE, not a one-off: every LATER assignment to the
          ;; name is evaluated arithmetically too, which is the whole point of
          ;; `declare -i n; n=3+4'.
          (when (member #\i flags)
            (setf (gethash (nth-value 0 (split-subscript name))
                           (shell-int-vars sh))
                  t)
            (when value (assign-one sh name value appendp)))
          ;; -n makes NAME an alias for the variable the value names.
          (when (and (member #\n flags) value)
            (setf (gethash name (shell-namerefs sh)) value)
            ;; The alias itself must not also hold a value, or GET-VAR would
            ;; find the alias's own cell before following the reference.
            (remhash name (shell-vars sh)))
          (when (member #\x flags) (export-var sh name))
          (when (member #\r flags) (mark-readonly sh name)))))))

(define-builtin "declare" (sh args out)
  :synopsis "declare [-aAirx] [NAME[=VALUE] ...]"
  :summary "Set variable values and attributes."
  :description "Declares each NAME, optionally assigning a value.

Options:
  -a  declare NAME an indexed array
  -A  declare NAME an associative array
  -i  give NAME the integer attribute: every later assignment to it is
      evaluated as an arithmetic expression
  -r  make NAME readonly
  -x  export NAME
  -n  make NAME a name reference to the variable its value names

Declaring an existing array with -a or -A only records the type; it does not
empty the variable. `typeset' is a synonym.

Like `export', this is a declaration utility, so a value is expanded as an
assignment rather than as an ordinary word.

Exit Status:
Zero unless an invalid option is given, a NAME is invalid, or an assignment
to a readonly variable is attempted."
  (declare-builtin sh args nil))
(setf (gethash "typeset" *builtins*) (gethash "declare" *builtins*))
(copy-builtin-help "declare" "typeset" :synopsis "typeset [-aAirx] [NAME[=VALUE] ...]")

(define-builtin "mapfile" (sh args out)
  :synopsis "mapfile [-t] [ARRAY]"
  :summary "Read lines from standard input into an array."
  :description "Reads lines from standard input into the indexed array ARRAY, one line per
element. The default array is MAPFILE. `readarray' is a synonym.

Options:
  -t  accepted; the trailing newline is not stored either way

Exit Status:
Zero unless an invalid option is given or ARRAY is readonly."
  ;; bash mapfile/readarray: read lines into an indexed array.
  (multiple-value-bind (flags rest) (declare-flags args)
    (declare (ignore flags))
    (let ((name (or (first rest) "MAPFILE"))
          (lines '()))
      (loop (multiple-value-bind (line missing) (fd-read-line 0)
              (declare (ignore missing))
              (when (eq line :eof) (return))
              ;; bash keeps the trailing newline unless -t is given, and -t is
              ;; the overwhelmingly common use, so both forms are stored the
              ;; same way here: with the newline, matching the default.
              (push (concatenate 'string line (string #\Newline)) lines)))
      (set-var sh name (array-from-list (nreverse lines)))
      0)))
(setf (gethash "readarray" *builtins*) (gethash "mapfile" *builtins*))
(copy-builtin-help "mapfile" "readarray" :synopsis "readarray [-t] [ARRAY]")

(define-builtin "shift" (sh args out)
  :synopsis "shift [N]"
  :summary "Shift positional parameters."
  :description "Renames $(N+1) to $1, $(N+2) to $2, and so on, discarding the first N and
reducing $#. N defaults to 1.

Exit Status:
Zero unless N is negative or greater than $#, in which case the parameters
are left untouched."
  (let ((n (if args (parse-integer (first args)) 1))
        (v (shell-positional sh)))
    (if (<= n (length v))
        (progn (setf (shell-positional sh) (subseq v n)) 0)
        1)))

(define-builtin "exit" (sh args out)
  :synopsis "exit [N]"
  :summary "Exit the shell."
  :description "Exits with status N. With no N, the status of the last command executed is
used. The EXIT trap runs first, whatever the cause of termination.

Inside a trap action, a bare `exit' reports the status that was current when
the trap began, not the status of the trap's own commands.

Exit Status:
N, or the last command's status."
  (signal 'shell-exit
          :code (cond (args (parse-integer (first args) :junk-allowed t))
                      ;; inside a trap, a bare `exit' reports the status that
                      ;; was current when the trap action began
                      (*trap-entry-status*)
                      (t (shell-last-status sh))))
  0)

(define-builtin "return" (sh args out)
  :synopsis "return [N]"
  :summary "Return from a shell function."
  :description "Ends the current function, or the current dot script, with status N. With no
N, the status of the last command executed is used.

Exit Status:
N, or the last command's status. Non-zero if used outside a function or a dot
script."
  (signal 'func-return :code (if args (parse-integer (first args) :junk-allowed t)
                                 (shell-last-status sh)))
  0)

(define-builtin "break" (sh args out)
  :synopsis "break [N]"
  :summary "Exit for, while, until or select loops."
  :description "Leaves the enclosing loop. With N, leaves the Nth enclosing loop counting
outwards; N must be 1 or greater.

Exit Status:
Zero unless N is not 1 or greater."
  (signal 'loop-break :n (if args (parse-integer (first args)) 1)) 0)
(define-builtin "continue" (sh args out)
  :synopsis "continue [N]"
  :summary "Resume the next iteration of a loop."
  :description "Skips the rest of the loop body and begins the next iteration. With N,
resumes the Nth enclosing loop counting outwards; N must be 1 or greater.

Exit Status:
Zero unless N is not 1 or greater."
  (signal 'loop-continue :n (if args (parse-integer (first args)) 1)) 0)

(define-builtin "read" (sh args out)
  :synopsis "read [-rs] [-p PROMPT] [-a ARRAY] [-d DELIM] [-n N] [-N N] [-t SEC] [-u FD] [NAME ...]"
  :summary "Read a line from standard input and split it into fields."
  :description "Reads one line, splits it on $IFS, and assigns the fields to the NAMEs in
order. The last NAME receives everything that is left. With no NAME, the whole
line is assigned to REPLY.

Options:
  -a ARRAY   assign the fields to ARRAY as an indexed array, not to NAMEs
  -d DELIM   read until the first character of DELIM instead of a newline;
             an empty DELIM means NUL, for `find -print0'
  -n N       return after N characters rather than a whole line, but still
             stop at the delimiter
  -N N       return after exactly N characters, ignoring the delimiter
  -p PROMPT  write PROMPT, without a newline, before reading -- only when
             reading from a terminal
  -r         do not treat backslash as an escape character
  -s         do not echo input coming from a terminal
  -t SEC     fail if a complete line is not read within SEC seconds; a SEC
             of 0 tests whether input is available and reads nothing
  -u FD      read from file descriptor FD instead of standard input

Without -r, a backslash escapes the following character and a backslash-newline
pair is a line continuation. -n counts characters AFTER that processing.

Exit Status:
Zero unless end of input is reached, the timeout expires, an invalid option
or argument is given, or FD is invalid. Input ending without a delimiter still
assigns, but returns non-zero."
  ;; POSIX has only -r. The rest are bash: -p prompt, -s silent, -n/-N a
  ;; character count, -d an alternate delimiter, -u a file descriptor, -t a
  ;; timeout.
  (let ((raw-mode nil) (silent nil) (prompt nil) (names args)
        (fd 0) (delim #\Newline) (max nil) (exact nil) (timeout nil)
        (array-name nil))
    ;; Options come in clusters, and the letter that takes an argument may sit
    ;; anywhere in one: bash accepts `-rn1 var' and `-rd "" var'. Reading only
    ;; (char opt 1) and discarding the word silently dropped the argument, so
    ;; `-rn1' behaved as a bare `-r'.
    (block options
      (labels ((count-arg (s)
                 ;; bash: a bad number is status 1, distinct from the status 2
                 ;; it uses for a bad option letter.
                 (let ((v (ignore-errors (parse-integer s))))
                   (unless (and v (>= v 0))
                     (format *error-output* "read: ~A: invalid number~%" s)
                     (return-from builtin 1))
                   v)))
        (loop while (and names (> (length (first names)) 1)
                         (char= (char (first names) 0) #\-))
              do (let ((opt (pop names)) (k 1))
                   ;; `--' ends the options; the next word is a name.
                   (when (string= opt "--") (return-from options))
                   (loop while (< k (length opt))
                         do (let ((kind (char opt k)))
                              (flet ((opt-arg ()
                                       ;; The rest of the cluster is the argument
                                       ;; if there is any, else the next word.
                                       (let ((rest (subseq opt (1+ k))))
                                         (setf k (length opt))
                                         (or (and (plusp (length rest)) rest)
                                             (pop names)
                                             ""))))
                                (case kind
                                  (#\r (setf raw-mode t) (incf k))
                                  (#\s (setf silent t) (incf k))
                                  (#\p (setf prompt (opt-arg)))
                                  (#\n (setf max (count-arg (opt-arg))))
                                  (#\N (setf max (count-arg (opt-arg))
                                             exact t))
                                  (#\d (let ((d (opt-arg)))
                                         ;; `read -d ""' means NUL-delimited.
                                         (setf delim (if (plusp (length d))
                                                         (char d 0)
                                                         (code-char 0)))))
                                  (#\a (setf array-name (opt-arg)))
                                  (#\u (setf fd (count-arg (opt-arg))))
                                  (#\t (let* ((s (opt-arg))
                                              (v (ignore-errors
                                                   (let ((*read-default-float-format*
                                                           'double-float))
                                                     (float (read-from-string s) 1d0)))))
                                         (unless (and (realp v) (>= v 0))
                                           (format *error-output*
                                                   "read: ~A: invalid timeout specification~%"
                                                   s)
                                           (return-from builtin 1))
                                         (setf timeout v)))
                                  (t
                                   (format *error-output*
                                           "read: -~A: invalid option~%" kind)
                                   (return-from builtin 2))))))))))
    ;; bash writes the -p prompt only when it is actually prompting someone;
    ;; on a pipe it would corrupt the stderr of every script that uses it.
    (when (and prompt (plusp (sb-unix:unix-isatty fd)))
      (write-string prompt *error-output*) (finish-output *error-output*))
    ;; -t: bash exits 142 (128+SIGALRM) when the deadline passes with nothing
    ;; read. -t 0 is the polling form: report whether input is available and
    ;; consume nothing.
    (when timeout
      (unless (fd-input-ready-p fd timeout)
        (return-from builtin (if (zerop timeout) 1 142)))
      (when (zerop timeout) (return-from builtin 0)))
    (multiple-value-bind (line escaped eof-no-newline)
        ;; A descriptor, not *STANDARD-INPUT*: redirections move the fd, and a
        ;; buffered stream would keep serving bytes read before the move.
        (read-one-logical-line fd raw-mode :delim delim :max max :exact exact)
      (when (eq line :eof) (return-from builtin 1))
      ;; bash echoes the newline the user's (suppressed) Return would have
      ;; produced -- but only on a terminal. Doing it unconditionally put a
      ;; stray blank line into every piped `read -s'.
      (when (and silent (plusp (sb-unix:unix-isatty fd)))
        (terpri *error-output*))
      ;; POSIX: input ending before a newline still assigns, but the status is
      ;; non-zero so `while read line' terminates on a file with no final
      ;; newline instead of looping on the last record.
      (let ((status (if eof-no-newline 1 0)))
        (cond
          ;; -a: split on IFS and store the fields in an indexed array.
          (array-name
           (set-var sh array-name
                    (array-from-list (split-on-ifs line (current-ifs sh) nil escaped)))
           status)
          ;; -N takes the bytes verbatim: no IFS trimming or splitting at all.
          (exact
           (if names
               (loop for nm in names for i from 0
                     do (set-var sh nm (if (zerop i) line "")))
               (set-var sh "REPLY" line))
           status)
          ((null names) (set-var sh "REPLY" line) status)
          (t
           (let* ((ifs (current-ifs sh))
                  (parts (split-on-ifs line ifs (length names) escaped)))
             (loop for nm in names for i from 0
                   do (set-var sh nm (or (nth i parts) "")))
             status)))))))

(defun fd-input-ready-p (fd seconds)
  "True if FD has input available within SECONDS (a float; 0 polls).

Used by bash's `read -t'. select(2) rather than a non-blocking read, because
the point is to wait without consuming anything."
  (sb-unix:unix-simple-poll fd :input (round (* (max seconds 0) 1000))))

(defun fd-read-line (fd &key (delim #\Newline) max exact)
  "Read from FD a byte at a time, consuming the delimiter but nothing beyond.

DELIM is the terminator (bash `read -d'). MAX stops after that many characters
(`read -n'). EXACT means read exactly MAX characters and treat no byte as a
delimiter (`read -N'), which is why the two are separate options rather than
one with a flag.

Returns (values text missing-delim), or :eof at end of input.

The single-byte reads are the point, not an oversight. POSIX requires `read'
to leave the file offset just past the newline it consumed, because the very
next command may read the same descriptor. A buffered stream cannot do that:
SBCL's *STANDARD-INPUT* slurped the whole file on the first READ-LINE, so in

    while read l; do read X <f1; ...; done <f2

the inner `read X' was served from f2's leftover buffer instead of f1, and
only started honouring its own redirection once that buffer ran dry. That is
what made gpgrt-config, which parses .pc files with exactly this shape, report
another field's value for every variable it read."
  (let ((buf (make-array 1 :element-type '(unsigned-byte 8)))
        (out (make-string-output-stream))
        (got-any nil)
        (count 0))
    (loop
      (let ((n (sb-sys:with-pinned-objects (buf)
                 (loop
                   (multiple-value-bind (r errno)
                       (sb-unix:unix-read fd (sb-sys:vector-sap buf) 1)
                     (cond
                       (r (return r))
                       ;; EINTR is not end of input: a trap firing mid-read
                       ;; would otherwise look like EOF and end the loop.
                       ((eql errno sb-unix:eintr))
                       (t (return 0))))))))
        (cond
          ((zerop n)
           (return (if got-any
                       (values (get-output-stream-string out) t)
                       :eof)))
          (t
           (setf got-any t)
           (let ((ch (code-char (aref buf 0))))
             (cond
               ;; -N: no character is a delimiter; stop only on the count.
               (exact
                (write-char ch out)
                (incf count)
                (when (and max (>= count max))
                  (return (values (get-output-stream-string out) nil))))
               ((char= ch delim)
                (return (values (get-output-stream-string out) nil)))
               (t
                (write-char ch out)
                (incf count)
                ;; -n: a short read is not a failure, so the delimiter is
                ;; reported as present.
                (when (and max (>= count max))
                  (return (values (get-output-stream-string out) nil))))))))))))

(defun read-one-logical-line (fd raw-mode &key (delim #\Newline) max exact)
  "Read one logical line for the `read' builtin from FD.

Returns (values text escaped-positions eof-without-newline), or :eof as the
first value at end of input.

Without -r, a backslash removes the special meaning of the next character
(POSIX 2.14 `read'): `a\bc' reads as `abc', and a backslash-newline pair is a
line continuation that keeps reading. The positions of characters that were
escaped are reported because they must NOT be treated as IFS delimiters
afterwards -- `read x' on `a\ b' yields the single field `a b'.

EOF-WITHOUT-NEWLINE drives the exit status: POSIX requires a non-zero status
when input ends before a newline, even though the fields are still assigned.

MAX (`read -n') counts characters AFTER escape processing, which is why the
limit is applied here rather than in the byte reader: bash reads `a\bc' with
-n 3 as `abc', not as the three raw bytes `a\b'.

A backslash-newline pair is a line continuation and is swallowed whatever -d
says -- `read -d ,' still joins continued lines."
  (let ((text (make-string-output-stream))
        (escaped '())
        (pos 0)
        (eof-no-newline nil)
        (got-any nil))
    (labels ((emit (ch esc)
               (write-char ch text)
               (when esc (push pos escaped))
               (incf pos))
             (limit-reached-p () (and max (>= pos max))))
      (block reading
        ;; `read -n 0' consumes nothing at all and assigns the empty string.
        (when (and max (zerop max)) (return-from reading))
        (loop
          (let ((ch (fd-read-char fd)))
            (cond
              ((null ch)
               (unless got-any
                 (return-from read-one-logical-line (values :eof nil nil)))
               (setf eof-no-newline t)
               (return-from reading))
              (t
               (setf got-any t)
               (cond
                 ((and (not raw-mode) (char= ch #\\))
                  (let ((next (fd-read-char fd)))
                    (cond
                      ((null next)      ; trailing backslash at end of input
                       (emit #\\ nil)
                       (setf eof-no-newline t)
                       (return-from reading))
                      ((char= next #\Newline)) ; continuation: emit nothing
                      (t (emit next t)
                         (when (limit-reached-p) (return-from reading))))))
                 ;; -N takes the bytes verbatim: no byte is a delimiter.
                 ((and (not exact) (char= ch delim)) (return-from reading))
                 (t (emit ch nil)
                    (when (limit-reached-p) (return-from reading))))))))))
    (values (get-output-stream-string text) (nreverse escaped) eof-no-newline)))

(defun fd-read-char (fd)
  "One byte from FD as a character, or NIL at end of input.

Single-byte reads for the reason FD-READ-LINE documents: `read' must leave the
offset just past what it consumed."
  (let ((buf (make-array 1 :element-type '(unsigned-byte 8))))
    (sb-sys:with-pinned-objects (buf)
      (loop
        (multiple-value-bind (r errno)
            (sb-unix:unix-read fd (sb-sys:vector-sap buf) 1)
          (cond
            ((and r (plusp r)) (return (code-char (aref buf 0))))
            ((and r (zerop r)) (return nil))
            ;; EINTR is not end of input: a trap firing mid-read would
            ;; otherwise look like EOF.
            ((eql errno sb-unix:eintr))
            (t (return nil))))))))

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

The line is always split in FULL first, and the leftover-goes-to-the-last-field
rule applies only when that yields MORE fields than MAXFIELDS. Truncating the
scan at MAXFIELDS-1 instead is subtly wrong at the end of the line, because a
trailing IFS-non-whitespace delimiter generates no field (POSIX 2.6.5): with
IFS=x, `read a b' on `xx' has exactly two fields and so gives b=\"\", while the
truncating version handed b the raw remainder `x'.

ESCAPED lists character positions that were backslash-escaped on input; those
are never delimiters, so `read x y' on `a\\ b c' gives x=\"a b\" and y=\"c\".
An escaped character is also immune to the trailing-whitespace trim -- `read a
b' on `x \\ ' must leave b holding that one quoted space."
  (let* ((ws '()) (delims '()))
    (loop for c across ifs
          do (if (member c '(#\Space #\Tab #\Newline))
                 (pushnew c ws)
                 (pushnew c delims)))
    (let ((all '()) (i 0) (n (length line)))
      (flet ((ifs-char-p (c &optional (at -1))
               (and (not (member at escaped))
                    (or (member c ws) (member c delims))))
             (skip-ws () (loop while (and (< i n) (member (char line i) ws)
                                          (not (member i escaped)))
                               do (incf i))))
        (skip-ws)                       ; leading IFS whitespace is discarded
        (loop
          (when (>= i n) (return))
          (let ((start i))
            (loop while (and (< i n) (not (ifs-char-p (char line i) i)))
                  do (incf i))
            ;; Each field is kept with the offset it began at, so the leftover
            ;; can be taken verbatim from the right place.
            (push (cons (subseq line start i) start) all))
          ;; A delimiter is: an optional IFS-whitespace run, then at most one
          ;; non-whitespace IFS character, then another optional run.
          (skip-ws)
          (when (and (< i n) (member (char line i) delims)
                     (not (member i escaped)))
            (incf i)
            (skip-ws))))
      (setf all (nreverse all))
      (cond
        ;; NIL means unlimited -- `read -a' wants every field, not N of them.
        ((or (null maxfields) (<= (length all) maxfields)) (mapcar #'car all))
        (t
         (append (mapcar #'car (subseq all 0 (1- maxfields)))
                 (list (trim-trailing-ifs-ws line (cdr (nth (1- maxfields) all))
                                             ws escaped))))))))

(defun trim-trailing-ifs-ws (line start ws escaped)
  "LINE from START to the end, less any trailing IFS whitespace that was not
backslash-escaped."
  (let ((end (length line)))
    (loop while (and (> end start)
                     (member (char line (1- end)) ws)
                     (not (member (1- end) escaped)))
          do (decf end))
    (subseq line start end)))

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
    ;; bash extension: a pipeline's status is the last non-zero stage's.
    (nil "pipefail"  :pipefail)
    ;; Emacs-style line editing. `set +o emacs' is the documented way to fall
    ;; back to the plain line-at-a-time reader.
    (nil "emacs"     :emacs)
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
  :synopsis "set [-abCefhmnuvx] [-o OPTION] [--] [ARG ...]"
  :summary "Set or unset shell options and positional parameters."
  :description "Changes shell option settings, sets the positional parameters, or displays
the names and values of shell variables.

With no arguments, lists every shell variable in a form that can be reused as
input. Options are turned ON with `-' and OFF with `+'.

  -a  allexport   export every variable that is assigned to afterwards
  -b  notify      report terminated background jobs immediately
  -C  noclobber   `>' will not truncate an existing file
  -e  errexit     exit as soon as a command fails
  -f  noglob      disable pathname expansion
  -h  hashall     remember command locations (accepted; we do not cache)
  -m  monitor     enable job control
  -n  noexec      read commands without executing them
  -u  nounset     treat an unset variable as an error
  -v  verbose     echo input lines as they are read
  -x  xtrace      print commands and their arguments as they are executed

  -o OPTION       set OPTION by its long name; with no OPTION, list them all.
                  Also accepts the names with no letter form: pipefail, emacs,
                  ignoreeof, nolog and vi.
  +o OPTION       unset OPTION

  --   end the options; remaining ARGs become the positional parameters, and
       `set --' with no ARGs clears them
  -    end the options WITHOUT clearing the positional parameters

Exit Status:
Zero unless an invalid option is given."
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
  :synopsis "eval [ARG ...]"
  :summary "Execute arguments as a shell command."
  :description "Joins the ARGs into a single string, reads it as shell input, and executes
the result in the CURRENT shell environment.

Because it runs inline, `exit', `return', `break' and `continue' inside it act
on the enclosing shell, function or loop. A variable assignment error ends the
eval rather than the whole shell.

Exit Status:
The status of the executed command, or zero if the arguments are empty."
  (let ((src (format nil "~{~A~^ ~}" args))
        ;; A failed assignment inside eval ends the eval, not the shell.
        (*assignment-error-fatal* nil))
    (if (string= (string-trim " " src) "") 0
        (run-string-capturing sh src out))))

(define-builtin "." (sh args out)
  :synopsis ". [-p PATH] FILENAME [ARGUMENT ...]"
  :summary "Execute commands from a file in the current shell."
  :description "Reads and executes FILENAME in the CURRENT shell environment, so its
variables, functions and `cd' persist afterwards.

If FILENAME contains no slash, the directories in $PATH are searched for it.
Any ARGUMENTs become the positional parameters while it runs.

`return' ends the sourced file and becomes its status; the caller carries on.
`source' is a synonym.

Exit Status:
The status of the last command executed, or 1 if FILENAME cannot be read."
  ;; POSIX: when the operand contains no slash the shell searches $PATH for it,
  ;; exactly as it would for a command. FIND-IN-PATH already does both halves;
  ;; the previous :allow-slash call short-circuited every name to itself, so a
  ;; bare `. helpers.sh' only ever worked from the right directory.
  (if (null args) (progn (format *error-output* ".: filename argument required~%") 2)
      (let ((path (find-source-file sh (first args))))
        (if (and path (probe-file path))
            ;; `return' ends the sourced script and becomes its status; the
            ;; caller carries on afterwards. Everything else -- exit, break,
            ;; continue -- propagates, because a dot script runs inline in the
            ;; current environment (bash and dash both let a sourced `break'
            ;; break the caller's loop; zsh is the outlier here).
            (handler-case (run-string-capturing sh (slurp-file path) out)
              (func-return (e) (cf-code e)))
            (progn (format *error-output* ".: ~A: not found~%" (first args)) 1)))))

(setf (gethash "source" *builtins*) (gethash "." *builtins*))
(copy-builtin-help "." "source" :synopsis "source [-p PATH] FILENAME [ARGUMENT ...]")

(defun type-kind (sh name)
  "Classify NAME: returns (values kind path) where kind is one of :alias,
:function, :builtin or :file."
  (cond
    ((gethash name (shell-aliases sh)) (values :alias nil))
    ((gethash name (shell-functions sh)) (values :function nil))
    ((builtin-p name) (values :builtin nil))
    (t (let ((p (find-in-path sh name)))
         (if p (values :file p) (values nil nil))))))

(define-builtin "type" (sh args out)
  :synopsis "type [-afpPt] NAME [NAME ...]"
  :summary "Display information about command type."
  :description "Reports how each NAME would be interpreted if used as a command name.

Options:
  -t  print a single word: alias, keyword, function, builtin or file
  -p  print the path of the file that would be run, and nothing for a
      builtin, function, alias or keyword
  -P  as -p, but search $PATH even when NAME is a builtin or function
  -a  accepted; we report the first match rather than every one
  -f  accepted; suppress function lookup

Exit Status:
Zero if every NAME is found, non-zero otherwise."
  ;; POSIX defines no options for `type', but -p and -t are ubiquitous in real
  ;; scripts -- an autoconf-style `if type -p gcc' is how they probe for a
  ;; compiler. Without them the option was taken for a name and reported as
  ;; \"-p: not found\".
  (let ((path-only nil) (type-only nil) (force-path nil) (rest args))
    (loop while (and rest (> (length (first rest)) 1)
                     (char= (char (first rest) 0) #\-))
          do (let ((o (pop rest)))
               (when (string= o "--") (return))
               (loop for c across (subseq o 1) do
                 (case c
                   (#\p (setf path-only t))
                   (#\P (setf path-only t force-path t))
                   (#\t (setf type-only t))
                   (#\a)                ; accepted; we report the first match
                   (t (format *error-output* "type: -~C: invalid option~%" c)
                      (return-from builtin 2))))))
    ;; POSIX: non-zero if any operand is not found, and the diagnostic goes to
    ;; stderr -- writing it to stdout with status 0 made
    ;; `type foo >/dev/null && ...' succeed for a command that does not exist.
    (let ((status 0))
      (dolist (a rest status)
        (multiple-value-bind (kind path)
            (if force-path
                (let ((p (find-in-path sh a))) (if p (values :file p) (values nil nil)))
                (type-kind sh a))
          (cond
            ((null kind)
             (unless (or path-only type-only)
               (format *error-output* "~A: not found~%" a))
             (setf status 1))
            (type-only
             (write-line (ecase kind
                           (:alias "alias") (:function "function")
                           (:builtin "builtin") (:file "file"))
                         out))
            (path-only
             ;; -p prints a path only for an executable on disk
             (if (eq kind :file) (write-line path out) (setf status 0)))
            (t
             (ecase kind
               (:alias (format out "~A is an alias for ~A~%"
                               a (gethash a (shell-aliases sh))))
               (:function (format out "~A is a function~%" a))
               (:builtin (format out "~A is a shell builtin~%" a))
               (:file (format out "~A is ~A~%" a path))))))))))

;;; ---------------------------------------------------------------------------
;;; Tier-2 POSIX builtins
;;; ---------------------------------------------------------------------------

;;; alias / unalias -------------------------------------------------------
(define-builtin "alias" (sh args out)
  :synopsis "alias [-a] [NAME[=VALUE] ...]"
  :summary "Define or display aliases."
  :description "With no arguments, lists every alias in a form that can be reused as input.
With NAME alone, lists that alias. With NAME=VALUE, defines it.

Options:
  -a  remove all alias definitions

An alias is replaced only in command position. If its value ends in a blank,
the word after it is checked for an alias too, which is what makes
`alias sudo=\"sudo \"' expand an aliased command after sudo.

Exit Status:
Zero unless a NAME has no alias defined."
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
  :synopsis "unalias [-a] [NAME ...]"
  :summary "Remove alias definitions."
  :description "Removes each NAME from the alias list.

Options:
  -a  remove all alias definitions

Exit Status:
Zero unless a NAME is not a defined alias."
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
  :synopsis "readonly [-p] [NAME[=VALUE] ...]"
  :summary "Mark names as unchangeable."
  :description "Marks each NAME as readonly. A later assignment to it, or an `unset', fails;
in a non-interactive shell such an assignment is fatal, as POSIX requires.

Options:
  -p  list the readonly names and their values, in a form that can be reused
      as input

Like `export', this is a declaration utility, so the value in NAME=VALUE is
expanded as an assignment rather than as an ordinary word.

Exit Status:
Zero unless an invalid name is given."
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

(define-builtin "local" (sh args out)
  :synopsis "local [-aAirx] [NAME[=VALUE] ...]"
  :summary "Define local variables."
  :description "Declares each NAME local to the current function: the value it had outside
is restored when the function returns. Takes the same options as `declare'.

`local' is not POSIX, but it is present in every shell that has functions.

Exit Status:
Zero unless used outside a function, an invalid option is given, or a NAME is
readonly."
  ;; `local' is not in POSIX, but bash, dash, ksh and zsh all have it and real
  ;; scripts assume it -- git's t/test-lib.sh alone uses it 41 times, and 155
  ;; of its test scripts do. Without it those scripts do not merely misbehave,
  ;; they abort on `local: command not found'.
  ;;
  ;; Semantics follow bash and dash, which agree wherever it matters: outside
  ;; a function it is an error; `local x=v' shadows for the duration of the
  ;; call; a callee still sees the local (dynamic scoping); and the previous
  ;; value comes back however the function ends.
  (cond
    ((not (in-function-p sh))
     (format *error-output* "local: can only be used in a function~%")
     1)
    ;; `local -a/-A/-i/-r/-x' is `declare' with function scoping.
    ((and args (> (length (first args)) 1)
          (member (char (first args) 0) '(#\- #\+)))
     (declare-builtin sh args t))
    (t
     (let ((status 0))
       (dolist (a args status)
         (let ((eq (position #\= a)))
           (let ((name (if eq (subseq a 0 eq) a)))
             ;; A readonly name cannot be shadowed: bash and dash both refuse.
             ;; Report and keep going, so `local a b c' still binds the rest.
             (cond
               ((readonly-p sh name)
                (format *error-output* "local: ~A: is read only~%" name)
                (setf status 1))
               (t
                (declare-local sh name)
                (when eq (set-var sh name (subseq a (1+ eq)))))))))))))

;;; builtin -- run a shell builtin, bypassing functions ---------------------
;;; EXEC-BUILTIN-PREFIX in the executor does the work whenever there is an
;;; operand, because `builtin break' has to act on the CALLER's loop. This
;;; entry exists so the no-operand form works and so `type builtin' and
;;; completion can see the name at all -- exactly the split `command' uses.
(define-builtin "builtin" (sh args out)
  :synopsis "builtin [SHELL-BUILTIN [ARG ...]]"
  :summary "Execute shell builtins."
  :description "Executes SHELL-BUILTIN with the given arguments, bypassing any
shell function of that name. This is how a function can override a builtin and
still reach it -- a function named `cd' can call `builtin cd' without recursing.

Unlike running the builtin through a function or an external command, the
control-flow builtins still act on the caller, so `builtin break' breaks the
enclosing loop and `builtin exit' exits the shell.

`builtin' and `command' may be combined in any order; the innermost name is
what runs.

Exit Status:
The status of SHELL-BUILTIN, or 1 if it is not a shell builtin. Zero when
given no arguments."
  (cond
    ((null args) 0)
    ((builtin-p (first args))
     (funcall (find-builtin (first args)) sh (rest args) *standard-output*))
    (t (format *error-output* "builtin: ~A: not a shell builtin~%" (first args))
       1)))

;;; command -- run a command bypassing functions; -v/-V to describe ---------
;;; The actual "run external/builtin bypassing function lookup" behavior is
;;; handled in the executor; here we implement -v and -V, and for the plain
;;; form we signal the executor via a throw.
(define-builtin "command" (sh args out)
  :synopsis "command [-pVv] COMMAND [ARG ...]"
  :summary "Execute a simple command or display information about commands."
  :description "Runs COMMAND with ARGs, suppressing the lookup of any shell FUNCTION of that
name -- which is how a function can call the command it overrides.

Options:
  -p  use a default value for $PATH that is guaranteed to find the standard
      utilities
  -v  print a description of COMMAND, similar to `type'
  -V  print a more verbose description

`command' and `builtin' may be combined in any order.

Exit Status:
The status of COMMAND, 127 if it is not found, or the status of `command'
itself if an option error occurs."
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
  :synopsis "getopts OPTSTRING NAME [ARG ...]"
  :summary "Parse option arguments."
  :description "Takes the next option from ARGs (or from the positional parameters when no
ARG is given) and stores it in the shell variable NAME.

OPTSTRING lists the recognised option characters; a character followed by `:'
takes an argument, which is placed in OPTARG. $OPTIND holds the index of the
next argument to process and is initialised to 1 by the shell -- reset it by
hand before parsing a second argument list.

A leading `:' in OPTSTRING selects silent error reporting: an invalid option
sets NAME to `?' and OPTARG to the offending character, and a missing argument
sets NAME to `:'. Otherwise a diagnostic is written to standard error.

Exit Status:
Zero while an option is found; non-zero at the end of the options or on error."
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
        ;; OPTITER (sxsh-internal) for bundled options like -abc.
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

(defparameter +pseudo-signals+ '("EXIT" "ERR" "DEBUG" "RETURN")
  "Trappable conditions that are not real signals. EXIT is POSIX; ERR, DEBUG
and RETURN are bash extensions.")

(defun valid-condition-p (name)
  "True if NAME designates a trappable condition."
  (let ((norm (normalize-signal name)))
    (or (member norm +pseudo-signals+ :test #'string=)
        (and (signal-number norm) t))))

(defparameter +shopt-names+
  '("extglob" "nullglob" "dotglob" "nocaseglob" "globstar" "failglob"
    "expand_aliases" "checkwinsize" "cmdhist" "histappend" "huponexit"
    "interactive_comments" "lithist" "login_shell" "nocasematch" "xpg_echo"
    "globskipdots")
  "shopt names we recognise. Only some change behaviour -- see SHOPT-P uses --
but an unknown name has to be an error, since scripts test the exit status of
`shopt -q name' to feature-detect.")

(defun shopt-p (sh name)
  (nth-value 1 (gethash name (shell-shopts sh))))

(define-builtin "history" (sh args out)
  :synopsis "history [-c] [-r] [-w] [N]"
  :summary "Display or manipulate the history list."
  :description "With no options, prints the history list with line numbers. With N, prints
only the last N entries.

Options:
  -c  clear the history list
  -r  read the history file and append its contents to the list
  -w  write the current list to the history file

The file is $HISTFILE, defaulting to ~/.sxsh_history, and its length is
capped by $HISTSIZE.

Exit Status:
Zero unless an invalid option is given or the history file cannot be read or
written."
  ;; bash: history [N] | -c | -w | -r. Not POSIX -- `fc -l' is the standard
  ;; spelling -- but universally expected at a prompt.
  (cond
    ((and args (string= (first args) "-c"))
     ;; Advance the base past everything dropped: history numbers keep
     ;; climbing for the life of the session, so clearing must not make the
     ;; next entry reuse a number already shown.
     (incf (shell-history-base sh) (history-count sh))
     (setf (fill-pointer (shell-history sh)) 0)
     0)
    ((and args (string= (first args) "-w")) (history-save sh) 0)
    ((and args (string= (first args) "-r")) (history-load sh) 0)
    (t
     (let* ((n (history-count sh))
            (want (or (and args (ignore-errors (parse-integer (first args)))) n))
            (start (max 0 (- n want))))
       (loop for i from start below n
             do (format out "~5D  ~A~%" (history-number sh i)
                        (aref (shell-history sh) i)))
       0))))

(define-builtin "fc" (sh args out)
  :synopsis "fc [-e EDITOR] [-lnr] [FIRST] [LAST]  or  fc -s [PAT=REP] [CMD]"
  :summary "Display or re-execute commands from the history list."
  :description "FIRST and LAST select a range of history entries, by number or by a string
that the command begins with; a negative number counts back from the current
command. Without them, `fc -l' lists the last 16 entries and the editing form
selects the previous command alone.

Options:
  -l  list the range on standard output instead of editing it
  -n  omit the line numbers when listing
  -r  reverse the order
  -e EDITOR  use EDITOR to edit the range; the default is $FCEDIT, then
             $EDITOR, then `ed'
  -s  re-execute without invoking an editor, after applying the substitution
      PAT=REP to the command text

The command about to be run is written to standard error first.

Exit Status:
Zero unless an invalid option is given or the history is empty; otherwise the
status of the command re-executed."
  ;; POSIX: fc [-r] [-e editor] [first [last]]
  ;;        fc -l [-nr] [first [last]]
  ;;        fc -s [old=new] [first]
  ;; `fc' means "fix command": list, or re-run, earlier commands.
  (let ((listp nil) (nonum nil) (reversep nil) (subst nil) (editor nil)
        (rest args))
    (loop while (and rest (> (length (first rest)) 1)
                     (char= (char (first rest) 0) #\-)
                     ;; A lone `-' or a negative number is an operand.
                     (not (digit-char-p (char (first rest) 1))))
          do (let ((a (pop rest)))
               (loop for i from 1 below (length a)
                     do (case (char a i)
                          (#\l (setf listp t))
                          (#\n (setf nonum t))
                          (#\r (setf reversep t))
                          (#\s (setf subst t))
                          (#\e (setf editor (if (> (length a) (1+ i))
                                                (subseq a (1+ i))
                                                (pop rest)))
                                (return))
                          (t nil)))))
    ;; POSIX: the `fc' command itself is removed from the history list and
    ;; replaced by whatever it re-executes. The REPL records each line before
    ;; running it, so the newest entry IS this invocation -- without dropping
    ;; it, `fc -s' with no operand resolves to itself and re-executes forever.
    ;; That is an infinite loop, not a cosmetic wart.
    (when (and (shell-interactive sh) (plusp (history-count sh)))
      (decf (fill-pointer (shell-history sh))))
    (when (zerop (history-count sh))
      (format *error-output* "fc: history is empty~%")
      (return-from builtin 1))
    (cond
      ;; -s: re-execute, optionally with a substitution. The command is echoed
      ;; first, as POSIX requires, so the user sees what actually ran.
      (subst
       (let* ((pair (and rest (find #\= (first rest)) (pop rest)))
              (idx (history-resolve sh (first rest) (1- (history-count sh)))))
         (unless idx
           (format *error-output* "fc: no such command~%")
           (return-from builtin 1))
         (let ((cmd (aref (shell-history sh) idx)))
           (when pair
             (let ((eq (position #\= pair)))
               (setf cmd (substitute-first cmd (subseq pair 0 eq)
                                           (subseq pair (1+ eq))))))
           ;; POSIX and bash both write the command being re-run to STDERR,
           ;; not stdout -- confirmed by running bash under a pty with
           ;; 2>/dev/null, which suppressed the echo. Writing it to stdout
           ;; would corrupt `x=$(fc -s ...)'.
           (format *error-output* "~A~%" cmd)
           (finish-output *error-output*)
           (history-add sh cmd)
           (return-from builtin (run-string-capturing sh cmd out)))))
      ;; -l: list a range.
      (listp
       (let* ((n (history-count sh))
              (first-i (history-resolve sh (first rest) (max 0 (- n 16))))
              (last-i (history-resolve sh (second rest) (1- n))))
         (unless (and first-i last-i)
           (format *error-output* "fc: no such command~%")
           (return-from builtin 1))
         (when (> first-i last-i) (rotatef first-i last-i) (setf reversep t))
         (let ((idxs (loop for i from first-i to last-i collect i)))
           ;; bash uses NUMBER<TAB><SPACE> here, and keeps the <TAB><SPACE>
           ;; even under -n. That is NOT the `history' builtin's format
           ;; (%5d then two spaces), so the two must not share a formatter.
           (dolist (i (if reversep (reverse idxs) idxs))
             (if nonum
                 (format out "~C ~A~%" #\Tab (aref (shell-history sh) i))
                 (format out "~D~C ~A~%" (history-number sh i) #\Tab
                         (aref (shell-history sh) i)))))
         0))
      ;; No -l and no -s: edit the range, then run whatever comes back.
      (t
       (let* ((n (history-count sh))
              (first-i (history-resolve sh (first rest) (1- n)))
              (last-i (history-resolve sh (second rest) first-i)))
         (unless (and first-i last-i)
           (format *error-output* "fc: no such command~%")
           (return-from builtin 1))
         (when (> first-i last-i) (rotatef first-i last-i))
         (let ((path (format nil "/tmp/sxsh-fc-~A-~A"
                             (sb-posix:getpid) (random 1000000)))
               (ed (or editor
                       (nth-value 0 (get-var sh "FCEDIT"))
                       (nth-value 0 (get-var sh "EDITOR"))
                       "vi")))
           (unwind-protect
                (progn
                  (with-open-file (o path :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
                    (loop for i from first-i to last-i
                          do (write-line (aref (shell-history sh) i) o)))
                  (let ((st (run-string-capturing
                             sh (format nil "~A ~A" ed (shell-quote path)) out)))
                    (unless (zerop st)
                      (format *error-output* "fc: editor exited ~D~%" st)
                      (return-from builtin 1)))
                  (let ((text (slurp-file path)))
                    (format out "~A" text)
                    (finish-output out)
                    (history-add sh (string-right-trim '(#\Newline) text))
                    (run-string-capturing sh text out)))
             (ignore-errors (delete-file path)))))))))

(defun substitute-first (string old new)
  "Replace the first occurrence of OLD in STRING with NEW."
  (let ((i (search old string)))
    (if i
        (concatenate 'string (subseq string 0 i) new
                     (subseq string (+ i (length old))))
        string)))

(define-builtin "shopt" (sh args out)
  :synopsis "shopt [-pqsu] [-o] [OPTNAME ...]"
  :summary "Set and unset shell options."
  :description "Sets or unsets the named shell options, or lists their settings.

Options:
  -s  enable each OPTNAME
  -u  disable each OPTNAME
  -q  suppress output; the exit status alone reports the setting
  -p  list in a reusable form (the default with no -s or -u)
  -o  address the `set -o' option names instead of the shopt ones

Recognised names include extglob, nullglob, dotglob, globskipdots (on by
default), nocaseglob, nocasematch, globstar, failglob, expand_aliases,
huponexit, interactive_comments and xpg_echo. Only some change behaviour, but
every name is accepted so that scripts can feature-test with `shopt -q'.

Exit Status:
Zero if every OPTNAME is enabled; non-zero otherwise. An unknown name is an
error."
  (let ((mode nil) (quiet nil) (names args))
    (loop while (and names (> (length (first names)) 1)
                     (char= (char (first names) 0) #\-))
          do (let ((a (pop names)))
               (loop for i from 1 below (length a)
                     do (case (char a i)
                          (#\s (setf mode :set))
                          (#\u (setf mode :unset))
                          (#\q (setf quiet t))
                          (#\o nil)      ; `shopt -o' addresses set -o names
                          (t nil)))))
    (cond
      ;; No names: list them all.
      ((null names)
       (unless quiet
         (dolist (n (sort (copy-list +shopt-names+) #'string<))
           ;; bash pads the name to 20 columns before the tab.
           (format out "~20A~C~A~%" n #\Tab (if (shopt-p sh n) "on" "off"))))
       0)
      (t
       (let ((status 0))
         (dolist (n names status)
           (cond
             ((not (member n +shopt-names+ :test #'string=))
              (unless quiet
                (format *error-output* "shopt: ~A: invalid shell option name~%" n))
              (setf status 1))
             ((eq mode :set) (setf (gethash n (shell-shopts sh)) t)
              (when (string= n "extglob") (setf *extglob* t)))
             ((eq mode :unset) (remhash n (shell-shopts sh))
              (when (string= n "extglob") (setf *extglob* nil)))
             (t
              ;; Query form: the status reports the state.
              (unless quiet
                (format out "~20A~C~A~%" n #\Tab (if (shopt-p sh n) "on" "off")))
              (unless (shopt-p sh n) (setf status 1))))))))))

(define-builtin "trap" (sh args out)
  :synopsis "trap [-lp] [[ACTION] SIGNAL_SPEC ...]"
  :summary "Trap signals and other events."
  :description "Arranges for ACTION to be read and executed when the shell receives any of
the SIGNAL_SPECs.

If ACTION is `-' each signal is reset to its original disposition; if it is
the empty string each signal is ignored. If ACTION is absent and every operand
is a signal spec, the signals are reset -- so `trap 0 EXIT' clears the EXIT
trap rather than running the command `0'.

Options:
  -l  list the signal names and their numbers
  -p  print the action associated with each signal spec

SIGNAL_SPEC is a signal name with or without the SIG prefix, or a number.
Besides real signals:
  EXIT (0)  runs when the shell exits, whatever the cause -- including a
            syntax error in a later command
  ERR       runs when a command fails
  DEBUG     runs before each command

Operands are applied in order and processing stops at the first invalid one,
so the specs before it have already taken effect.

The status a handler leaves behind is discarded: `$?' after it returns is
whatever it was when the signal arrived.

Exit Status:
Zero unless a SIGNAL_SPEC is invalid or an option is unrecognised."
  ;; `--' is accepted and ignored, as for any POSIX utility.
  (when (and args (string= (first args) "--")) (pop args))
  (cond
    ((null args) (print-traps sh out))
    ((string= (first args) "-p") (print-traps sh out (rest args)))
    ;; A leading `-SOMETHING' that is not `-' or a known option is an invalid
    ;; option, not an action: `trap -1 EXIT' must fail rather than register a
    ;; handler that runs the command `-1'.
    ((and (> (length (first args)) 1) (char= (char (first args) 0) #\-)
          (not (string= (first args) "--")))
     (format *error-output* "trap: ~A: invalid option~%" (first args))
     (format *error-output* "trap: usage: trap [-lp] [[action] signal_spec ...]~%")
     2)
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
       ;; Applied in order, stopping at the first bad spec -- the ones before
       ;; it have already taken effect. Validating them all up front and
       ;; bailing meant `trap - 0 -99' left the EXIT trap installed, where
       ;; bash has already removed it by the time it rejects `-99'.
       (dolist (c conds)
         (unless (valid-condition-p c)
           (format *error-output* "trap: ~A: bad signal specification~%" c)
           (return-from builtin 1))
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
  :synopsis "wait [PID | JOBSPEC ...]"
  :summary "Wait for job completion."
  :description "Waits for each process or job and reports its termination status. With no
operand, waits for every active child process.

Exit Status:
The status of the last operand waited for; 127 if it does not name a live
child. Zero when waiting for all children."
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
                           (setf status (sxsh-shell::wait-and-decode pid))))
                     (setf (shell-bg-pids sh) (remove pid (shell-bg-pids sh)))))))))
        ;; no args: wait for all jobs and loose bg pids
        (progn
          (dolist (job (copy-list (shell-jobs sh)))
            (when (eq (job-state job) :running)
              (setf status (wait-for-job sh job))))
          (dolist (pid (shell-bg-pids sh))
            (setf status (sxsh-shell::wait-and-decode pid)))
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
  :synopsis "kill [-s SIGSPEC | -SIGSPEC] [-l] PID | JOBSPEC ..."
  :summary "Send a signal to a job."
  :description "Sends the named signal to each process or job. The default is TERM.

Options:
  -s SIGSPEC  the signal to send, by name or number
  -SIGSPEC    the same, written directly (`kill -9', `kill -HUP')
  -l          list the signal names; with an argument, translate between a
              name and a number

A JOBSPEC signals the whole process group, so `kill %1' stops a pipeline
rather than just its last stage.

Exit Status:
Zero unless an invalid option or signal is given, or the signal cannot be
sent."
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
  :synopsis "jobs [-l] [JOBSPEC ...]"
  :summary "Display status of jobs."
  :description "Lists the active jobs. The current job is marked `+' and the previous one
`-'.

Options:
  -l  also show each job's process group id

Exit Status:
Zero unless an invalid JOBSPEC is given."
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
  :synopsis "fg [JOBSPEC]"
  :summary "Move a job to the foreground."
  :description "Brings JOBSPEC into the foreground, giving it the terminal, and waits for it
to finish. With no JOBSPEC the current job is used. Requires job control
(`set -m', on by default when interactive).

Exit Status:
The status of the job, or non-zero if job control is disabled or JOBSPEC does
not name a job."
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
  :synopsis "bg [JOBSPEC ...]"
  :summary "Move jobs to the background."
  :description "Resumes each stopped JOBSPEC in the background, as though it had been started
with `&'. With no JOBSPEC the current job is used.

Exit Status:
Zero unless job control is disabled or a JOBSPEC does not name a job."
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
(defun umask-symbolic (mask)
  "Render MASK the way `umask -S' does: the permissions it LEAVES, not the
bits it clears."
  (flet ((who (shift)
           (let ((bits (logandc1 (ldb (byte 3 shift) mask) 7)))
             (with-output-to-string (s)
               (when (logtest bits 4) (write-char #\r s))
               (when (logtest bits 2) (write-char #\w s))
               (when (logtest bits 1) (write-char #\x s))))))
    (format nil "u=~A,g=~A,o=~A" (who 6) (who 3) (who 0))))

(define-builtin "umask" (sh args out)
  :synopsis "umask [-S] [MODE]"
  :summary "Display or set the file mode creation mask."
  :description "With MODE, sets the mask to it; MODE may be octal, or symbolic in the form
`u=rwx,go=rx'. With no MODE, prints the current mask.

Options:
  -S  print the mask symbolically rather than in octal

Exit Status:
Zero unless MODE is invalid."
  (let ((symbolic nil) (rest args))
    (loop while (and rest (string= (first rest) "-S"))
          do (pop rest) (setf symbolic t))
    (cond
      ((null rest)
       (let ((m (sb-posix:umask 0)))
         (sb-posix:umask m)            ; restore
         (write-line (if symbolic (umask-symbolic m) (format nil "~4,'0O" m)) out))
       0)
      (t
       (let ((spec (first rest)))
         (handler-case
             (let ((val (parse-integer spec :radix 8)))
               (unless (<= 0 val #o777) (error "out of range"))
               (sb-posix:umask val) 0)
           (error ()
             (format *error-output* "umask: ~A: invalid mask~%" spec)
             1)))))))

;;; hash -- command location cache; we keep a minimal real cache -----------
;;; ulimit ----------------------------------------------------------------
;;;
;;; getrlimit/setrlimit are not in sb-posix, so they are bound here. The
;;; resource numbers are NOT portable -- macOS has AS=5, MEMLOCK=6, NPROC=7,
;;; NOFILE=8 where Linux has RSS=5, NPROC=6, NOFILE=7, MEMLOCK=8, AS=9 -- and
;;; RLIM_INFINITY differs too (2^63-1 vs ~0). Every one of them is behind a
;;; reader conditional for that reason; hardcoding one platform's values is
;;; the same mistake that made job control silently wrong on macOS.

(sb-alien:define-alien-routine ("getrlimit" %getrlimit) sb-alien:int
  (resource sb-alien:int) (rlp (* t)))
(sb-alien:define-alien-routine ("setrlimit" %setrlimit) sb-alien:int
  (resource sb-alien:int) (rlp (* t)))

(defconstant +rlimit-cpu+     0)
(defconstant +rlimit-fsize+   1)
(defconstant +rlimit-data+    2)
(defconstant +rlimit-stack+   3)
(defconstant +rlimit-core+    4)
(defconstant +rlimit-as+      #+darwin 5 #-darwin 9)
(defconstant +rlimit-memlock+ #+darwin 6 #-darwin 8)
(defconstant +rlimit-nproc+   #+darwin 7 #-darwin 6)
(defconstant +rlimit-nofile+  #+darwin 8 #-darwin 7)
(defconstant +rlim-infinity+
  #+darwin #x7FFFFFFFFFFFFFFF
  #-darwin #xFFFFFFFFFFFFFFFF)

(defparameter +ulimit-resources+
  ;; (flag  description  resource  unit-in-bytes-or-1)
  `((#\c "core file size"     ,+rlimit-core+    512)
    (#\d "data seg size"      ,+rlimit-data+    1024)
    (#\f "file size"          ,+rlimit-fsize+   512)
    (#\l "max locked memory"  ,+rlimit-memlock+ 1024)
    (#\n "open files"         ,+rlimit-nofile+  1)
    (#\s "stack size"         ,+rlimit-stack+   1024)
    (#\t "cpu time"           ,+rlimit-cpu+     1)
    (#\u "max user processes" ,+rlimit-nproc+   1)
    (#\v "virtual memory"     ,+rlimit-as+      1024))
  "Resources ulimit can report or set, with the unit each is expressed in.
POSIX only requires -f, in 512-byte blocks; the rest follow common usage.")

(defun rlimit-get (resource)
  "Return (values soft hard) for RESOURCE, or NIL on failure."
  (sb-alien:with-alien ((rl (array (sb-alien:unsigned 64) 2)))
    (when (zerop (%getrlimit resource (sb-alien:cast rl (* t))))
      (values (sb-alien:deref rl 0) (sb-alien:deref rl 1)))))

(defun rlimit-set (resource soft hard)
  (sb-alien:with-alien ((rl (array (sb-alien:unsigned 64) 2)))
    (setf (sb-alien:deref rl 0) soft
          (sb-alien:deref rl 1) hard)
    (zerop (%setrlimit resource (sb-alien:cast rl (* t))))))

(defun ulimit-format (value unit)
  (if (>= value +rlim-infinity+) "unlimited"
      (princ-to-string (if (= unit 1) value (floor value unit)))))

(defun ulimit-parse (text unit)
  (cond
    ((string= text "unlimited") +rlim-infinity+)
    (t (let ((n (ignore-errors (parse-integer text))))
         (and n (* n unit))))))

(define-builtin "ulimit" (sh args out)
  :synopsis "ulimit [-HSa] [-cdflnstuv] [LIMIT]"
  :summary "Modify or display shell resource limits."
  :description "Reports or sets a limit on a resource available to the shell and to the
processes it starts. LIMIT may be a number in the resource's own units, or
`unlimited'; with no LIMIT the current value is printed.

Which limit:
  -c  core file size (512-byte blocks)     -n  number of open files
  -d  data segment size (KB)               -s  stack size (KB)
  -f  size of files written (512-byte blocks, the default and the only
      resource POSIX requires)             -t  CPU time (seconds)
  -l  maximum locked memory (KB)           -u  number of user processes
  -v  size of virtual memory (KB)
  -a  report every limit

Which value:
  -H  the hard limit -- once lowered it cannot be raised again
  -S  the soft limit (reported by default)

Exit Status:
Zero unless an invalid option is given or the limit cannot be set."
  (let ((hard nil) (soft nil) (flag #\f) (operand nil) (all nil) (rest args))
    (loop while (and rest (> (length (first rest)) 1)
                     (char= (char (first rest) 0) #\-))
          do (let ((letters (subseq (pop rest) 1)))
               (loop for c across letters do
                 (case c
                   (#\H (setf hard t))
                   (#\S (setf soft t))
                   (#\a (setf all t))
                   (t (if (assoc c +ulimit-resources+)
                          (setf flag c)
                          (progn
                            (format *error-output*
                                    "ulimit: ~C: invalid option~%" c)
                            (return-from builtin 2))))))))
    (setf operand (first rest))
    ;; which of the pair to report: -H hard, otherwise soft
    (flet ((report (entry)
             (destructuring-bind (c desc resource unit) entry
               (declare (ignore c))
               (multiple-value-bind (cur max) (rlimit-get resource)
                 (if cur
                     (format out "~A~28T~A~%" desc
                             (ulimit-format (if hard max cur) unit))
                     (format out "~A~28T~A~%" desc "unknown"))))))
      (when all
        (dolist (e +ulimit-resources+) (report e))
        (return-from builtin 0)))
    (let ((entry (assoc flag +ulimit-resources+)))
      (destructuring-bind (c desc resource unit) entry
        (declare (ignore c desc))
        (cond
          ((null operand)
           (multiple-value-bind (cur max) (rlimit-get resource)
             (unless cur
               (format *error-output* "ulimit: cannot read limit~%")
               (return-from builtin 1))
             (write-line (ulimit-format (if hard max cur) unit) out)
             0))
          (t
           (let ((v (ulimit-parse operand unit)))
             (unless v
               (format *error-output* "ulimit: ~A: invalid number~%" operand)
               (return-from builtin 1))
             (multiple-value-bind (cur max) (rlimit-get resource)
               (unless cur
                 (format *error-output* "ulimit: cannot read limit~%")
                 (return-from builtin 1))
               ;; with neither -H nor -S, POSIX sets both
               (let ((new-soft (if hard cur v))
                     (new-hard (if (or hard (not soft)) v max)))
                 (when (and soft (not hard)) (setf new-hard max))
                 (if (rlimit-set resource new-soft new-hard)
                     0
                     (progn (format *error-output*
                                    "ulimit: cannot modify limit~%")
                            1)))))))))))

(define-builtin "hash" (sh args out)
  :synopsis "hash [-r] [NAME ...]"
  :summary "Remember or display program locations."
  :description "Accepted for compatibility. sxsh looks commands up in $PATH each time rather
than caching locations, so there is nothing to remember or forget.

Options:
  -r  forget all remembered locations -- a no-op here

Exit Status:
Zero unless NAME is not found in $PATH."
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
  :synopsis "times"
  :summary "Display process times."
  :description "Prints the accumulated user and system CPU time for the shell and for all of
its terminated children: the shell's own pair on the first line, the children's
on the second, each as MINUTESmSECONDSs.

Exit Status:
Always zero."
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
  :synopsis "exec [COMMAND [ARGUMENT ...]] [REDIRECTION ...]"
  :summary "Replace the shell with the given command."
  :description "With COMMAND, runs it in place of the shell: nothing after it is executed,
and the shell's exit status is the command's.

With NO command, any redirections take effect in the current shell and are
permanent -- this is how `exec 3>file' and `exec 3>&-' open and close
descriptors for the rest of the script.

Exit Status:
Zero if the redirections succeed. If COMMAND cannot be found the shell exits
with 127; otherwise the shell is replaced and does not return."
  (throw 'exec-builtin args))

;;; test / [ --------------------------------------------------------------

(define-builtin "test" (sh args out)
  :synopsis "test EXPRESSION"
  :summary "Evaluate conditional expression."
  :description "Exits with status 0 (true) or 1 (false) depending on EXPRESSION.

File tests:
  -e FILE  exists            -f FILE  is a regular file
  -d FILE  is a directory    -h, -L FILE  is a symbolic link
  -r/-w/-x FILE  is readable / writable / executable
  -s FILE  exists and is not empty
  -p FILE  is a named pipe   -S FILE  is a socket
  -b/-c FILE  is a block / character special file
  -t FD    FD is open on a terminal
  -g/-u/-k FILE  has the setgid / setuid / sticky bit
  -N FILE  has been modified since it was last read
  -O/-G FILE  is owned by the effective user / group
  F1 -nt/-ot F2  F1 is newer / older than F2
  F1 -ef F2      F1 and F2 are the same file

String tests:
  -z STRING  is empty        -n STRING, STRING  is not empty
  S1 = S2, S1 != S2          S1 < S2, S1 > S2  (code-point order)

Arithmetic tests:
  N1 -eq/-ne/-lt/-le/-gt/-ge N2

Other:
  -v NAME  the shell variable NAME is set
  -o OPT   the shell option OPT is set
  ! EXPR, EXPR1 -a EXPR2, EXPR1 -o EXPR2, ( EXPR )

Unlike `[[ ]]', operands here are ordinary words, so they are subject to word
splitting and pathname expansion and must be quoted.

Exit Status:
Zero if EXPRESSION is true, 1 if false, 2 if it is malformed."
  (handler-case (if (eval-test args) 0 1)
    (test-usage-error (e)
      (format *error-output* "test: ~A~%" e)
      2)))
(define-builtin "[" (sh args out)
  :synopsis "[ EXPRESSION ]"
  :summary "Evaluate conditional expression."
  :description "Identical to `test' except that a final `]' argument is
required. See `help test' for the full list of expressions.

Exit Status:
Zero if EXPRESSION is true, 1 if false, 2 if it is malformed or the closing
`]' is missing."
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

;;; ---------------------------------------------------------------------------
;;; help
;;; ---------------------------------------------------------------------------

(defparameter +syntax-help+
  ;; (topic synopsis summary description)
  ;;
  ;; Shell GRAMMAR, not builtins, so these deliberately do NOT go into
  ;; *BUILTINS*: that table drives dispatch, and COMMAND-NAMES maphashes it for
  ;; completion. `help' searches both.
  '(("if" "if LIST; then LIST; [ elif LIST; then LIST; ]... [ else LIST; ] fi"
     "Execute commands conditionally."
     "The `if LIST' is executed. If its exit status is zero, the `then LIST' is
executed. Otherwise each `elif LIST' is executed in turn, and if any exits
zero the corresponding `then LIST' runs. Failing that, the `else LIST' runs if
one is present.

Exit Status:
The status of the last command executed, or zero if none was.")

    ("for" "for NAME [in WORDS ...]; do LIST; done"
     "Execute commands for each member of a list."
     "NAME is set to each member of WORDS in turn and LIST is executed. With no
`in WORDS', the positional parameters are used instead.

WORDS are expanded first: brace expansion, then the usual word expansions, so
`for i in {1..3}' and `for f in *.c' both work.

See also `for ((' for the arithmetic form.

Exit Status:
The status of the last command executed, or zero if WORDS was empty.")

    ("for ((" "for (( INIT; TEST; STEP )); do LIST; done"
     "Arithmetic for loop."
     "INIT is evaluated once, then LIST is executed while TEST evaluates
non-zero, with STEP evaluated after each iteration. All three are arithmetic
expressions; an omitted TEST is treated as 1, giving an infinite loop.

Exit Status:
The status of the last command executed, or zero if the body never ran.")

    ("while" "while LIST; do LIST; done"
     "Execute commands as long as a test succeeds."
     "The `while LIST' is executed; while its exit status is zero, the body LIST
is executed.

Exit Status:
The status of the last body command executed, or zero if the body never ran.")

    ("until" "until LIST; do LIST; done"
     "Execute commands as long as a test does not succeed."
     "As `while', but the body runs while the test's exit status is NON-zero.

Exit Status:
The status of the last body command executed, or zero if the body never ran.")

    ("case" "case WORD in [PATTERN [| PATTERN]...) LIST ;;]... esac"
     "Execute commands based on pattern matching."
     "WORD is expanded and matched against each PATTERN in turn; the LIST of the
first match is executed. Patterns are shell patterns, not regular expressions.
Alternatives are separated by `|'.

A clause ends with `;;' to stop, `;&' to fall through into the next clause's
LIST unconditionally, or `;;&' to continue testing the remaining patterns.

Exit Status:
The status of the last command executed, or zero if no pattern matched.")

    ("select" "select NAME [in WORDS ...]; do LIST; done"
     "Select a word from a list and execute commands."
     "WORDS are printed as a numbered menu on standard error, $PS3 is written as
a prompt, and a line is read. A number selects the corresponding word, which is
assigned to NAME; any other input assigns the empty string. The input line is
kept in REPLY. The menu is reprinted whenever the reply is empty, and the loop
continues until end of input or a `break'.

With no `in WORDS', the positional parameters are used.

Exit Status:
The status of the last command executed.")

    ("function" "function NAME { LIST; }  or  NAME () { LIST; }"
     "Define a shell function."
     "Creates a function called NAME whose body is LIST. Calling NAME runs the
body with the arguments as the positional parameters; $0 is unchanged.

The `function' keyword is a bash extension; the POSIX form is `NAME ()'. Both
accept any compound command as the body, not only a brace group.

Exit Status:
Zero, unless NAME is not a valid function name.")

    ("{" "{ LIST; }"
     "Group commands in the current shell."
     "LIST is executed in the CURRENT shell environment, so assignments and
`cd' persist afterwards. Unlike `( LIST )' no subshell is created. The braces
are reserved words, so they need surrounding blanks, and LIST needs a
terminating `;' or newline.

Redirections apply to the whole group.

Exit Status:
The status of the last command executed.")

    ("(" "( LIST )"
     "Execute commands in a subshell."
     "LIST is executed in a SUBSHELL: variable assignments, `cd', function
definitions and trap changes do not survive it. Redirections apply to the whole
group.

Exit Status:
The status of the last command executed.")

    ("((" "(( EXPRESSION ))"
     "Evaluate an arithmetic expression."
     "EXPRESSION is evaluated according to the arithmetic rules (see `let').
This is a command, not a substitution: it produces no output.

Exit Status:
Zero if EXPRESSION evaluates to a non-zero value, 1 otherwise -- the reverse of
the value's truthiness, so `(( i > 0 ))' reads naturally in an `if'.")

    ("[[" "[[ EXPRESSION ]]"
     "Evaluate a conditional expression."
     "Like `test', but a shell KEYWORD rather than a builtin, so its operands
are not subject to word splitting or pathname expansion and `<' and `>' are
comparison operators rather than redirections.

In addition to everything `test' accepts:
  STRING == PATTERN  unquoted PATTERN is a shell pattern, not a literal
  STRING != PATTERN  as above, negated
  STRING =~ REGEX    POSIX extended regular expression; the match and its
                     capture groups are left in the BASH_REMATCH array
  &&, ||             with the usual short-circuit evaluation
  ( )                grouping

Numeric operators (-eq and friends) evaluate their operands as ARITHMETIC
here, so `[[ e -eq 3 ]]' is true when e is `1+2'.

Exit Status:
Zero if EXPRESSION is true.")

    ("!" "! PIPELINE"
     "Negate the exit status of a pipeline."
     "PIPELINE is executed and its exit status inverted: zero becomes 1, any
non-zero becomes 0. `!' is a reserved word and needs a following blank.

Note that a negated pipeline is exempt from `set -e'.

Exit Status:
The logical negation of PIPELINE's status.")

    ("time" "time [-p] PIPELINE"
     "Report the time consumed by a pipeline."
     "PIPELINE is executed and the elapsed real time, user CPU time and system
CPU time are written to standard error when it finishes. -p selects the POSIX
output format.

`time' is a reserved word, so it can time any pipeline, including builtins and
functions that an external time(1) could not see.

Exit Status:
PIPELINE's status.")

    ("job_spec" "JOB_SPEC [&]"
     "Refer to a job."
     "Wherever a command expects a job, it may be named as:
  %N          job number N
  %STRING     the job whose command begins with STRING
  %?STRING    the job whose command contains STRING
  %%, %+      the current job
  %-          the previous job

A bare job spec as a command resumes that job in the foreground; with a
trailing `&' it resumes in the background. See `fg', `bg' and `jobs'.

Exit Status:
The status of the resumed job.")

    ("coproc" "coproc [NAME] COMMAND"
     "Run a command in the background with pipes to it."
     "COMMAND runs asynchronously with its standard input and output connected
to the shell through two pipes. The descriptors are placed in the array NAME
(default COPROC) as NAME[0] for reading from the coprocess and NAME[1] for
writing to it, and its process id in NAME_PID.

Exit Status:
Zero once the coprocess has been started.")))

(defun help-topic (name)
  "The (synopsis summary description) for NAME, from either table."
  (or (gethash name *builtin-help*)
      (let ((e (assoc name +syntax-help+ :test #'string=)))
        (and e (rest e)))))

(defun help-topic-names ()
  (let ((names '()))
    (maphash (lambda (k v) (declare (ignore v)) (push k names)) *builtin-help*)
    (dolist (e +syntax-help+) (push (first e) names))
    (sort (remove-duplicates names :test #'string=) #'string<)))

(defun help-matching-names (pattern)
  "Topics matching PATTERN. An exact name wins outright, so `help [' finds the
builtin rather than being read as an unterminated bracket expression."
  (if (help-topic pattern)
      (list pattern)
      (remove-if-not (lambda (n) (shell-pattern-match pattern n))
                     (help-topic-names))))

(defun help-print-columns (out names)
  "Two columns of synopses, as bash lists them. Cells are truncated rather than
wrapped: a wrapped cell would break the column alignment that makes the list
scannable."
  (let* ((cols (max 40 (terminal-columns)))
         (width (max 20 (floor (1- cols) 2)))
         (syns (mapcar (lambda (n)
                         (let ((s (or (first (help-topic n)) n)))
                           (if (> (length s) (1- width))
                               (subseq s 0 (1- width))
                               s)))
                       names))
         (rows (ceiling (length syns) 2))
         (v (coerce syns 'vector)))
    ;; Down the first column, then the second -- the order bash uses, so a
    ;; sorted list still reads alphabetically down the page.
    (dotimes (r rows)
      (let ((a (aref v r))
            (b (when (< (+ r rows) (length v)) (aref v (+ r rows)))))
        (if b
            (format out "~VA~A~%" width a b)
            (format out "~A~%" a))))))

(define-builtin "help" (sh args out)
  :synopsis "help [-dms] [PATTERN ...]"
  :summary "Display information about builtin commands."
  :description "Displays brief summaries of builtin commands and shell syntax.
With PATTERN, gives detailed help on all topics matching PATTERN, which is a
shell pattern -- `help re*' describes read, readarray, readonly and return.

Options:
  -d  output only a short description of each topic
  -m  display usage in pseudo-manpage format
  -s  output only a short usage synopsis for each topic

Arguments:
  PATTERN   a pattern naming a builtin or a shell syntax topic

Exit Status:
Zero unless no topic matches PATTERN."
  (let ((mode nil) (rest args))
    (loop while (and rest (> (length (first rest)) 1)
                     (char= (char (first rest) 0) #\-))
          do (let ((o (pop rest)))
               ;; `--' ends the options; `help -- help' must describe `help'.
               (when (string= o "--") (return))
               (loop for c across (subseq o 1) do
                 (case c
                   (#\d (setf mode :desc))
                   (#\s (setf mode :synopsis))
                   (#\m (setf mode :man))
                   (t (format *error-output* "help: -~C: invalid option~%" c)
                      (format *error-output*
                              "help: usage: help [-dms] [pattern ...]~%")
                      (return-from builtin 2))))))
    (cond
      ((null rest)
       (format out "sxsh, a POSIX shell with bash extensions.~%")
       (format out "These commands and syntax forms are defined internally.~%")
       (format out "Type `help name' to find out more about the function `name'.~%~%")
       (help-print-columns out (help-topic-names))
       0)
      (t
       (let ((status 0))
         (dolist (pattern rest)
           (let ((names (help-matching-names pattern)))
             (cond
               ((null names)
                (format *error-output*
                        "help: no help topics match `~A'.  Try `help help'.~%"
                        pattern)
                (setf status 1))
               (t
                (dolist (n names)
                  (destructuring-bind (synopsis summary description)
                      (help-topic n)
                    (case mode
                      (:synopsis (format out "~A: ~A~%" n synopsis))
                      (:desc (format out "~A - ~A~%" n summary))
                      (:man
                       (format out "NAME~%    ~A - ~A~%~%" n summary)
                       (format out "SYNOPSIS~%    ~A~%~%" synopsis)
                       (format out "DESCRIPTION~%    ~A~%~%" summary)
                       (help-print-indented out description)
                       (format out "~%SEE ALSO~%    sxsh(1)~%~%")
                       (format out "IMPLEMENTATION~%    sxsh~%"))
                      (t
                       (format out "~A: ~A~%" n synopsis)
                       (format out "    ~A~%" summary)
                       (when (plusp (length description))
                         (format out "~%")
                         (help-print-indented out description))))))))))
         status)))))

(defun help-print-indented (out text)
  "Write TEXT with each line indented four spaces, blank lines left blank.
The stored descriptions are written flush-left so they stay readable in the
source; the indentation is the printer's job."
  (let ((start 0) (n (length text)))
    (loop
      (let ((nl (position #\Newline text :start start)))
        (let ((line (subseq text start (or nl n))))
          (if (zerop (length line))
              (format out "~%")
              (format out "    ~A~%" line)))
        (unless nl (return))
        (setf start (1+ nl))))))
