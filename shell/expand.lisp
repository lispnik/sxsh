;;;; shell/expand.lisp --- word expansion (POSIX 2.6) + quote removal + globbing.
;;;;
;;;; Expansion order per the standard:
;;;;   1. tilde expansion, parameter expansion, command substitution,
;;;;      arithmetic expansion (done together in a single left-to-right pass)
;;;;   2. field splitting (on IFS), for results of unquoted expansions
;;;;   3. pathname expansion (globbing), unless set -f
;;;;   4. quote removal
;;;;
;;;; We track which characters came from an unquoted expansion so that field
;;;; splitting only applies there. This is done by building, alongside the
;;;; expanded string, a parallel "split-here" mask.

(in-package #:sxsh-shell)

;;; A field-builder accumulates characters plus per-character metadata:
;;;   :lit    ordinary literal (from source, unquoted) -- eligible for globbing
;;;   :quoted came from inside quotes or escaped -- never split, never glob
;;;   :split  came from an unquoted expansion -- eligible for IFS splitting
;;; Globbing metacharacters are active for :lit AND :split (POSIX 2.6 globs
;;; the results of unquoted expansions); :quoted never splits and never globs.

(defstruct (xchar (:constructor make-xchar (char class)))
  char class)                           ; class in (:lit :quoted :split)

(defparameter +default-ifs+
  (coerce (list #\Space #\Tab #\Newline) 'string)
  "IFS when the variable is unset. POSIX distinguishes unset from empty: unset
behaves as <space><tab><newline>, only an explicitly empty IFS disables field
splitting. Conflating them made `unset IFS' switch splitting off entirely.")

(defun expand-word-to-fields (sh raw &key (split t) (glob t) (tilde t) assignment)
  "Expand RAW (a word's raw source text) into a list of field strings.
When SPLIT is nil, no field splitting is done (assignment RHS, here-doc,
case word, redirection target -> single field). When GLOB is nil, no
pathname expansion. When ASSIGNMENT is true, tilde expands after ':' too."
  (let ((xchars (expand-pass sh raw :tilde tilde :assignment assignment)))
    (let ((fields (if split
                      (split-fields sh xchars)
                      (list xchars))))
      (let ((out '()))
        (dolist (f fields)
          (if (and glob (not (opt sh :noglob)) (field-has-glob-p f))
              (let ((matches (glob-field f)))
                (if matches
                    (dolist (m matches) (push m out))
                    (push (xchars->string f) out)))  ; no match -> literal
              (push (xchars->string f) out)))
        (nreverse out)))))

(defun xchars->string (xchars)
  "Quote removal: drop the quoting scaffolding, return the literal string.
Zero-width :anchor markers are dropped; :field-sep markers (from \"$@\" in a
non-splitting context) become a single space, matching the join behavior of
assignments like x=\"$@\"."
  (with-output-to-string (s)
    (dolist (xc xchars)
      (case (xchar-class xc)
        (:anchor)                        ; drop
        (:field-sep (write-char #\Space s))
        (t (write-char (xchar-char xc) s))))))

;;; ---------------------------------------------------------------------------
;;; Pass 1: tilde + parameter + command + arithmetic, honoring quotes
;;; ---------------------------------------------------------------------------

(defun expand-pass (sh raw &key (tilde t) assignment)
  "Return a list of XCHAR for RAW after step-1 expansions. When ASSIGNMENT is
true, an unquoted ':' also enables tilde expansion of a following '~' (the
PATH=~/a:~/b rule)."
  (let ((out '()) (i 0) (n (length raw)) (start-of-field t))
    (labels ((emit (ch class) (push (make-xchar ch class) out))
             (emit-anchor () (push (make-xchar #\Nul :anchor) out))
             (emit-field-sep () (push (make-xchar #\Nul :field-sep) out)))
      (loop while (< i n) do
        (let ((c (char raw i)))
          (cond
            ;; backslash escape (outside quotes): next char literal/quoted
            ((char= c #\\)
             (incf i)
             (when (< i n) (emit (char raw i) :quoted) (incf i))
             (setf start-of-field nil))
            ;; single quotes: everything literal-quoted
            ((char= c #\')
             (incf i)
             (emit-anchor)              ; keep an empty '' as a real field
             (loop while (and (< i n) (char/= (char raw i) #\'))
                   do (emit (char raw i) :quoted) (incf i))
             (incf i)                   ; closing '
             (setf start-of-field nil))
            ;; double quotes: expand $ and ` but mark chars quoted (no split/glob)
            ((char= c #\")
             (incf i)
             ;; POSIX special case: "$@" with no positionals produces no field
             ;; at all. Only emit the empty-field anchor when the quoted section
             ;; is NOT exactly $@ (so plain "" still yields one empty field).
             (unless (and (< (1+ i) n)
                          (char= (char raw i) #\$)
                          (char= (char raw (1+ i)) #\@)
                          (or (= (+ i 2) n)
                              (char= (char raw (+ i 2)) #\")))
               (emit-anchor))
             (setf i (expand-double sh raw i n #'emit #'emit-field-sep))
             (setf start-of-field nil))
            ;; tilde at start of field, unquoted
            ((and tilde (char= c #\~) start-of-field)
             (multiple-value-bind (str next) (expand-tilde sh raw i n)
               (dolist (ch (coerce str 'list)) (emit ch :quoted))
               (setf i next start-of-field nil)))
            ;; $'...' : C-style escapes, otherwise literal (POSIX Issue 8)
            ((and (char= c #\$) (< (1+ i) n) (char= (char raw (1+ i)) #\'))
             (multiple-value-bind (str next) (expand-dollar-quote raw (+ i 2) n)
               (emit-anchor)             ; $'' is still a field
               (dolist (ch (coerce str 'list)) (emit ch :quoted))
               (setf i next start-of-field nil)))
            ;; $@ / $* need special multi-field handling (see emit-at-params)
            ((and (char= c #\$) (< (1+ i) n) (char= (char raw (1+ i)) #\@))
             (emit-at-params sh #'emit #'emit-field-sep nil :split)
             (setf i (+ i 2) start-of-field nil))
            ((and (char= c #\$) (< (1+ i) n) (char= (char raw (1+ i)) #\*))
             ;; unquoted $* splits like $@ (each param a field, then IFS split)
             (emit-at-params sh #'emit #'emit-field-sep nil :split)
             (setf i (+ i 2) start-of-field nil))
            ;; $ expansions (unquoted -> splittable)
            ((char= c #\$)
             (multiple-value-bind (str next literal-quoted)
                 (expand-dollar sh raw i n)
               (dolist (ch (coerce str 'list))
                 (emit ch (if literal-quoted :quoted :split)))
               (setf i next start-of-field nil)))
            ;; backquote command substitution (unquoted -> splittable)
            ((char= c #\`)
             (multiple-value-bind (str next) (expand-backquote sh raw i n)
               (dolist (ch (coerce str 'list)) (emit ch :split))
               (setf i next start-of-field nil)))
            (t (emit c :lit) (incf i)
               ;; In an assignment context, tilde expansion is re-enabled after
               ;; the '=' and after each unquoted ':' -- that is what makes
               ;; both `x=~' and `PATH=~/a:~/b' work. Outside one, a tilde is
               ;; only special at the very start of the word.
               (setf start-of-field (and assignment
                                         (or (char= c #\:) (char= c #\=)))))))))
    (nreverse out)))

(defun dollar-quote-escape (raw i n)
  "Decode the backslash escape at RAW[i] inside $'...'.
Returns (values string next-index)."
  (let ((e (char raw (1+ i))))
    (macrolet ((ret (ch skip) `(values (string ,ch) (+ i ,skip))))
      (case e
        (#\a (ret (code-char 7) 2))
        (#\b (ret (code-char 8) 2))
        ((#\e #\E) (ret (code-char 27) 2))
        (#\f (ret (code-char 12) 2))
        (#\n (ret #\Newline 2))
        (#\r (ret #\Return 2))
        (#\t (ret #\Tab 2))
        (#\v (ret (code-char 11) 2))
        (#\\ (ret #\\ 2))
        (#\' (ret #\' 2))
        (#\" (ret #\" 2))
        (#\? (ret #\? 2))
        (#\x (let ((j (+ i 2)) (count 0))
               (let ((start j))
                 (loop while (and (< j n) (< count 2)
                                  (digit-char-p (char raw j) 16))
                       do (incf j) (incf count))
                 (if (> j start)
                     (values (string (code-char (parse-integer raw :start start
                                                                   :end j
                                                                   :radix 16)))
                             j)
                     (values "\\x" (+ i 2))))))
        ((#\u #\U)
         (let* ((limit (if (char= e #\u) 4 8)) (j (+ i 2)) (count 0)
                (start j))
           (loop while (and (< j n) (< count limit)
                            (digit-char-p (char raw j) 16))
                 do (incf j) (incf count))
           (if (> j start)
               (values (string (code-char (parse-integer raw :start start
                                                             :end j :radix 16)))
                       j)
               (values (concatenate 'string "\\" (string e)) (+ i 2)))))
        (#\c (if (< (+ i 2) n)
                 (values (string (code-char
                                  (logand (char-code
                                           (char-upcase (char raw (+ i 2))))
                                          #x1f)))
                         (+ i 3))
                 (values "\\c" (+ i 2))))
        (t (if (digit-char-p e 8)
               (let ((j (1+ i)) (count 0))
                 (let ((start j))
                   (loop while (and (< j n) (< count 3)
                                    (digit-char-p (char raw j) 8))
                         do (incf j) (incf count))
                   (values (string (code-char (parse-integer raw :start start
                                                                 :end j
                                                                 :radix 8)))
                           j)))
               (values (concatenate 'string "\\" (string e)) (+ i 2))))))))

(defun expand-dollar-quote (raw i n)
  "Expand a $'...' literal whose opening quote is at RAW[i-1].
Returns (values text index-past-the-closing-quote).

POSIX Issue 8 (2024) adopted this from ksh/bash. The body is single-quoted --
no parameter or command substitution -- but C-style backslash escapes are
interpreted, which is the only portable way to put a tab or newline into a
word without a literal one in the source."
  (let ((out (make-string-output-stream)))
    (loop
      (when (>= i n) (return))
      (let ((c (char raw i)))
        (cond
          ((char= c #\') (incf i) (return))
          ((and (char= c #\\) (< (1+ i) n))
           (multiple-value-bind (str next) (dollar-quote-escape raw i n)
             (write-string str out)
             (setf i next)))
          (t (write-char c out) (incf i)))))
    (values (get-output-stream-string out) i)))

(defun expand-double (sh raw i n emit &optional emit-field-sep)
  "Expand the interior of a double-quoted string starting just after the
opening quote at RAW[i-1]. Returns index just past the closing quote.
EMIT-FIELD-SEP, when provided, emits an explicit field boundary (used for
\"$@\" which yields one field per positional parameter even inside quotes)."
  (loop
    (when (>= i n) (return i))
    (let ((c (char raw i)))
      (cond
        ((char= c #\") (return (1+ i)))
        ((char= c #\\)
         ;; inside dquotes, backslash is literal except before $ ` " \ newline
         (if (and (< (1+ i) n)
                  (member (char raw (1+ i)) '(#\$ #\` #\" #\\ #\Newline)))
             (progn
               (when (char/= (char raw (1+ i)) #\Newline)
                 (funcall emit (char raw (1+ i)) :quoted))
               (incf i 2))
             (progn (funcall emit #\\ :quoted) (incf i))))
        ;; "$@" : one field per positional parameter, each quoted
        ((and (char= c #\$) (< (1+ i) n) (char= (char raw (1+ i)) #\@) emit-field-sep)
         (emit-at-params sh emit emit-field-sep t :quoted)
         (incf i 2))
        ;; "$*" : all params joined by IFS-first into a single quoted field
        ((and (char= c #\$) (< (1+ i) n) (char= (char raw (1+ i)) #\*))
         (dolist (ch (coerce (join-positional sh (ifs-first sh)) 'list))
           (funcall emit ch :quoted))
         (incf i 2))
        ((char= c #\$)
         (multiple-value-bind (str next) (expand-dollar sh raw i n)
           (dolist (ch (coerce str 'list)) (funcall emit ch :quoted))
           (setf i next)))
        ((char= c #\`)
         (multiple-value-bind (str next) (expand-backquote sh raw i n)
           (dolist (ch (coerce str 'list)) (funcall emit ch :quoted))
           (setf i next)))
        (t (funcall emit c :quoted) (incf i))))))

(defun expand-heredoc-body (sh raw)
  "Expand a here-document body whose delimiter was NOT quoted.

POSIX 2.7.4: the body is treated as if inside double quotes -- parameter,
command and arithmetic expansion apply -- except that the quote characters
themselves are ordinary text, and backslash is special only before $, `, \\
and newline.

Running the body through the general word expansion instead (which is what
this used to do) applied quote removal to it: a body containing a lone \"
silently lost it, and an unbalanced quote could derail the scan entirely.
There is no field splitting or pathname expansion here either."
  (let ((out '()) (i 0) (n (length raw)))
    (flet ((emit (ch) (push (make-xchar ch :quoted) out))
           (emit-string (s) (dolist (ch (coerce s 'list))
                              (push (make-xchar ch :quoted) out))))
      (loop while (< i n) do
        (let ((c (char raw i)))
          (cond
            ((char= c #\\)
             (if (and (< (1+ i) n)
                      (member (char raw (1+ i)) '(#\$ #\` #\\ #\Newline)))
                 (progn
                   ;; backslash-newline is a line continuation: both vanish
                   (when (char/= (char raw (1+ i)) #\Newline)
                     (emit (char raw (1+ i))))
                   (incf i 2))
                 (progn (emit #\\) (incf i))))
            ;; No field splitting happens here, so $@ and $* both join.
            ((and (char= c #\$) (< (1+ i) n)
                  (member (char raw (1+ i)) '(#\@ #\*)))
             (emit-string (join-positional sh (ifs-first sh)))
             (incf i 2))
            ((char= c #\$)
             (multiple-value-bind (str next) (expand-dollar sh raw i n)
               (emit-string str)
               (setf i next)))
            ((char= c #\`)
             (multiple-value-bind (str next) (expand-backquote sh raw i n)
               (emit-string str)
               (setf i next)))
            (t (emit c) (incf i))))))
    (nreverse out)))

(defun emit-at-params (sh emit emit-field-sep quoted-p class)
  "Emit the positional parameters as separate fields, inserting a field
separator between each. CLASS is the xchar class for the characters. When
there are zero positionals, nothing is emitted (so \"$@\" with no args yields
no field). QUOTED-P is informational; CLASS already encodes quoting."
  (declare (ignore quoted-p))
  (let ((params (coerce (shell-positional sh) 'list)))
    (loop for p in params
          for first = t then nil
          do (unless first (funcall emit-field-sep))
             (dolist (ch (coerce p 'list)) (funcall emit ch class)))))

;;; ---------------------------------------------------------------------------
;;; Tilde
;;; ---------------------------------------------------------------------------

(defun expand-tilde (sh raw i n)
  "RAW[i] is ~. Expand ~ / ~user up to the next / or end-of-field."
  (let ((j (1+ i)))
    (loop while (and (< j n) (not (member (char raw j) '(#\/ #\: nil))))
          do (incf j))
    (let ((name (subseq raw (1+ i) j)))
      (if (string= name "")
          (values (or (nth-value 0 (get-var sh "HOME"))
                      (namestring (user-homedir-pathname)))
                  j)
          ;; ~user: best-effort via getpwnam; fall back to literal
          (let ((home (ignore-errors
                       (sb-posix:passwd-dir (sb-posix:getpwnam name)))))
            (if home (values home j)
                (values (subseq raw i j) j)))))))

;;; ---------------------------------------------------------------------------
;;; $ parameter / command / arithmetic
;;; ---------------------------------------------------------------------------

(defun expand-dollar (sh raw i n)
  "RAW[i] is $. Returns (values expansion-string next-index literal-quoted-p).
LITERAL-QUOTED-P is T when the result must never be split (e.g. \"$@\" join)."
  (let ((j (1+ i)))
    (when (>= j n) (return-from expand-dollar (values "$" j nil)))
    (let ((c (char raw j)))
      (cond
        ;; $(( arithmetic )). Per POSIX, the expression first undergoes
        ;; parameter/command/arithmetic expansion and quote removal, then the
        ;; result is evaluated as an arithmetic expression.
        ((and (char= c #\() (< (1+ j) n) (char= (char raw (1+ j)) #\())
         (multiple-value-bind (expr next) (scan-until-close raw (+ j 2) n #\( #\) 2)
           (let ((expanded (xchars->string (expand-pass sh expr :tilde nil))))
             (values (princ-to-string (eval-arith sh expanded)) next nil))))
        ;; $( command )
        ((char= c #\()
         (multiple-value-bind (cmd next) (scan-until-close raw (1+ j) n #\( #\) 1)
           (values (command-substitute sh cmd) next nil)))
        ;; ${ parameter }
        ((char= c #\{)
         (multiple-value-bind (body next) (scan-until-close raw (1+ j) n #\{ #\} 1)
           (multiple-value-bind (val quoted) (expand-braced-param sh body)
             (values val next quoted))))
        ;; special single-char params
        ((char= c #\?) (values (princ-to-string (shell-last-status sh)) (1+ j) nil))
        ((char= c #\$) (values (princ-to-string (shell-pid sh)) (1+ j) nil))
        ((char= c #\!) (values (if (shell-last-bg-pid sh)
                                   (princ-to-string (shell-last-bg-pid sh)) "")
                               (1+ j) nil))
        ((char= c #\#) (values (princ-to-string (length (shell-positional sh)))
                               (1+ j) nil))
        ((char= c #\@) (values (join-positional sh " ") (1+ j) nil))
        ((char= c #\*) (values (join-positional sh (ifs-first sh)) (1+ j) nil))
        ((char= c #\0) (values (shell-name sh) (1+ j) nil))
        ((char= c #\-) (values (current-option-flags sh) (1+ j) nil))
        ;; $N positional
        ((digit-char-p c)
         (let ((k j))
           (loop while (and (< k n) (digit-char-p (char raw k))) do (incf k))
           (values (or (positional-ref sh (parse-integer raw :start j :end k)) "")
                   k nil)))
        ;; $name
        ((or (alpha-char-p c) (char= c #\_))
         (let ((k j))
           (loop while (and (< k n)
                            (let ((ch (char raw k)))
                              (or (alphanumericp ch) (char= ch #\_))))
                 do (incf k))
           (let ((vname (subseq raw j k)))
             (multiple-value-bind (val found) (get-var sh vname)
               (when (and (not found) (opt sh :nounset))
                 (error 'shell-unset-var :name vname))
               (values (or val "") k nil)))))
        (t (values "$" (1+ i) nil))))))

(defun current-option-flags (sh)
  "The $- string: one letter per currently-enabled shell option, plus 'i' when
interactive."
  (with-output-to-string (s)
    (when (shell-interactive sh) (write-char #\i s))
    (when (opt sh :errexit) (write-char #\e s))
    (when (opt sh :xtrace)   (write-char #\x s))
    (when (opt sh :nounset)  (write-char #\u s))
    (when (opt sh :noglob)   (write-char #\f s))
    (when (opt sh :monitor)  (write-char #\m s))
    (when (opt sh :allexport) (write-char #\a s))
    (when (opt sh :noclobber) (write-char #\C s))
    (when (opt sh :noexec)   (write-char #\n s))
    (when (opt sh :verbose)  (write-char #\v s))))

(defun current-ifs (sh)
  "The effective IFS: its value if set (possibly empty), else the default.
Every consumer must go through this -- treating unset as \"\" disabled field
splitting, joined \"$*\" with nothing, and made `read' split on spaces only."
  (multiple-value-bind (val found) (get-var sh "IFS")
    (if found val +default-ifs+)))

(defun ifs-first (sh)
  "The first IFS character, used to join \"$*\"."
  (let ((ifs (current-ifs sh)))
    (if (plusp (length ifs)) (string (char ifs 0)) "")))

(defun join-positional (sh sep)
  (with-output-to-string (s)
    (loop for i from 0 below (length (shell-positional sh))
          for first = t then nil
          do (unless first (write-string sep s))
             (write-string (aref (shell-positional sh) i) s))))

(defun scan-until-close (raw start n open close initial-depth)
  "From START, find the matching CLOSE at depth 0, honoring quotes. Returns
(values inner-string index-past-close)."
  (let ((depth initial-depth) (i start))
    (loop
      (when (>= i n) (error "unterminated expansion (missing ~C)" close))
      (let ((c (char raw i)))
        (cond
          ((char= c #\') (incf i)
           (loop while (and (< i n) (char/= (char raw i) #\')) do (incf i))
           (incf i))
          ((char= c #\") (incf i)
           (loop while (and (< i n) (char/= (char raw i) #\"))
                 do (when (char= (char raw i) #\\) (incf i)) (incf i))
           (incf i))
          ((char= c #\\) (incf i 2))
          ((char= c open) (incf depth) (incf i))
          ((char= c close) (decf depth)
           (if (zerop depth) (return (values (subseq raw start i) (1+ i)))
               (incf i)))
          (t (incf i)))))))

;;; ${...} with the common operators
(defun expand-braced-param (sh body)
  "Handle ${name}, ${#name}, ${name:-w} ${name:=w} ${name:?w} ${name:+w}
(and the non-colon variants), and ${name#pat} ${name##pat} ${name%pat}
${name%%pat}. Returns (values string quoted-p)."
  (when (string= body "") (return-from expand-braced-param (values "" nil)))
  ;; length: ${#name}
  (when (and (char= (char body 0) #\#) (> (length body) 1))
    (let ((name (subseq body 1)))
      (return-from expand-braced-param
        (values (princ-to-string
                 (cond ((string= name "@") (length (shell-positional sh)))
                       ((string= name "*") (length (shell-positional sh)))
                       (t (length (or (nth-value 0 (get-var sh name)) "")))))
                nil))))
  ;; find operator
  (let* ((ops '("##" "#" "%%" "%" ":-" ":=" ":?" ":+" "-" "=" "?" "+"))
         (oppos nil) (op nil))
    ;; name is leading run of name chars (or special @ * # ? $ ! or digits)
    (let ((k 0) (n (length body)))
      (cond
        ((and (plusp n) (or (alpha-char-p (char body 0)) (char= (char body 0) #\_)))
         (loop while (and (< k n)
                          (let ((ch (char body k)))
                            (or (alphanumericp ch) (char= ch #\_))))
               do (incf k)))
        ((and (plusp n) (digit-char-p (char body 0)))
         (loop while (and (< k n) (digit-char-p (char body k))) do (incf k)))
        ((plusp n) (setf k 1)))         ; special single-char param
      (let ((name (subseq body 0 k)) (rest (subseq body k)))
        ;; Substring expansion: ${name:offset} / ${name:offset:length}.
        ;; A ':' NOT followed by one of - = ? + introduces a substring, with
        ;; arithmetic offset and optional length.
        (when (and (>= (length rest) 1) (char= (char rest 0) #\:)
                   (or (= (length rest) 1)
                       (not (member (char rest 1) '(#\- #\= #\? #\+)))))
          (return-from expand-braced-param
            (values (substring-expand sh name (subseq rest 1)) nil)))
        ;; detect operator at start of REST
        (dolist (o ops)
          (when (and (>= (length rest) (length o))
                     (string= rest o :end1 (length o)))
            (setf op o oppos (length o)) (return)))
        (let ((word (if op (subseq rest oppos) nil))
              (val (param-value sh name)))
          (flet ((set-and-return (v)
                   (unless (special-param-p name) (set-var sh name v))
                   (values v nil)))
            (cond
              ((null op) (values (or val "") nil))
              ;; pattern removal
              ((string= op "#")  (values (remove-prefix (or val "") word nil) nil))
              ((string= op "##") (values (remove-prefix (or val "") word t) nil))
              ((string= op "%")  (values (remove-suffix (or val "") word nil) nil))
              ((string= op "%%") (values (remove-suffix (or val "") word t) nil))
              ;; use default
              ((string= op ":-") (if (nonempty val) (values val nil)
                                     (values (expand-nested sh word) nil)))
              ((string= op "-")  (if val (values val nil)
                                     (values (expand-nested sh word) nil)))
              ;; assign default
              ((string= op ":=") (if (nonempty val) (values val nil)
                                     (set-and-return (expand-nested sh word))))
              ((string= op "=")  (if val (values val nil)
                                     (set-and-return (expand-nested sh word))))
              ;; error if unset
              ((string= op ":?") (if (nonempty val) (values val nil)
                                     (error "~A: ~A" name
                                            (if (plusp (length word))
                                                (expand-nested sh word)
                                                "parameter null or not set"))))
              ((string= op "?")  (if val (values val nil)
                                     (error "~A: ~A" name
                                            (if (plusp (length word))
                                                (expand-nested sh word)
                                                "parameter not set"))))
              ;; use alternative
              ((string= op ":+") (if (nonempty val)
                                     (values (expand-nested sh word) nil)
                                     (values "" nil)))
              ((string= op "+")  (if val (values (expand-nested sh word) nil)
                                     (values "" nil)))
              (t (values (or val "") nil)))))))))

(defun substring-expand (sh name spec)
  "Implement ${name:offset:length}. SPEC is the text after the first colon,
i.e. 'offset' or 'offset:length'. Offset and length are arithmetic; a negative
offset counts from the end, a negative length is an end index from the string's
end. Out-of-range values are clamped, matching common shell behavior."
  (let* ((val (or (param-value sh name) ""))
         (len (length val))
         (colon (find-unnested-colon spec))
         (off-str (if colon (subseq spec 0 colon) spec))
         (len-str (if colon (subseq spec (1+ colon)) nil))
         (offset (ignore-errors (eval-arith sh (xchars->string (expand-pass sh off-str :tilde nil)))))
         (offset (or offset 0)))
    ;; normalize offset: negative counts from end
    (when (< offset 0) (setf offset (max 0 (+ len offset))))
    (setf offset (min offset len))
    (let ((end
            (if len-str
                (let ((l (or (ignore-errors (eval-arith sh (xchars->string (expand-pass sh len-str :tilde nil)))) 0)))
                  (if (< l 0)
                      (max offset (+ len l))   ; negative length: index from end
                      (min len (+ offset l))))
                len)))
      (subseq val offset end))))

(defun find-unnested-colon (s)
  "Position of the first ':' in S that is not inside parentheses (so the ':' of
a ternary inside an arithmetic offset is not mistaken for the length
separator). Returns NIL if none."
  (let ((depth 0))
    (dotimes (i (length s) nil)
      (case (char s i)
        (#\( (incf depth))
        (#\) (when (plusp depth) (decf depth)))
        (#\: (when (zerop depth) (return i)))))))

(defun special-param-p (name)
  (or (string= name "@") (string= name "*") (string= name "#")
      (string= name "?") (string= name "$") (string= name "!")
      (and (plusp (length name)) (every #'digit-char-p name))))

(defun param-value (sh name)
  (cond
    ((string= name "@") (join-positional sh " "))
    ((string= name "*") (join-positional sh (ifs-first sh)))
    ((string= name "#") (princ-to-string (length (shell-positional sh))))
    ((string= name "?") (princ-to-string (shell-last-status sh)))
    ((string= name "$") (princ-to-string (shell-pid sh)))
    ((string= name "0") (shell-name sh))
    ((string= name "-") (current-option-flags sh))
    ((string= name "!") (if (shell-last-bg-pid sh)
                            (princ-to-string (shell-last-bg-pid sh)) ""))
    ((and (plusp (length name)) (every #'digit-char-p name))
     (positional-ref sh (parse-integer name)))
    (t (nth-value 0 (get-var sh name)))))

(defun nonempty (v) (and v (plusp (length v))))

(defun expand-nested (sh word)
  "Expand a ${...} default/alternative word (no splitting, no globbing)."
  (xchars->string (expand-pass sh word)))

;;; pattern removal helpers (shell patterns: * ? [..])
(defun remove-prefix (s pattern greedy)
  "Remove the shortest (or longest, if GREEDY) prefix of S matching PATTERN."
  (let ((pat (expand-nested-pattern pattern))
        (candidates (if greedy
                        (loop for e from (length s) downto 0 collect e)
                        (loop for e from 0 to (length s) collect e))))
    (dolist (end candidates s)
      (when (shell-pattern-match pat (subseq s 0 end))
        (return-from remove-prefix (subseq s end))))))

(defun remove-suffix (s pattern greedy)
  "Remove the shortest (or longest, if GREEDY) suffix of S matching PATTERN."
  (let ((pat (expand-nested-pattern pattern))
        (candidates (if greedy
                        (loop for st from 0 to (length s) collect st)
                        (loop for st from (length s) downto 0 collect st))))
    (dolist (start candidates s)
      (when (shell-pattern-match pat (subseq s start))
        (return-from remove-suffix (subseq s 0 start))))))

(defun expand-nested-pattern (word)
  ;; patterns in ${..#..} are expanded but not quote-removed of their globs;
  ;; for our purposes expand params then treat as a pattern string.
  word)

(defun xchars->pattern (xchars)
  "Render XCHARS as a pattern string, keeping backslash escapes for anything
that was quoted in the source.

Plain quote removal (XCHARS->STRING) is wrong for a pattern: it drops the
backslash from `a\\*b' and leaves a live `*', so `case axb in a\\*b)' matched.
The matcher understands `\\c' as a literal c, so re-emitting the escape is
enough to keep quoted metacharacters inert."
  (with-output-to-string (out)
    (dolist (xc xchars)
      (case (xchar-class xc)
        (:anchor)
        (:field-sep (write-char #\Space out))
        (t
         (let ((c (xchar-char xc)))
           (when (and (eq (xchar-class xc) :quoted)
                      (find c "*?[]-\\"))
             (write-char #\\ out))
           (write-char c out)))))))

;;; ---------------------------------------------------------------------------
;;; Backquote command substitution
;;; ---------------------------------------------------------------------------

(defun expand-backquote (sh raw i n)
  "RAW[i] is `. Returns (values output next-index)."
  (let ((j (1+ i)) (buf (make-string-output-stream)))
    (loop
      (when (>= j n) (error "unterminated backquote"))
      (let ((c (char raw j)))
        (cond
          ((char= c #\`) (incf j) (return))
          ((and (char= c #\\) (< (1+ j) n)
                (member (char raw (1+ j)) '(#\` #\\ #\$)))
           (write-char (char raw (1+ j)) buf) (incf j 2))
          (t (write-char c buf) (incf j)))))
    (values (command-substitute sh (get-output-stream-string buf)) j)))

;;; ---------------------------------------------------------------------------
;;; Field splitting on IFS (2.6.5)
;;; ---------------------------------------------------------------------------

(defun split-fields (sh xchars)
  "Split XCHARS into fields. Two boundary kinds apply:
  * :field-sep markers are HARD boundaries (from \"$@\"): they always separate
    fields regardless of IFS.
  * IFS splitting applies to :split-classed characters between hard boundaries.
Quoted/literal characters never split, so a quoted empty string yields one
empty field."
  ;; A completely empty expansion (no chars, no anchors, no separators) yields
  ;; no fields at all -- this is the "$@" with no positionals case, and also an
  ;; unquoted empty variable that is the whole word.
  (when (null xchars)
    (return-from split-fields '()))
  ;; First cut on hard :field-sep boundaries.
  (let ((segments '()) (cur '()))
    (dolist (xc xchars)
      (if (eq (xchar-class xc) :field-sep)
          (progn (push (nreverse cur) segments) (setf cur nil))
          (push xc cur)))
    (push (nreverse cur) segments)
    (setf segments (nreverse segments))
    ;; If there were hard boundaries, each segment is its own field, but each
    ;; segment is still subject to IFS splitting of its :split content.
    (if (> (length segments) 1)
        (loop for seg in segments append (ifs-split-segment sh seg))
        (ifs-split-segment sh (first segments)))))

(defun ifs-split-segment (sh xchars)
  "Apply IFS field splitting to one segment (no hard boundaries inside).

POSIX 2.6.5 treats IFS as two kinds of character, and a delimiter is a whole
run rather than a single character: any amount of IFS whitespace, then at most
one IFS non-whitespace character, then any more IFS whitespace, together form
ONE field separator. Leading and trailing IFS whitespace is discarded.

Flushing on each IFS character independently -- as this once did -- turned
`a : b' with IFS=\" :\" into three fields, the middle one empty."
  (let ()
    (let* ((ifs (current-ifs sh))
           (ws (remove-if-not (lambda (c) (member c '(#\Space #\Tab #\Newline)))
                              (coerce ifs 'list)))
           (non-ws (remove-if (lambda (c) (member c '(#\Space #\Tab #\Newline)))
                              (coerce ifs 'list))))
      (when (string= ifs "")
        (return-from ifs-split-segment (list xchars)))   ; explicit IFS= : no split
      ;; No splittable characters => the segment is a single field verbatim
      ;; (quoted / literal case, incl. "" -> one empty field, and each "$@"
      ;; element which is emitted as :quoted).
      (unless (some (lambda (xc) (eq (xchar-class xc) :split)) xchars)
        (return-from ifs-split-segment (list xchars)))
      (let* ((v (coerce xchars 'vector))
             (n (length v))
             (i 0)
             (fields '()))
        (labels ((splittable (k) (eq (xchar-class (aref v k)) :split))
                 (ch (k) (xchar-char (aref v k)))
                 (ws-at (k) (and (splittable k) (member (ch k) ws)))
                 (delim-at (k) (and (splittable k) (member (ch k) non-ws)))
                 (ifs-at (k) (or (ws-at k) (delim-at k)))
                 (skip-ws () (loop while (and (< i n) (ws-at i)) do (incf i))))
          (skip-ws)                      ; leading IFS whitespace is discarded
          (loop
            ;; Stopping here rather than after consuming a separator is what
            ;; keeps a trailing delimiter from producing an empty last field.
            (when (>= i n) (return))
            (let ((start i))
              (loop while (and (< i n) (not (ifs-at i))) do (incf i))
              (push (coerce (subseq v start i) 'list) fields))
            ;; consume exactly one separator run
            (skip-ws)
            (when (and (< i n) (delim-at i))
              (incf i)
              (skip-ws))))
        (nreverse fields)))))

;;; ---------------------------------------------------------------------------
;;; Pathname expansion (globbing) -- honoring quoted vs literal metachars
;;; ---------------------------------------------------------------------------

(defun glob-active-p (class)
  "True for xchar classes whose metacharacters take part in pathname expansion.

POSIX 2.6 applies pathname expansion to the results of *unquoted* expansions,
so :split characters -- those produced by an unquoted parameter or command
substitution -- glob exactly like literal source text. Restricting this to
:lit, as it once was, meant `p=*.c; echo $p\' printed the pattern instead of
the files, and likewise for $(...) output and \"$@\" elements. Only :quoted is
exempt, which is what keeps `echo \"*\"' literal."
  (member class '(:lit :split)))

(defun field-has-glob-p (field)
  (some (lambda (xc) (and (glob-active-p (xchar-class xc))
                          (member (xchar-char xc) '(#\* #\? #\[))))
        field))

(defun glob-field (field)
  "Expand a field containing active glob metacharacters against the filesystem.
Returns a sorted list of matching pathnames, or NIL if none match."
  (let* ((pat (field->glob-pattern field))
         (absolute (and (plusp (length pat)) (char= (char pat 0) #\/)))
         (segments (remove "" (split-on-slash pat) :test #'string=))
         (start (if absolute (list "/") (list "."))))
    (let ((results (glob-segments start segments absolute)))
      (mapcar (lambda (p)
                ;; strip leading ./ we added
                (if (and (not absolute) (> (length p) 2)
                         (string= (subseq p 0 2) "./"))
                    (subseq p 2) p))
              (sort results #'string<)))))

(defun field->glob-pattern (field)
  "Turn a field into a pattern string where quoted metachars are escaped with
NUL sentinels so they match literally."
  (with-output-to-string (s)
    (dolist (xc field)
      (unless (member (xchar-class xc) '(:anchor :field-sep))
        (let ((c (xchar-char xc)))
          (if (and (member c '(#\* #\? #\[ #\]))
                   (not (glob-active-p (xchar-class xc))))
              (progn (write-char #\Nul s) (write-char c s))  ; literal metachar
              (write-char c s)))))))

(defun split-on-slash (s)
  (loop with start = 0 with out = '()
        for i from 0 below (length s)
        when (char= (char s i) #\/)
          do (push (subseq s start i) out) (setf start (1+ i))
        finally (push (subseq s start) out) (return (nreverse out))))

(defun glob-segments (dirs segments absolute)
  (if (null segments)
      dirs
      (let ((seg (first segments)) (next '()))
        (dolist (dir dirs)
          (dolist (name (directory-entries dir))
            (when (glob-segment-match seg name)
              (push (path-join dir name absolute) next))))
        (glob-segments (nreverse next) (rest segments) absolute))))

(defun path-join (dir name absolute)
  (declare (ignore absolute))
  (cond ((string= dir "/") (concatenate 'string "/" name))
        ((string= dir ".") (concatenate 'string "./" name))
        (t (concatenate 'string dir "/" name))))

(defun directory-entries (dir)
  (handler-case
      (let ((entries '()))
        (let ((d (sb-posix:opendir dir)))
          (unwind-protect
               (loop for ent = (sb-posix:readdir d)
                     until (sb-alien:null-alien ent)
                     for name = (sb-posix:dirent-name ent)
                     do (push name entries))
            (sb-posix:closedir d))
          entries))
    (error () '())))

(defun glob-segment-match (pattern name)
  "Match one path segment. Leading-dot files are not matched by a leading * or ?."
  (let ((real (unescape-glob pattern)))
    (when (and (plusp (length name)) (char= (char name 0) #\.)
               (not (and (plusp (length real)) (char= (char real 0) #\.))))
      (return-from glob-segment-match nil))
    (and (not (string= name ".")) (not (string= name ".."))
         (shell-pattern-match real name))
    ;; allow . and .. only on explicit literal patterns
    (cond
      ((or (string= real ".") (string= real "..")) (string= real name))
      ((or (string= name ".") (string= name "..")) nil)
      (t (shell-pattern-match real name)))))

(defun unescape-glob (pattern)
  "Remove NUL sentinels but keep the following char literal for matching by
converting it into a bracket expression when it's a metachar."
  (with-output-to-string (s)
    (let ((i 0) (n (length pattern)))
      (loop while (< i n) do
        (if (and (char= (char pattern i) #\Nul) (< (1+ i) n))
            (let ((c (char pattern (1+ i))))
              ;; escape metachar by wrapping in [ ]
              (if (member c '(#\* #\? #\[ #\]))
                  (progn (write-char #\[ s) (write-char c s) (write-char #\] s))
                  (write-char c s))
              (incf i 2))
            (progn (write-char (char pattern i) s) (incf i)))))))

;;; ---------------------------------------------------------------------------
;;; Shell pattern matching (fnmatch-like: * ? [set])
;;; ---------------------------------------------------------------------------

(defun shell-pattern-match (pattern string)
  "Match STRING fully against a shell PATTERN (*, ?, [..])."
  (pat-match pattern 0 (length pattern) string 0 (length string)))

(defun pat-match (p pp pe s si sn)
  (loop
    (when (>= pp pe) (return (>= si sn)))
    (let ((pc (char p pp)))
      (cond
        ;; a backslash makes the next pattern character literal
        ((and (char= pc #\\) (< (1+ pp) pe))
         (when (>= si sn) (return nil))
         (unless (char= (char p (1+ pp)) (char s si)) (return nil))
         (incf pp 2) (incf si))
        ((char= pc #\*)
         (incf pp)
         ;; collapse consecutive *
         (loop while (and (< pp pe) (char= (char p pp) #\*)) do (incf pp))
         (when (>= pp pe) (return t))   ; trailing * matches rest
         (loop for k from si to sn
               when (pat-match p pp pe s k sn) do (return-from pat-match t))
         (return nil))
        ((char= pc #\?)
         (when (>= si sn) (return nil))
         (incf pp) (incf si))
        ((char= pc #\[)
         (multiple-value-bind (ok next) (match-bracket p pp pe s si sn)
           (unless ok (return nil))
           (setf pp next) (incf si)))
        (t
         (when (or (>= si sn) (char/= pc (char s si))) (return nil))
         (incf pp) (incf si))))))

(defun char-class-member-p (name ch)
  "True if CH belongs to the POSIX character class NAME (without the [: :])."
  (cond
    ((string= name "alpha") (alpha-char-p ch))
    ((string= name "digit") (digit-char-p ch))
    ((string= name "alnum") (alphanumericp ch))
    ((string= name "upper") (upper-case-p ch))
    ((string= name "lower") (lower-case-p ch))
    ((string= name "space") (member ch '(#\Space #\Tab #\Newline #\Page #\Return
                                         #\Vt)))
    ((string= name "blank") (member ch '(#\Space #\Tab)))
    ((string= name "print") (and (graphic-char-p ch) (char/= ch #\Rubout)))
    ((string= name "graph") (and (graphic-char-p ch) (char/= ch #\Space)
                                 (char/= ch #\Rubout)))
    ((string= name "cntrl") (or (< (char-code ch) 32) (= (char-code ch) 127)))
    ((string= name "punct") (and (graphic-char-p ch)
                                 (char/= ch #\Space)
                                 (not (alphanumericp ch))))
    ((string= name "xdigit") (digit-char-p ch 16))
    (t nil)))                           ; unknown class matches nothing

(defun bracket-delimited (p i pe open close)
  "Scan a [: :] / [= =] / [. .] construct starting at P[i] (the '[').
Returns (values contents index-past-close) or (values nil nil) if unterminated."
  (when (and (< (1+ i) pe) (char= (char p (1+ i)) open))
    (let ((j (+ i 2)))
      (loop
        (when (>= (1+ j) pe) (return (values nil nil)))
        (when (and (char= (char p j) close) (char= (char p (1+ j)) #\]))
          (return (values (subseq p (+ i 2) j) (+ j 2))))
        (incf j)))))

(defun match-bracket (p pp pe s si sn)
  "Match a [..] bracket expression at P[pp]. Returns (values matched-p next-pp).

Handles the POSIX bracket-expression forms (XBD 9.3.5): negation with ! or ^,
ranges, character classes [:alpha:], equivalence classes [=a=] and collating
symbols [.a.]. The class forms are what `[[:digit:]]' relies on; without them
that pattern was read as the ordinary set {[,:,d,i,g,t} and matched the wrong
characters entirely."
  (when (>= si sn) (return-from match-bracket (values nil pe)))
  (let ((i (1+ pp)) (negate nil) (matched nil) (ch (char s si)))
    (when (and (< i pe) (member (char p i) '(#\! #\^)))
      (setf negate t) (incf i))
    (let ((start i))
      (loop
        (when (>= i pe) (return-from match-bracket (values nil pe))) ; no close
        (let ((c (char p i)))
          (cond
            ((and (char= c #\]) (> i start))
             (incf i) (return))
            ;; escaped member: [a\-z] lists a, - and z rather than a range
            ((and (char= c #\\) (< (1+ i) pe))
             (when (char= (char p (1+ i)) ch) (setf matched t))
             (incf i 2))
            ;; [:class:]
            ((char= c #\[)
             (multiple-value-bind (name next) (bracket-delimited p i pe #\: #\:)
               (cond
                 (name (when (char-class-member-p name ch) (setf matched t))
                       (setf i next))
                 (t
                  ;; [=a=] and [.a.] have no locale weight here: both stand for
                  ;; the literal character they enclose.
                  (multiple-value-bind (eq next) (bracket-delimited p i pe #\= #\=)
                    (multiple-value-bind (coll cnext)
                        (if eq (values nil nil) (bracket-delimited p i pe #\. #\.))
                      (let ((lit (or eq coll)) (nxt (or next cnext)))
                        (cond
                          (lit (when (and (plusp (length lit))
                                          (char= (char lit 0) ch))
                                 (setf matched t))
                               (setf i nxt))
                          (t (when (char= c ch) (setf matched t))
                             (incf i))))))))))
            ;; range a-z
            ((and (< (+ i 2) pe) (char= (char p (1+ i)) #\-)
                  (char/= (char p (+ i 2)) #\]))
             (when (char<= (char p i) ch (char p (+ i 2))) (setf matched t))
             (incf i 3))
            (t (when (char= c ch) (setf matched t)) (incf i))))))
    (values (if negate (not matched) matched) i)))

;;; ---------------------------------------------------------------------------
;;; Command substitution and arithmetic hook into the executor / evaluator
;;; (defined in exec.lisp and arith.lisp; declared here)
;;; ---------------------------------------------------------------------------

(declaim (ftype (function (t string) string) command-substitute))
(declaim (ftype (function (t string) integer) eval-arith))
