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
              (let ((matches (glob-field f :dotglob (shopt-p sh "dotglob")
                                       :skipdots (shopt-p sh "globskipdots"))))
                (cond
                  (matches (dolist (m matches) (push m out)))
                  ;; bash `nullglob': a pattern that matches nothing expands
                  ;; to nothing at all rather than to itself.
                  ((shopt-p sh "nullglob"))
                  (t (push (xchars->string f) out))))
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
        ;; `$*' joins with IFS's first character, not a space -- `IFS=:;
        ;; set -- x "y z"; s=$*' gives `x:y z'. The character travels on the
        ;; marker because quote removal cannot reach the shell to ask for IFS.
        ;; #\Nul means IFS was empty, so the parts abut.
        (:star-sep (unless (char= (xchar-char xc) #\Nul)
                     (write-char (xchar-char xc) s)))
        (t (write-char (xchar-char xc) s))))))

;;; ---------------------------------------------------------------------------
;;; Pass 1: tilde + parameter + command + arithmetic, honoring quotes
;;; ---------------------------------------------------------------------------

(defun expansion-value->string (v)
  "An expansion result as a plain string, whether it arrived as one or as
XCHARs."
  (if (stringp v) v (xchars->string v)))

(defun emit-expansion (value emit default-class)
  "Emit an expansion result through EMIT.

VALUE is either a string -- every character takes DEFAULT-CLASS -- or a list of
XCHAR that already carries its own classes. The latter is how quoting INSIDE a
${...} operand survives: `${undef:-\"2 3\" \"4 5\"}' unquoted must yield the two
fields `2 3' and `4 5', so the quoted spaces have to stay unsplittable while
the space between the two parts becomes a separator.

In a quoted context the whole expansion is quoted and DEFAULT-CLASS wins.
Otherwise anything that was not quoted inside the operand becomes :SPLIT --
being merely literal is not enough, because these characters did come from an
expansion and POSIX splits them."
  (if (stringp value)
      (dolist (ch (coerce value 'list)) (funcall emit ch default-class))
      (dolist (xc value)
        (case (xchar-class xc)
          (:anchor (funcall emit (xchar-char xc) :anchor))
          (t (funcall emit (xchar-char xc)
                     (cond ((eq default-class :quoted) :quoted)
                           ((eq (xchar-class xc) :quoted) :quoted)
                           (t :split))))))))

(defun expand-pass (sh raw &key (tilde t) assignment)
  "Return a list of XCHAR for RAW after step-1 expansions. When ASSIGNMENT is
true, an unquoted ':' also enables tilde expansion of a following '~' (the
PATH=~/a:~/b rule)."
  (let ((out '()) (i 0) (n (length raw)) (start-of-field t))
    (labels ((emit (ch class) (push (make-xchar ch class) out))
             (emit-anchor () (push (make-xchar #\Nul :anchor) out))
             (emit-star-sep ()
               (let ((f (ifs-first sh)))
                 (push (make-xchar (if (plusp (length f)) (char f 0) #\Nul)
                                   :star-sep)
                       out)))
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
             ;; POSIX special case: an at-expansion that yields nothing must
             ;; produce NO field, so the empty-field anchor is suppressed when
             ;; the quoted section is exactly one at-expansion. Plain "" still
             ;; yields one empty field.
             (unless (quoted-at-only-p raw i n)
               (emit-anchor))
             (setf i (expand-double sh raw i n #'emit #'emit-field-sep))
             (setf start-of-field nil))
            ;; tilde at start of field, unquoted
            ((and tilde (char= c #\~) start-of-field)
             (multiple-value-bind (str next) (expand-tilde sh raw i n)
               (dolist (ch (coerce str 'list)) (emit ch :quoted))
               (setf i next start-of-field nil)))
            ;; $"..." : a locale-translated string. With no message catalog
            ;; that is exactly "...", so the $ is dropped and the next pass of
            ;; the loop handles the double quote normally.
            ((and (char= c #\$) (< (1+ i) n) (char= (char raw (1+ i)) #\"))
             (incf i))
            ;; $'...' : C-style escapes, otherwise literal (POSIX Issue 8)
            ((and (char= c #\$) (< (1+ i) n) (char= (char raw (1+ i)) #\'))
             (multiple-value-bind (str next) (expand-dollar-quote raw (+ i 2) n)
               (emit-anchor)             ; $'' is still a field
               (dolist (ch (coerce str 'list)) (emit ch :quoted))
               (setf i next start-of-field nil)))
            ;; ${a[@]} / ${a[*]} unquoted: one field per element, then IFS
            ((braced-array-at-p sh raw i n :star t)
             (multiple-value-bind (arr next) (braced-array-at-p sh raw i n :star t)
               (emit-array-elements arr #'emit #'emit-field-sep :split)
               (setf i next start-of-field nil)))
            ;; $@ / $* need special multi-field handling (see emit-at-params)
            ((and (char= c #\$) (< (1+ i) n) (char= (char raw (1+ i)) #\@))
             (emit-at-params sh #'emit #'emit-field-sep nil :split)
             (setf i (+ i 2) start-of-field nil))
            ((and (char= c #\$) (< (1+ i) n) (char= (char raw (1+ i)) #\*))
             ;; Unquoted $* splits like $@ -- each parameter is a field, then
             ;; IFS splitting applies. It differs only when there is NO
             ;; splitting (an assignment RHS), where it joins on IFS's first
             ;; character rather than a space, so it needs its own separator.
             (emit-at-params sh #'emit #'emit-star-sep nil :split)
             (setf i (+ i 2) start-of-field nil))
            ;; $ expansions (unquoted -> splittable)
            ((char= c #\$)
             (multiple-value-bind (str next literal-quoted)
                 (expand-dollar sh raw i n)
               (emit-expansion str #'emit (if literal-quoted :quoted :split))
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
        ;; "${a[@]}" : one field per array element, each quoted
        ((and emit-field-sep (braced-array-at-p sh raw i n))
         (multiple-value-bind (arr next) (braced-array-at-p sh raw i n)
           (emit-array-elements arr emit emit-field-sep :quoted)
           (setf i next)))
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
           (emit-expansion str emit :quoted)
           (setf i next)))
        ((char= c #\`)
         (multiple-value-bind (str next)
             (expand-backquote sh raw i n :in-dquotes t)
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
               (emit-string (expansion-value->string str))
               (setf i next)))
            ((char= c #\`)
             (multiple-value-bind (str next) (expand-backquote sh raw i n)
               (emit-string str)
               (setf i next)))
            (t (emit c) (incf i))))))
    (nreverse out)))

(defun quoted-at-only-p (raw i n)
  "True if the double-quoted section starting at I is exactly one at-expansion
-- `$@' or `${name[@]}' -- and nothing else.

Whether the array exists is deliberately not consulted: \"${undefined[@]}\" and
\"${empty[@]}\" both have to expand to zero fields, and neither has an array to
ask. Matching only the literal `$@', as this once did, meant `read -a' on an
empty line produced one empty field instead of none."
  (let ((end nil))
    (cond
      ((and (< (1+ i) n) (char= (char raw i) #\$) (char= (char raw (1+ i)) #\@))
       (setf end (+ i 2)))
      ((and (< (1+ i) n) (char= (char raw i) #\$) (char= (char raw (1+ i)) #\{))
       (let ((close (position #\} raw :start (+ i 2))))
         (when close
           (multiple-value-bind (base sub)
               (split-subscript (subseq raw (+ i 2) close))
             (declare (ignore base))
             (when (and sub (string= sub "@")) (setf end (1+ close))))))))
    (and end (or (= end n) (char= (char raw end) #\")))))

(defun braced-array-at-p (sh raw i n &key star)
  "If RAW at I is `${name[@]}', return (values array end-index); else NIL.

With STAR, `${name[*]}' matches too. Inside double quotes it must not: there
`[*]' joins the elements into one field, exactly as `\"$*\"' differs from
`\"$@\"'. UNQUOTED, though, the two behave alike -- `IFS=\"\"; argv ${a[*]}'
yields one field per element, which joining silently got wrong.

Recognised here rather than in EXPAND-BRACED-PARAM because only the caller can
emit field separators."
  (when (and (< (+ i 1) n) (char= (char raw i) #\$) (char= (char raw (1+ i)) #\{))
    (let ((close (position #\} raw :start (+ i 2))))
      (when close
        (let ((body (subseq raw (+ i 2) close)))
          (multiple-value-bind (base sub) (split-subscript body)
            (let ((arr (and sub (or (string= sub "@")
                                    (and star (string= sub "*")))
                            (var-array sh base))))
              (when arr (values arr (1+ close))))))))))

(defun emit-array-elements (arr emit emit-field-sep class)
  "Emit each element of ARR as its own field, like EMIT-AT-PARAMS does for $@."
  (let ((values (array-values arr)))
    ;; Unquoted, an empty element contributes no field at all.
    (unless (eq class :quoted)
      (setf values (remove-if (lambda (v) (zerop (length v))) values)))
    (loop for v in values
          for first = t then nil
          do (unless first (funcall emit-field-sep))
             (dolist (ch (coerce v 'list)) (funcall emit ch class)))))

(defun emit-at-params (sh emit emit-field-sep quoted-p class)
  "Emit the positional parameters as separate fields, inserting a field
separator between each. CLASS is the xchar class for the characters. When
there are zero positionals, nothing is emitted (so \"$@\" with no args yields
no field).

Unquoted, an EMPTY parameter contributes no field: `set -- a \"\" b; argv $@'
is two arguments, while the quoted form keeps the empty one. Emitting a
separator for it regardless left a stray empty field on the end."
  (let ((params (coerce (shell-positional sh) 'list)))
    (unless quoted-p
      (setf params (remove-if (lambda (p) (zerop (length p))) params)))
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
               ;; $RANDOM / $SECONDS have no entry in the table until someone
               ;; assigns one, so they are resolved on the miss path.
               (unless found
                 (let ((dyn (dynamic-var sh vname)))
                   (when dyn (setf val dyn found t))))
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
(values inner-string index-past-close).

A `)' that ends a CASE PATTERN is not a closing paren, and it has no opening
one to balance it: `$(case $x in a) echo A;; esac)' ended the substitution at
`a)', leaving `echo A;; esac)' to be parsed as a separate command. Words are
therefore tracked well enough to count `case' and `esac', and while a case
statement is open an unbalanced CLOSE is taken as a pattern terminator.

`case' only counts in command position, so `echo case' does not open one --
a false positive there would swallow the rest of the input looking for a
close that never comes."
  (let ((depth initial-depth) (i start) (case-depth 0) (cmd-pos t))
    (flet ((word-at (k)
             ;; The run of name characters starting at K, or NIL.
             (let ((e k))
               (loop while (and (< e n)
                                (or (alphanumericp (char raw e))
                                    (char= (char raw e) #\_)))
                     do (incf e))
               (and (> e k) (values (subseq raw k e) e)))))
      (loop
        (when (>= i n) (error "unterminated expansion (missing ~C)" close))
        (let ((c (char raw i)))
          (cond
            ((char= c #\') (incf i)
             (loop while (and (< i n) (char/= (char raw i) #\')) do (incf i))
             (incf i) (setf cmd-pos nil))
            ((char= c #\") (incf i)
             (loop while (and (< i n) (char/= (char raw i) #\"))
                   do (when (char= (char raw i) #\\) (incf i)) (incf i))
             (incf i) (setf cmd-pos nil))
            ((char= c #\\) (incf i 2) (setf cmd-pos nil))
            ;; A separator puts us back in command position.
            ((member c '(#\; #\& #\| #\Newline)) (incf i) (setf cmd-pos t))
            ((member c '(#\Space #\Tab)) (incf i))
            ((char= c open) (incf depth) (incf i) (setf cmd-pos t))
            ((char= c close)
             (cond
               ((> depth 1) (decf depth) (incf i) (setf cmd-pos nil))
               ;; Unbalanced, but a case is open: this ends a pattern.
               ((and (= depth 1) (plusp case-depth)) (incf i) (setf cmd-pos t))
               (t (decf depth)
                  (if (zerop depth)
                      (return (values (subseq raw start i) (1+ i)))
                      (progn (incf i) (setf cmd-pos nil))))))
            (t
             (multiple-value-bind (w e) (word-at i)
               (cond
                 (w (when (and cmd-pos (string= w "case")) (incf case-depth))
                    (when (string= w "esac")
                      (setf case-depth (max 0 (1- case-depth))))
                    ;; `in' keeps us in the position where patterns follow.
                    (setf cmd-pos (string= w "in"))
                    (setf i e))
                 (t (setf cmd-pos nil) (incf i)))))))))))

;;; ${...} with the common operators
(defun expand-braced-param (sh body)
  "Handle ${name}, ${#name}, ${name:-w} ${name:=w} ${name:?w} ${name:+w}
(and the non-colon variants), and ${name#pat} ${name##pat} ${name%pat}
${name%%pat}. Returns (values string quoted-p).

Also the bash extensions, which POSIX does not have: ${name/pat/rep} and its
//, /# and /% variants, ${name^} ${name^^} ${name,} ${name,,} for case
mapping, ${!name} for indirection and ${!prefix*} / ${!prefix@} for matching
variable names."
  (when (string= body "") (return-from expand-braced-param (values "" nil)))
  ;; ${!name} indirection, and ${!prefix*} / ${!prefix@} name listing. Checked
  ;; before the length branch so that ${!x} is not read as a name of "!x".
  (when (and (char= (char body 0) #\!) (> (length body) 1))
    (let ((rest (subseq body 1)))
      (return-from expand-braced-param
        (cond
          ;; ${!prefix*} and ${!prefix@}: the NAMES that start with prefix,
          ;; sorted, space separated.
          ((and (> (length rest) 1)
                (member (char rest (1- (length rest))) '(#\* #\@)))
           (let ((prefix (subseq rest 0 (1- (length rest))))
                 (names '()))
             (maphash (lambda (k v)
                        (declare (ignore v))
                        (when (and (>= (length k) (length prefix))
                                   (string= prefix k :end2 (length prefix)))
                          (push k names)))
                      (shell-vars sh))
             (values (format nil "~{~A~^ ~}" (sort names #'string<)) nil)))
          ;; ${!a[@]} / ${!a[*]}: the SUBSCRIPTS in use, not the values.
          ((multiple-value-bind (base sub) (split-subscript rest)
             (let ((arr (var-array sh base)))
               (and arr sub (member sub '("@" "*") :test #'string=)
                    (values (format nil "~{~A~^ ~}" (array-keys arr)) nil)))))
          ;; ${!name}: the value of the variable NAMED by name.
          (t (values (or (nth-value 0 (get-var sh (or (param-value sh rest) "")))
                         "")
                     nil))))))
  ;; length: ${#name}
  (when (and (char= (char body 0) #\#) (> (length body) 1))
    (let ((name (subseq body 1)))
      (return-from expand-braced-param
        (values (princ-to-string
                 (cond ((string= name "@") (length (shell-positional sh)))
                       ((string= name "*") (length (shell-positional sh)))
                       (t (multiple-value-bind (base sub) (split-subscript name)
                            (let ((arr (var-array sh base)))
                              (cond
                                ;; ${#a[@]} / ${#a[*]}: how many elements
                                ((and arr sub (member sub '("@" "*") :test #'string=))
                                 (length (array-keys arr)))
                                ;; ${#a[i]}: length of one element
                                ((and arr sub)
                                 (length (or (array-get arr (array-subscript sh arr sub)) "")))
                                (t (length (or (nth-value 0 (get-var sh name)) "")))))))))
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
      ;; An array subscript belongs to the name: `${a[1]#x}' must see the
      ;; name as `a[1]', not `a'.
      (when (and (< k n) (char= (char body k) #\[))
        (let ((close (position #\] body :start k :from-end t)))
          (when close (setf k (1+ close)))))
      (let ((name (subseq body 0 k)) (rest (subseq body k)))
        ;; Substring expansion: ${name:offset} / ${name:offset:length}.
        ;; A ':' NOT followed by one of - = ? + introduces a substring, with
        ;; arithmetic offset and optional length.
        (when (and (>= (length rest) 1) (char= (char rest 0) #\:)
                   (or (= (length rest) 1)
                       (not (member (char rest 1) '(#\- #\= #\? #\+)))))
          (return-from expand-braced-param
            (values (substring-expand sh name (subseq rest 1)) nil)))
        ;; bash: ${name@OP} parameter transformation. Before the operator
        ;; table, which has no `@' in it and would fall through to the plain
        ;; value -- which is how `${v@Q}' used to expand to v unquoted.
        (when (and (= (length rest) 2) (char= (char rest 0) #\@))
          (return-from expand-braced-param
            (values (transform-expand sh name (char rest 1)) nil)))
        ;; bash: ${name/pat/rep}. Handled before the operator table because
        ;; the pattern may itself contain any of those operator characters.
        (when (and (plusp (length rest)) (char= (char rest 0) #\/))
          (return-from expand-braced-param
            (values (substitute-expand sh (or (param-value sh name) "")
                                       (subseq rest 1))
                    nil)))
        ;; bash: ${name^} ${name^^} ${name,} ${name,,} case mapping. The
        ;; doubled form maps every character, the single form only the first.
        (when (and (plusp (length rest))
                   (member (char rest 0) '(#\^ #\,)))
          (let* ((ch (char rest 0))
                 (allp (and (> (length rest) 1) (char= (char rest 1) ch)))
                 (pat (subseq rest (if allp 2 1)))
                 (fn (if (char= ch #\^) #'char-upcase #'char-downcase)))
            (return-from expand-braced-param
              (values (map-case (or (param-value sh name) "") fn allp
                                (if (string= pat "") nil
                                    (expand-nested-pattern sh pat)))
                      nil))))
        ;; detect operator at start of REST
        (dolist (o ops)
          (when (and (>= (length rest) (length o))
                     (string= rest o :end1 (length o)))
            (setf op o oppos (length o)) (return)))
        (let ((word (if op (subseq rest oppos) nil))
              (val (param-value sh name)))
          (flet ((set-and-return (v)
                   (unless (special-param-p name)
                     (set-var sh name (expansion-value->string v)))
                   (values v nil)))
            (cond
              ((null op) (values (or val "") nil))
              ;; pattern removal
              ((string= op "#")  (values (remove-prefix sh (or val "") word nil) nil))
              ((string= op "##") (values (remove-prefix sh (or val "") word t) nil))
              ((string= op "%")  (values (remove-suffix sh (or val "") word nil) nil))
              ((string= op "%%") (values (remove-suffix sh (or val "") word t) nil))
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
                                                (expand-nested-string sh word)
                                                "parameter null or not set"))))
              ((string= op "?")  (if val (values val nil)
                                     (error "~A: ~A" name
                                            (if (plusp (length word))
                                                (expand-nested-string sh word)
                                                "parameter not set"))))
              ;; use alternative
              ((string= op ":+") (if (nonempty val)
                                     (values (expand-nested sh word) nil)
                                     (values "" nil)))
              ((string= op "+")  (if val (values (expand-nested sh word) nil)
                                     (values "" nil)))
              (t (values (or val "") nil)))))))))

(defun quote-for-reuse (s)
  "S written so the shell reads it back unchanged, as bash's ${v@Q} writes it.

Not the same rendering as printf's %q, which backslash-escapes each special
character: @Q always quotes, so `plain' becomes 'plain'. A value holding a
control character needs the $'...' form, which is the only one that can
express a newline on one line."
  (if (some (lambda (c) (< (char-code c) 32)) s)
      (with-output-to-string (o)
        (write-string "$'" o)
        (loop for c across s
              do (case c
                   (#\Newline (write-string "\\n" o))
                   (#\Tab (write-string "\\t" o))
                   (#\Return (write-string "\\r" o))
                   (t (if (< (char-code c) 32)
                          (format o "\\x~2,'0x" (char-code c))
                          (progn (when (find c "'\\") (write-char #\\ o))
                                 (write-char c o))))))
        (write-char #\' o))
      (shell-quote s)))

(defun variable-attributes (sh name)
  "The attribute letters `declare -p' would print for NAME, as ${name@a} gives
them: most-significant first, empty when the variable has none."
  (let ((base (nth-value 0 (split-subscript name))))
    (with-output-to-string (o)
      (let ((arr (var-array sh base)))
        (when arr
          (write-char (if (eq (sh-array-kind arr) :assoc) #\A #\a) o)))
      (when (nth-value 1 (gethash base (shell-int-vars sh))) (write-char #\i o))
      (when (readonly-p sh base) (write-char #\r o))
      (when (nth-value 2 (get-var sh base)) (write-char #\x o)))))

(defun transform-value (value op)
  "Apply one ${...@OP} transformation to a single VALUE."
  (case op
    ;; @K and @k differ from @Q only for arrays, which they render as key/value
    ;; pairs; for one value all three quote it.
    ((#\Q #\K #\k) (quote-for-reuse value))
    (#\U (string-upcase value))
    (#\L (string-downcase value))
    (#\u (if (plusp (length value))
              (concatenate 'string (string (char-upcase (char value 0)))
                           (subseq value 1))
              value))
    ;; @E reads the value as a $'...' body; @P expands prompt escapes, which
    ;; without any PS1 machinery leaves the value alone.
    (#\E (expand-dollar-quote-string value))
    (t value)))

(defun transform-expand (sh name op)
  "${name@OP}: a parameter transformation.

$@, $* and an array's [@]/[*] transform ELEMENT BY ELEMENT and are then joined,
so `${arr[@]@Q}' gives 'a' 'b c' rather than one quoted string holding both.
Every other name transforms its single value.

@a and @A are about the VARIABLE rather than its value -- its attribute
letters, and the assignment that would recreate it -- so they are answered
here and never reach TRANSFORM-VALUE."
  (unless (find op "QEPAKakuLU")
    (error "~A: bad substitution" name))
  (let ((base (nth-value 0 (split-subscript name))))
    (case op
      (#\a (return-from transform-expand (variable-attributes sh name)))
      (#\A (return-from transform-expand
              (if (nth-value 1 (get-var sh base))
                  (format nil "~A=~A" base
                          (quote-for-reuse (or (param-value sh name) "")))
                  "")))))
  (let ((elements (transform-elements sh name)))
    (if elements
        (format nil "~{~A~^ ~}"
                (mapcar (lambda (v) (transform-value v op)) elements))
        ;; An unset variable transforms to nothing at all: ${u@Q} is empty,
        ;; not the two characters '' that an empty-but-set variable gives.
        (if (and (not (special-param-p name))
                 (not (nth-value 1 (get-var sh (nth-value 0 (split-subscript name))))))
            ""
            (transform-value (or (param-value sh name) "") op)))))

(defun transform-elements (sh name)
  "The list of values a transformation should map over, or NIL for a scalar."
  (cond ((or (string= name "@") (string= name "*"))
         (coerce (shell-positional sh) 'list))
        (t (multiple-value-bind (base sub) (split-subscript name)
             (let ((arr (var-array sh base)))
               (and arr sub (member sub '("@" "*") :test #'string=)
                    (array-values arr)))))))

(defun expand-dollar-quote-string (s)
  "The $'...' reading of S: backslash escapes interpreted, everything else
literal. A closing quote is appended because EXPAND-DOLLAR-QUOTE scans for
one; a `'\'' already inside S is escaped and so cannot end the scan early."
  (values (expand-dollar-quote (concatenate 'string s "'") 0 (1+ (length s)))))

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
    (t (multiple-value-bind (base sub) (split-subscript name)
         (let ((arr (var-array sh base)))
           (cond
             ;; ${a[@]} / ${a[*]} joined; the field-splitting form is handled
             ;; earlier, in EXPAND-PASS, where separators can be emitted.
             ((and arr sub (string= sub "@"))
              (format nil "~{~A~^ ~}" (array-values arr)))
             ((and arr sub (string= sub "*"))
              (format nil (format nil "~~{~~A~~^~A~~}" (ifs-first sh))
                      (array-values arr)))
             ((and arr sub) (array-get arr (array-subscript sh arr sub)))
             ;; A subscript on a non-array: [0] is the scalar itself.
             (sub (and (member sub '("0" "@" "*") :test #'string=)
                       (nth-value 0 (get-var sh base))))
             (t (multiple-value-bind (val found) (get-var sh name)
                  (if found val (dynamic-var sh name))))))))))

(defun array-subscript (sh arr sub)
  "Evaluate a subscript: arithmetic for an indexed array, a word for assoc.
A negative index counts from the end, as in bash."
  (if (eq (sh-array-kind arr) :assoc)
      (xchars->string (expand-pass sh sub))
      (let ((i (eval-arith sh sub)))
        (if (minusp i)
            (+ (array-next-index arr) i)
            i))))

(defun dynamic-var (sh name)
  "bash's dynamic variables: $RANDOM and $SECONDS.

Only consulted when nothing has been assigned to the name. Assigning replaces
the dynamic behaviour, as in bash -- `RANDOM=5; echo $RANDOM' prints 5."
  (when (nth-value 1 (gethash name (shell-dynamic-off sh)))
    (return-from dynamic-var nil))
  (cond
    ((string= name "RANDOM") (princ-to-string (random 32768)))
    ((string= name "SECONDS")
     (princ-to-string (floor (- (get-universal-time) (shell-start-time sh)))))
    (t nil)))

(defun nonempty (v) (and v (plusp (length v))))

(defun expand-nested (sh word)
  "Expand a ${...} default/alternative word, KEEPING the per-character quoting.

Returning a flat string here was what made `${undef:-"2 3"}' split: once the
quotes are gone there is nothing left to say the space was protected."
  (expand-pass sh word))

(defun expand-nested-string (sh word)
  "As EXPAND-NESTED, flattened -- for diagnostics and for the value stored by
`${name:=word}', both of which need a string."
  (xchars->string (expand-pass sh word)))

;;; pattern removal helpers (shell patterns: * ? [..])
(defun remove-prefix (sh s pattern greedy)
  "Remove the shortest (or longest, if GREEDY) prefix of S matching PATTERN."
  (let ((pat (expand-nested-pattern sh pattern))
        (candidates (if greedy
                        (loop for e from (length s) downto 0 collect e)
                        (loop for e from 0 to (length s) collect e))))
    (dolist (end candidates s)
      (when (shell-pattern-match pat (subseq s 0 end))
        (return-from remove-prefix (subseq s end))))))

(defun remove-suffix (sh s pattern greedy)
  "Remove the shortest (or longest, if GREEDY) suffix of S matching PATTERN."
  (let ((pat (expand-nested-pattern sh pattern))
        (candidates (if greedy
                        (loop for st from 0 to (length s) collect st)
                        (loop for st from (length s) downto 0 collect st))))
    (dolist (start candidates s)
      (when (shell-pattern-match pat (subseq s start))
        (return-from remove-suffix (subseq s 0 start))))))

(defun map-case (s fn allp pattern)
  "bash ${x^} / ${x^^} / ${x,} / ${x,,}: apply FN to characters of S.

When ALLP, every character is mapped; otherwise only the first. PATTERN, if
given, restricts mapping to characters matching it -- `${v^^[aeiou]}' upcases
only vowels. A nil PATTERN matches everything."
  (let ((out (copy-seq s)) (done nil))
    (dotimes (i (length out) out)
      (unless (and done (not allp))
        (when (or (null pattern)
                  (shell-pattern-match pattern (string (char out i))))
          (setf (char out i) (funcall fn (char out i)))
          (setf done t))))))

(defun substitute-expand (sh value spec)
  "bash ${name/pat/rep}: replace the first match of PAT in VALUE with REP.

SPEC is everything after the first `/'. A leading `/' means replace every
match, `#' anchors the pattern to the start and `%' to the end. The
replacement is optional -- ${v/pat} deletes the match.

The pattern is matched longest-first at each position, which is what bash
does: `v=aaa; ${v/a*/X}' gives X, not Xaa."
  (let ((allp nil) (anchor nil) (i 0))
    (when (and (< i (length spec)) (char= (char spec i) #\/))
      (setf allp t) (incf i))
    (when (and (< i (length spec)) (member (char spec i) '(#\# #\%)))
      (setf anchor (char spec i)) (incf i))
    ;; Split on the first unescaped, unquoted `/' -- the rest is the
    ;; replacement. A `\/' inside the pattern is a literal slash.
    (let ((pat-end nil) (j i))
      (loop while (< j (length spec)) do
        (cond ((and (char= (char spec j) #\\) (< (1+ j) (length spec)))
               (incf j 2))
              ((char= (char spec j) #\/) (setf pat-end j) (return))
              (t (incf j))))
      (let* ((raw-pat (subseq spec i (or pat-end (length spec))))
             ;; The replacement is spliced into a string, so it needs the
             ;; flattened form -- unlike a :- operand, it is never a field of
             ;; its own and carries no quoting into the result.
             (rep (if pat-end
                      (expand-nested-string sh (subseq spec (1+ pat-end)))
                      ""))
             (pat (expand-nested-pattern sh raw-pat)))
        (when (string= raw-pat "") (return-from substitute-expand value))
        (substitute-matches value pat rep allp anchor)))))

(defun substitute-matches (s pat rep allp anchor)
  (let ((out (make-string-output-stream))
        (i 0) (n (length s)) (replaced nil))
    (loop while (<= i n) do
      (let ((end nil))
        ;; Longest match wins at this position, matching bash.
        (unless (and replaced (not allp))
          (when (or (null anchor)
                    (and (char= anchor #\#) (= i 0))
                    (char= anchor #\%))
            (loop for e from n downto i do
              (when (and (or (not (char= (or anchor #\Nul) #\%)) (= e n))
                         (shell-pattern-match pat (subseq s i e)))
                (setf end e) (return)))))
        (cond
          ;; An empty match must not loop forever: emit a character and move on.
          ((and end (> end i))
           (write-string rep out) (setf i end replaced t))
          ((and end (= end i) (not replaced))
           (write-string rep out) (setf replaced t)
           (when (< i n) (write-char (char s i) out))
           (incf i))
          (t (when (< i n) (write-char (char s i) out))
             (incf i)))))
    (get-output-stream-string out)))

(defun expand-nested-pattern (sh word)
  "Expand the pattern operand of ${var#pat} and friends, then render it as a
pattern string.

This used to return WORD untouched -- the comment claimed it expanded
parameters, but nothing did, so the pattern was matched as literal text.
`p=h; ${s#$p}' looked for the two characters `$p'. That is an infinite loop in
any script that walks a string a character at a time, which is how
gpgrt-config's substitute_vars works:

    __result=\"$__result$(printf %c \"$__string\")\"
    __string=\"${__string#$(printf %c \"$__string\")}\"   # never shrank

POSIX applies tilde, parameter, command and arithmetic expansion to the
operand but neither field splitting nor pathname expansion, so EXPAND-PASS is
right and EXPAND-WORD would not be. XCHARS->PATTERN, not XCHARS->STRING,
renders the result: metacharacters that came from an expansion stay live
(`p=\"*.\"; ${s#$p}' strips through the first dot) while ones that were quoted
in the source go inert (`${s#\"*\"}' matches a literal asterisk)."
  (xchars->pattern (expand-pass sh word)))

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

(defun expand-backquote (sh raw i n &key in-dquotes)
  "RAW[i] is `. Returns (values output next-index).

Inside backticks a backslash escapes ` \ and $, and -- when the backticks
themselves sit inside DOUBLE QUOTES -- also \" (POSIX 2.6.3). So
`echo \"x `echo \\\"hi\\\"`\"' prints `x hi': the escaped quotes become real
quotes for the inner command. The $() form does NOT do this, which is the whole
point of the distinction; virtualenv's bin/activate depends on it."
  (let ((j (1+ i)) (buf (make-string-output-stream))
        (escapable (if in-dquotes '(#\` #\\ #\$ #\") '(#\` #\\ #\$))))
    (loop
      (when (>= j n) (error "unterminated backquote"))
      (let ((c (char raw j)))
        (cond
          ((char= c #\`) (incf j) (return))
          ((and (char= c #\\) (< (1+ j) n)
                (member (char raw (1+ j)) escapable))
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
      (if (member (xchar-class xc) '(:field-sep :star-sep))
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

(defvar *glob-skipdots* t
  "Whether globbing skips `.' and `..'; mirrors shopt globskipdots, which bash
has ON by default.")

(defvar *glob-dotglob* nil
  "Bound by GLOB-FIELD from shopt dotglob; consulted deep in the walk.")

(defun glob-field (field &key dotglob (skipdots t))
  "Expand a field containing active glob metacharacters against the filesystem.
Returns a sorted list of matching pathnames, or NIL if none match.

DOTGLOB is bash's shopt of the same name: when set, a leading `*' or `?' also
matches names beginning with a dot."
  (let* ((*glob-dotglob* dotglob)
         (*glob-skipdots* skipdots)
         (pat (field->glob-pattern field))
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
          ;; `-' belongs here too: quoted, it must not form a range, which
          ;; is what `[C\\-D]' relies on.
          (if (and (member c '(#\* #\? #\[ #\] #\- #\\))
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
  "Match one path segment. Leading-dot files are not matched by a leading * or ?
unless shopt dotglob is set."
  (let ((real (unescape-glob pattern)))
    (when (and (not *glob-dotglob*)
               (plusp (length name)) (char= (char name 0) #\.)
               (not (and (plusp (length real)) (char= (char real 0) #\.))))
      (return-from glob-segment-match nil))
    ;; `.' and `..' are skipped unless shopt globskipdots is unset -- bash
    ;; enables it by default, so `.*' normally shows neither.
    (cond
      ((or (string= real ".") (string= real "..")) (string= real name))
      ((and (or (string= name ".") (string= name "..")) *glob-skipdots*) nil)
      (t (shell-pattern-match real name)))))

(defun unescape-glob (pattern)
  "Turn NUL sentinels into backslash escapes, which both PAT-MATCH and
MATCH-BRACKET already read as `the next character is literal'.

Wrapping the character in a bracket expression instead -- `[' became `[[]' --
works only OUTSIDE a bracket expression. Inside one it is nonsense: `[\[z]'
arrived as [ NUL [ z ] and came back out as `[[[]z]', so the escaped forms of
the very cases brackets exist to express could never match."
  (with-output-to-string (s)
    (let ((i 0) (n (length pattern)))
      (loop while (< i n) do
        (if (and (char= (char pattern i) #\Nul) (< (1+ i) n))
            (progn (write-char #\\ s)
                   (write-char (char pattern (1+ i)) s)
                   (incf i 2))
            (progn (write-char (char pattern i) s) (incf i)))))))

;;; ---------------------------------------------------------------------------
;;; Shell pattern matching (fnmatch-like: * ? [set])
;;; ---------------------------------------------------------------------------

(defvar *extglob* nil
  "Whether ?() *() +() @() !() are active, mirroring shopt extglob.

A global rather than a parameter because SHELL-PATTERN-MATCH is reached from
many places that have no SHELL handy (case, [[ ]], parameter expansion,
globbing), and there is one shell per process. The SHOPT builtin keeps it in
step. It is gated at all because without extglob `ab?(c)' means `ab?' followed
by a literal `(c)', so switching it on unconditionally would change the
meaning of ordinary patterns.")

(defun shell-pattern-match (pattern string)
  "Match STRING fully against a shell PATTERN (*, ?, [..], and extglob)."
  (pat-match pattern 0 (length pattern) string 0 (length string)))

(defun extglob-group-end (p pp pe)
  "PP is at the `(' of an extglob group. Return the index just past its `)'."
  (let ((depth 0) (i pp))
    (loop while (< i pe) do
      (let ((c (char p i)))
        (cond
          ((char= c #\\) (incf i 2))
          ((char= c #\() (incf depth) (incf i))
          ((char= c #\)) (decf depth) (incf i)
           (when (zerop depth) (return-from extglob-group-end i)))
          (t (incf i)))))
    nil))

(defun extglob-alternatives (p start end)
  "Split an extglob group body on top-level `|'."
  (let ((alts '()) (from start) (depth 0) (i start))
    (loop while (< i end) do
      (let ((c (char p i)))
        (cond
          ((char= c #\\) (incf i 2))
          ((char= c #\() (incf depth) (incf i))
          ((char= c #\)) (decf depth) (incf i))
          ((and (char= c #\|) (zerop depth))
           (push (subseq p from i) alts) (setf from (1+ i)) (incf i))
          (t (incf i)))))
    (push (subseq p from end) alts)
    (nreverse alts)))

(defun extglob-match (kind alts p pp pe s si sn)
  "Match an extglob group at SI, then the rest of the pattern from PP.

KIND is one of #\? #\* #\+ #\@ #\!. Each alternative is itself a pattern, so
matching is recursive; the continuation is `the rest of the outer pattern',
which is what lets the group give back characters on failure."
  (labels ((rest-matches (k) (pat-match p pp pe s k sn))
           ;; How far can one alternative reach from position K?
           (alt-ends (k)
             (let ((ends '()))
               (dolist (a alts ends)
                 (loop for e from k to sn
                       when (pat-match a 0 (length a) s k e)
                         do (push e ends)))))
           (repeat (k seen)
             ;; Zero or more, longest-first, refusing empty progress.
             (or (rest-matches k)
                 (some (lambda (e)
                         (and (> e k) (not (member e seen))
                              (repeat e (cons e seen))))
                       (sort (alt-ends k) #'>)))))
    (ecase kind
      (#\@ (some #'rest-matches (alt-ends si)))
      (#\? (or (rest-matches si) (some #'rest-matches (alt-ends si))))
      (#\* (repeat si '()))
      (#\+ (some (lambda (e) (and (> e si) (repeat e (list e))))
                 (sort (alt-ends si) #'>)))
      ;; !(...) matches any run that no alternative matches in full.
      (#\! (loop for e from sn downto si
                 when (and (notany (lambda (a)
                                     (pat-match a 0 (length a) s si e))
                                   alts)
                           (rest-matches e))
                   do (return t))))))

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
        ;; extglob, BEFORE the plain * and ? branches or those would
        ;; consume the quantifier character first: ?(a|b) *(a|b) +(a|b)
        ;; @(a|b) !(a|b) ?(a|b) *(a|b) +(a|b) @(a|b) !(a|b)
        ((and *extglob* (member pc '(#\? #\* #\+ #\@ #\!))
              (< (1+ pp) pe) (char= (char p (1+ pp)) #\()
              (extglob-group-end p (1+ pp) pe))
         (let* ((close (extglob-group-end p (1+ pp) pe))
                (alts (extglob-alternatives p (+ pp 2) (1- close))))
           (return (extglob-match pc alts p close pe s si sn))))
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

;;; ---------------------------------------------------------------------------
;;; Brace expansion (bash; not POSIX)
;;;
;;; Runs BEFORE every other expansion and operates on the raw word text, which
;;; is why it lives outside EXPAND-PASS: `{a,b}' has to become two words before
;;; anything looks at parameters. Quoting suppresses it, so the scan has to
;;; skip quoted regions rather than work on the quote-removed string.
;;; ---------------------------------------------------------------------------

(defun brace-skip-quoted (s i n)
  "If S[i] opens a quoted or substituted region, return the index just past it,
else NIL."
  (let ((c (char s i)))
    (case c
      (#\\ (min n (+ i 2)))
      (#\' (let ((j (position #\' s :start (1+ i))))
             (if j (1+ j) n)))
      (#\" (let ((j (1+ i)))
             (loop while (< j n) do
               (cond ((char= (char s j) #\\) (incf j 2))
                     ((char= (char s j) #\") (return (1+ j)))
                     (t (incf j)))
                   finally (return n))))
      (t nil))))

(defun brace-find-group (s)
  "Find the leftmost unquoted `{' with a matching unquoted `}'.
Returns (values open-index close-index) or NIL."
  (let ((n (length s)) (i 0))
    (loop while (< i n) do
      (let ((skip (brace-skip-quoted s i n)))
        (cond
          (skip (setf i skip))
          ((char= (char s i) #\{)
           ;; find the matching close, tracking nesting and quotes
           (let ((depth 0) (j i))
             (loop while (< j n) do
               (let ((sk (brace-skip-quoted s j n)))
                 (cond
                   (sk (setf j sk))
                   ((char= (char s j) #\{) (incf depth) (incf j))
                   ((char= (char s j) #\})
                    (decf depth)
                    (when (zerop depth)
                      (return-from brace-find-group (values i j)))
                    (incf j))
                   (t (incf j)))))
             ;; unmatched `{': it is literal, keep looking after it
             (incf i)))
          (t (incf i)))))
    nil))

(defun brace-split-commas (s)
  "Split S on top-level unquoted commas. Returns NIL if there are none."
  (let ((parts '()) (start 0) (depth 0) (n (length s)) (i 0) (found nil))
    (loop while (< i n) do
      (let ((skip (brace-skip-quoted s i n)))
        (cond
          (skip (setf i skip))
          ((char= (char s i) #\{) (incf depth) (incf i))
          ((char= (char s i) #\}) (decf depth) (incf i))
          ((and (char= (char s i) #\,) (zerop depth))
           (push (subseq s start i) parts)
           (setf start (1+ i) found t)
           (incf i))
          (t (incf i)))))
    (when found
      (push (subseq s start) parts)
      (nreverse parts))))

(defun brace-range (s)
  "Expand `{1..5}' / `{a..e}' / `{0..10..5}'. Returns a list, or NIL if S is
not a range."
  (let ((parts '()) (start 0))
    ;; split on ".." at top level (ranges never nest)
    (let ((i 0) (n (length s)))
      (loop while (< i n) do
        (if (and (< (1+ i) n) (char= (char s i) #\.) (char= (char s (1+ i)) #\.))
            (progn (push (subseq s start i) parts) (setf start (+ i 2)) (incf i 2))
            (incf i)))
      (push (subseq s start) parts))
    (setf parts (nreverse parts))
    (unless (member (length parts) '(2 3)) (return-from brace-range nil))
    (destructuring-bind (from to &optional step) parts
      (let ((istep (and step (ignore-errors (abs (parse-integer step))))))
        (when (and step (or (null istep) (zerop istep)))
          (return-from brace-range nil))
        (cond
          ;; numeric range
          ((and (ignore-errors (parse-integer from))
                (ignore-errors (parse-integer to)))
           (let* ((a (parse-integer from)) (b (parse-integer to))
                  (d (or istep 1))
                  (out '()))
             (if (<= a b)
                 (loop for v from a to b by d do (push (princ-to-string v) out))
                 (loop for v downfrom a to b by d do (push (princ-to-string v) out)))
             (nreverse out)))
          ;; single-character range
          ((and (= 1 (length from)) (= 1 (length to)))
           (let* ((a (char-code (char from 0))) (b (char-code (char to 0)))
                  (d (or istep 1)) (out '()))
             (if (<= a b)
                 (loop for v from a to b by d do (push (string (code-char v)) out))
                 (loop for v downfrom a to b by d do (push (string (code-char v)) out)))
             (nreverse out)))
          (t nil))))))

(defun brace-expand (raw)
  "Expand brace groups in RAW, returning a list of words (possibly just RAW).

`{a,b}' needs at least one top-level comma, or a `..' range, to expand at all;
`{a}' and `{}' stay literal, as in bash."
  (multiple-value-bind (open close) (brace-find-group raw)
    (if (null open)
        (list raw)
        (let* ((pre (subseq raw 0 open))
               (inside (subseq raw (1+ open) close))
               (post (subseq raw (1+ close)))
               (alts (or (brace-split-commas inside) (brace-range inside))))
          (if (null alts)
              ;; Not a real brace group. Keep the `{' literal and carry on
              ;; past it, or we would rescan it forever.
              (mapcar (lambda (tail) (concatenate 'string pre "{" tail))
                      (brace-expand (concatenate 'string inside "}" post)))
              (let ((out '()))
                (dolist (a alts)
                  (dolist (w (brace-expand (concatenate 'string pre a post)))
                    (push w out)))
                (nreverse out)))))))
