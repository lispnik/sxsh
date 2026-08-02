;;;; shell/arith.lisp --- POSIX arithmetic expansion $(( ... )) (2.6.4).
;;;;
;;;; Signed integer arithmetic with C-like operators and precedence.
;;;; Supports: + - * / % , unary + - ! ~, comparisons, && || , bitwise,
;;;; assignment (= += -= *= /= %= etc.), ternary ?:, and variable references
;;;; (a bare name is looked up; parameter expansion already ran on the text).

(in-package #:sxsh-shell)

(defun nonzero (divisor)
  "Guard a divisor. Letting the Lisp DIVISION-BY-ZERO escape printed the raw
condition -- \"arithmetic error DIVISION-BY-ZERO signalled / Operation was
(/ 1 0).\" -- where a shell should report a diagnostic of its own."
  (if (zerop divisor)
      (error "division by 0")
      divisor))

(defstruct atok kind value start)       ; kind: :num :name :op ; value

(defvar *arith-live* t
  "NIL while an expression is being parsed but must NOT take effect.

`||' and `&&' short-circuit and the untaken ternary branch is not evaluated,
but the tokens still have to be consumed to find the end of the expression.
Parsing with side effects suppressed is how that is done: `(( 1 || (x = 22) ))'
must leave x alone, and evaluating the right operand to discard it does not.")

(defun base-digit-value (ch base)
  "Value of CH as a digit in BASE, or NIL.

bash's extended alphabet runs 0-9, a-z = 10..35, A-Z = 36..61, @ = 62, _ = 63.
Case only carries information above base 36; at or below it the two cases mean
the same digit, so 36#A and 36#a are both 10."
  (let ((v (cond ((digit-char-p ch) (digit-char-p ch))
                 ((char<= #\a ch #\z) (+ 10 (- (char-code ch) (char-code #\a))))
                 ((char<= #\A ch #\Z)
                  (if (<= base 36)
                      (+ 10 (- (char-code ch) (char-code #\A)))
                      (+ 36 (- (char-code ch) (char-code #\A)))))
                 ((char= ch #\@) 62)
                 ((char= ch #\_) 63)
                 (t nil))))
    (and v (< v base) v)))

(defun arith-tokenize (s)
  (let ((toks '()) (i 0) (n (length s)))
    (flet ((push-op (str start)
             (push (make-atok :kind :op :value str :start start) toks)))
      (loop while (< i n) do
        (let ((c (char s i))
              (tok-start i))
          (cond
            ((member c '(#\Space #\Tab #\Newline)) (incf i))
            ((digit-char-p c)
             (let ((start i) (value nil))
               ;; Read the leading decimal run first: it is either the whole
               ;; constant, or the BASE of a `base#digits' constant.
               (loop while (and (< i n) (digit-char-p (char s i))) do (incf i))
               (cond
                 ;; BASE#DIGITS
                 ((and (< i n) (char= (char s i) #\#))
                  (let ((base (parse-integer s :start start :end i)))
                    ;; `02#0110' is an error, not base 2: a leading zero would
                    ;; otherwise read as octal and mean something else.
                    (when (char= (char s start) #\0)
                      (error "invalid arithmetic base: ~A"
                             (subseq s start i)))
                    (unless (<= 2 base 64)
                      (error "invalid arithmetic base: ~D" base))
                    (incf i)            ; past the #
                    (let ((dstart i) (v 0))
                      (loop while (and (< i n)
                                       (base-digit-value (char s i) base))
                            do (setf v (+ (* v base)
                                          (base-digit-value (char s i) base)))
                               (incf i))
                      (when (= i dstart)
                        (error "value too great for base: ~A" (subseq s start)))
                      (setf value v))))
                 ;; 0x / 0X hex
                 ((and (char= c #\0) (> i start) (< i n)
                       (member (char s i) '(#\x #\X))
                       (= (- i start) 1))
                  (incf i)
                  (let ((hstart i))
                    (loop while (and (< i n) (digit-char-p (char s i) 16))
                          do (incf i))
                    (when (= i hstart) (error "invalid hex constant"))
                    (setf value (parse-integer s :start hstart :end i :radix 16))))
                 ;; leading 0 is octal -- and every digit must BE octal, so
                 ;; `09' is an error rather than nine.
                 ((and (char= c #\0) (> (- i start) 1))
                  (setf value (or (ignore-errors
                                    (parse-integer s :start start :end i :radix 8))
                                  (error "invalid octal constant: ~A"
                                         (subseq s start i)))))
                 (t (setf value (parse-integer s :start start :end i))))
               ;; A constant may not run straight into a name: `0x1X' and `09'
               ;; are errors in bash, not a number followed by a variable.
               (when (and (< i n)
                          (let ((ch (char s i)))
                            (or (alphanumericp ch) (char= ch #\_))))
                 (error "invalid arithmetic constant: ~A"
                        (subseq s start (min n (1+ i)))))
               (push (make-atok :kind :num :value value :start tok-start) toks)))
            ((or (alpha-char-p c) (char= c #\_))
             (let ((start i))
               (loop while (and (< i n)
                                (let ((ch (char s i)))
                                  (or (alphanumericp ch) (char= ch #\_))))
                     do (incf i))
               (push (make-atok :kind :name :value (subseq s start i)
                                :start tok-start)
                     toks)))
            (t
             ;; multi-char operators, longest first
             (let ((two (and (< (1+ i) n) (subseq s i (+ i 2))))
                   (three (and (< (+ i 2) n) (subseq s i (+ i 3)))))
               (cond
                 ((and three (member three '("<<=" ">>=") :test #'string=))
                  (push-op three tok-start) (incf i 3))
                 ((and two (member two '("==" "!=" "<=" ">=" "&&" "||" "<<" ">>"
                                         "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^="
                                         "**" "++" "--")
                                   :test #'string=))
                  (push-op two tok-start) (incf i 2))
                 ((char= c #\.)
                  ;; POSIX arithmetic is integer-only; `1.5' is a syntax error
                  ;; rather than something to truncate silently.
                  (error "invalid arithmetic operator: ~A" (subseq s i)))
                 (t (push-op (string c) tok-start) (incf i)))))))))
    (nreverse toks)))

(defun eval-arith (sh expr)
  "Evaluate arithmetic EXPR (a string) in shell SH. Returns an integer."
  (let* ((toks (coerce (arith-tokenize expr) 'vector))
         (pos 0))
    (labels ((peek () (when (< pos (length toks)) (aref toks pos)))
             (next () (prog1 (aref toks pos) (incf pos)))
             (op? (str) (let ((tk (peek)))
                          (and tk (eq (atok-kind tk) :op)
                               (string= (atok-value tk) str))))
             (eat (str) (if (op? str) (progn (next) t) nil))
             (num (text)
               ;; A variable's value is itself an arithmetic expression, which
               ;; is what makes `e=1+2; $(( e + 3 ))' six.
               (if (and text (plusp (length text)))
                   (or (ignore-errors (eval-arith sh text)) 0)
                   0))
             ;; An LVALUE is (NAME INDEX RAW) -- INDEX nil for a plain scalar.
             (lv-read (lv)
               (destructuring-bind (name index raw) lv
                 (declare (ignore raw))
                 (cond
                   ((null index)
                    (multiple-value-bind (v found) (get-var sh name)
                      (when (and (not found) (opt sh :nounset))
                        (error 'shell-unset-var :name name))
                      (num v)))
                   (t
                    (let ((arr (var-array sh name)))
                      (cond
                        ;; An out-of-range element of an array that DOES exist
                        ;; is 0, but a subscript of a name that does not exist
                        ;; at all is still an unset-variable error under -u.
                        (arr (num (array-get arr index)))
                        (t
                         (multiple-value-bind (v found) (get-var sh name)
                           (when (and (not found) (opt sh :nounset))
                             (error 'shell-unset-var :name name))
                           ;; `s=42; $(( s[0] ))' is 42 and `$(( s[1] ))' is 0:
                           ;; a scalar behaves as a one-element array.
                           (if (equal index 0) (num v) 0)))))))))
             (lv-write (lv value)
               (when *arith-live*
                 (destructuring-bind (name index raw) lv
                   (declare (ignore raw))
                   (if (null index)
                       (set-var sh name (princ-to-string value))
                       (progn
                         (when (readonly-p sh (resolve-nameref sh name))
                           (error 'readonly-violation :name name))
                         (let ((arr (or (var-array sh name)
                                        (let ((new (make-sh-array :indexed)))
                                          (set-var sh name new)
                                          new))))
                           (array-set arr index (princ-to-string value)))))))
               value)
             ;; NAME or NAME[expr]; a second subscript is a syntax error.
             (p-lvalue ()
               (let* ((name (atok-value (next)))
                      (index nil) (raw nil))
                 (when (op? "[")
                   (next)
                   (let ((start (and (peek) (atok-start (peek)))))
                     (setf index (p-comma))
                     (let ((close (peek)))
                       (unless (and close (eq (atok-kind close) :op)
                                    (string= (atok-value close) "]"))
                         (error "~A: arithmetic syntax error" name))
                       (when start
                         (setf raw (subseq expr start (atok-start close))))
                       (next)))
                   (when (op? "[")
                     (error "~A: arithmetic syntax error" name)))
                 ;; An associative array is keyed by the subscript TEXT, not by
                 ;; its arithmetic value.
                 (let ((arr (var-array sh name)))
                   (when (and raw arr (eq (sh-array-kind arr) :assoc))
                     (setf index raw)))
                 (list name index raw)))
             ;; ++lvalue / --lvalue : update, yield the NEW value
             (p-preincr (delta)
               (let ((tk (peek)))
                 (unless (and tk (eq (atok-kind tk) :name))
                   (error "arithmetic: operand expected"))
                 (let ((lv (p-lvalue)))
                   (lv-write lv (+ (lv-read lv) delta)))))
             ;; Is the token run starting at POS an lvalue followed by an
             ;; assignment operator? Needs a scan because the subscript may be
             ;; any expression, brackets included.
             (assign-ahead-p ()
               (let ((tk (peek)))
                 (and tk (eq (atok-kind tk) :name)
                      (let ((k (1+ pos)))
                        (when (and (< k (length toks))
                                   (eq (atok-kind (aref toks k)) :op)
                                   (string= (atok-value (aref toks k)) "["))
                          (let ((depth 0))
                            (loop while (< k (length toks))
                                  do (let ((v (and (eq (atok-kind (aref toks k)) :op)
                                                   (atok-value (aref toks k)))))
                                       (cond ((equal v "[") (incf depth))
                                             ((equal v "]") (decf depth)))
                                       (incf k)
                                       (when (zerop depth) (return))))))
                        (and (< k (length toks))
                             (eq (atok-kind (aref toks k)) :op)
                             (member (atok-value (aref toks k))
                                     '("=" "+=" "-=" "*=" "/=" "%="
                                       "&=" "|=" "^=" "<<=" ">>=")
                                     :test #'string=))))))
             ;; expression : assignment {, assignment}
             (p-comma ()
               (let ((v (p-assign)))
                 (loop while (eat ",") do (setf v (p-assign)))
                 v))
             (p-assign ()
               (if (assign-ahead-p)
                   (let* ((lv (p-lvalue))
                          (aop (atok-value (next)))
                          (rhs (p-assign))
                          (cur (if (string= aop "=") 0 (lv-read lv)))
                          (newv (cond ((string= aop "=") rhs)
                                      ((string= aop "+=") (+ cur rhs))
                                      ((string= aop "-=") (- cur rhs))
                                      ((string= aop "*=") (* cur rhs))
                                      ((string= aop "/=") (truncate cur (nonzero rhs)))
                                      ((string= aop "%=") (rem cur (nonzero rhs)))
                                      ((string= aop "&=") (logand cur rhs))
                                      ((string= aop "|=") (logior cur rhs))
                                      ((string= aop "^=") (logxor cur rhs))
                                      ((string= aop "<<=") (ash cur rhs))
                                      ((string= aop ">>=") (ash cur (- rhs))))))
                     (lv-write lv newv))
                   (p-ternary)))
             ;; Only the taken branch may take effect; the other is parsed
             ;; with *ARITH-LIVE* off purely to step over its tokens.
             (p-ternary ()
               (let ((c (p-logor)))
                 (if (eat "?")
                     (let ((then (if (/= c 0)
                                     (p-assign)
                                     (let ((*arith-live* nil)) (p-assign)))))
                       (eat ":")
                       (let ((else (if (= c 0)
                                       (p-assign)
                                       (let ((*arith-live* nil)) (p-assign)))))
                         (if (/= c 0) then else)))
                     c)))
             (p-logor ()
               (let ((v (p-logand)))
                 (loop while (eat "||") do
                   ;; Already true: the right operand is not evaluated, so
                   ;; `(( 1 || (x = 22) ))' must not touch x.
                   (let ((r (if (/= v 0)
                                (progn (let ((*arith-live* nil)) (p-logand)) 0)
                                (p-logand))))
                     (setf v (if (or (/= v 0) (/= r 0)) 1 0))))
                 v))
             (p-logand ()
               (let ((v (p-bitor)))
                 (loop while (eat "&&") do
                   (let ((r (if (= v 0)
                                (progn (let ((*arith-live* nil)) (p-bitor)) 0)
                                (p-bitor))))
                     (setf v (if (and (/= v 0) (/= r 0)) 1 0))))
                 v))
             (p-bitor () (let ((v (p-bitxor)))
                           (loop while (op? "|") do (next) (setf v (logior v (p-bitxor))))
                           v))
             (p-bitxor () (let ((v (p-bitand)))
                            (loop while (op? "^") do (next) (setf v (logxor v (p-bitand))))
                            v))
             (p-bitand () (let ((v (p-eq)))
                            (loop while (op? "&") do (next) (setf v (logand v (p-eq))))
                            v))
             (p-eq ()
               (let ((v (p-rel)))
                 (loop
                   (cond ((eat "==") (setf v (if (= v (p-rel)) 1 0)))
                         ((eat "!=") (setf v (if (/= v (p-rel)) 1 0)))
                         (t (return))))
                 v))
             (p-rel ()
               (let ((v (p-shift)))
                 (loop
                   (cond ((eat "<=") (setf v (if (<= v (p-shift)) 1 0)))
                         ((eat ">=") (setf v (if (>= v (p-shift)) 1 0)))
                         ((op? "<") (next) (setf v (if (< v (p-shift)) 1 0)))
                         ((op? ">") (next) (setf v (if (> v (p-shift)) 1 0)))
                         (t (return))))
                 v))
             (p-shift ()
               (let ((v (p-add)))
                 (loop
                   (cond ((eat "<<") (setf v (ash v (p-add))))
                         ((eat ">>") (setf v (ash v (- (p-add)))))
                         (t (return))))
                 v))
             (p-add ()
               (let ((v (p-mul)))
                 (loop
                   (cond ((op? "+") (next) (setf v (+ v (p-mul))))
                         ((op? "-") (next) (setf v (- v (p-mul))))
                         (t (return))))
                 v))
             (p-mul ()
               (let ((v (p-power)))
                 (loop
                   (cond ((op? "*") (next) (setf v (* v (p-power))))
                         ((op? "/") (next) (setf v (truncate v (nonzero (p-power)))))
                         ((op? "%") (next) (setf v (rem v (nonzero (p-power)))))
                         (t (return))))
                 v))
             (p-power ()
               (let ((base (p-unary)))
                 (if (op? "**")
                     (progn (next)
                            (let ((e (p-power)))
                              (when (minusp e)
                                ;; (expt 2 -1) is the rational 1/2 in Lisp, and
                                ;; printing that produced the literal "1/2".
                                (error "exponent less than 0"))
                              (expt base e)))  ; right-associative
                     base)))
             (p-unary ()
               (cond ((eat "++") (p-preincr 1))
                     ((eat "--") (p-preincr -1))
                     ((eat "+") (p-unary))
                     ((eat "-") (- (p-unary)))
                     ((eat "!") (if (= 0 (p-unary)) 1 0))
                     ((eat "~") (lognot (p-unary)))
                     (t (p-primary))))
             (p-primary ()
               (let ((tk (peek)))
                 (cond
                   ((null tk) 0)
                   ((op? "(") (next) (prog1 (p-comma) (eat ")")))
                   ((eq (atok-kind tk) :num) (atok-value (next)))
                   ((eq (atok-kind tk) :name)
                    (let ((lv (p-lvalue)))
                      (cond
                        ;; postfix: yields the value from before the update
                        ((eat "++") (let ((old (lv-read lv)))
                                      (lv-write lv (1+ old)) old))
                        ((eat "--") (let ((old (lv-read lv)))
                                      (lv-write lv (1- old)) old))
                        (t (lv-read lv)))))
                   (t (next) 0)))))
      (if (zerop (length toks)) 0 (p-comma)))))
