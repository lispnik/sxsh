;;;; shell/arith.lisp --- POSIX arithmetic expansion $(( ... )) (2.6.4).
;;;;
;;;; Signed integer arithmetic with C-like operators and precedence.
;;;; Supports: + - * / % , unary + - ! ~, comparisons, && || , bitwise,
;;;; assignment (= += -= *= /= %= etc.), ternary ?:, and variable references
;;;; (a bare name is looked up; parameter expansion already ran on the text).

(in-package #:posh-shell)

(defun nonzero (divisor)
  "Guard a divisor. Letting the Lisp DIVISION-BY-ZERO escape printed the raw
condition -- \"arithmetic error DIVISION-BY-ZERO signalled / Operation was
(/ 1 0).\" -- where a shell should report a diagnostic of its own."
  (if (zerop divisor)
      (error "division by 0")
      divisor))

(defstruct atok kind value)             ; kind: :num :name :op ; value

(defun arith-tokenize (s)
  (let ((toks '()) (i 0) (n (length s)))
    (flet ((push-op (str) (push (make-atok :kind :op :value str) toks)))
      (loop while (< i n) do
        (let ((c (char s i)))
          (cond
            ((member c '(#\Space #\Tab #\Newline)) (incf i))
            ((digit-char-p c)
             (let ((start i) (radix 10))
               (cond
                 ((and (char= c #\0) (< (1+ i) n)
                       (member (char s (1+ i)) '(#\x #\X)))
                  (setf radix 16) (incf i 2) (setf start i)
                  (loop while (and (< i n) (digit-char-p (char s i) 16)) do (incf i)))
                 ((char= c #\0)
                  (setf radix 8)
                  (loop while (and (< i n) (digit-char-p (char s i) 8)) do (incf i)))
                 (t (loop while (and (< i n) (digit-char-p (char s i))) do (incf i))))
               (push (make-atok :kind :num
                                :value (if (= start i) 0
                                           (parse-integer s :start start :end i
                                                            :radix radix)))
                     toks)))
            ((or (alpha-char-p c) (char= c #\_))
             (let ((start i))
               (loop while (and (< i n)
                                (let ((ch (char s i)))
                                  (or (alphanumericp ch) (char= ch #\_))))
                     do (incf i))
               (push (make-atok :kind :name :value (subseq s start i)) toks)))
            (t
             ;; multi-char operators, longest first
             (let ((two (and (< (1+ i) n) (subseq s i (+ i 2))))
                   (three (and (< (+ i 2) n) (subseq s i (+ i 3)))))
               (cond
                 ((and three (member three '("<<=" ">>=") :test #'string=))
                  (push-op three) (incf i 3))
                 ((and two (member two '("==" "!=" "<=" ">=" "&&" "||" "<<" ">>"
                                         "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^="
                                         "**" "++" "--")
                                   :test #'string=))
                  (push-op two) (incf i 2))
                 ((char= c #\.)
                  ;; POSIX arithmetic is integer-only; `1.5' is a syntax error
                  ;; rather than something to truncate silently.
                  (error "invalid arithmetic operator: ~A" (subseq s i)))
                 (t (push-op (string c)) (incf i)))))))))
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
             (val (name)
               (multiple-value-bind (v found) (get-var sh name)
                 (when (and (not found) (opt sh :nounset))
                   (error 'shell-unset-var :name name))
                 (if (and v (plusp (length v)))
                     (or (ignore-errors (eval-arith sh v)) 0)
                     0)))
             (store (name value) (set-var sh name (princ-to-string value)) value)
             ;; ++name / --name : update, yield the NEW value
             (p-preincr (delta)
               (let ((tk (peek)))
                 (unless (and tk (eq (atok-kind tk) :name))
                   (error "arithmetic: operand expected"))
                 (let ((name (atok-value (next))))
                   (store name (+ (val name) delta)))))
             ;; expression : assignment {, assignment}
             (p-comma ()
               (let ((v (p-assign)))
                 (loop while (eat ",") do (setf v (p-assign)))
                 v))
             (p-assign ()
               ;; lvalue detection: NAME assign-op ...
               (let ((tk (peek)))
                 (if (and tk (eq (atok-kind tk) :name)
                          (< (1+ pos) (length toks))
                          (let ((nx (aref toks (1+ pos))))
                            (and (eq (atok-kind nx) :op)
                                 (member (atok-value nx)
                                         '("=" "+=" "-=" "*=" "/=" "%="
                                           "&=" "|=" "^=" "<<=" ">>=")
                                         :test #'string=))))
                     (let* ((name (atok-value (next)))
                            (aop (atok-value (next)))
                            (rhs (p-assign))
                            (cur (val name))
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
                       (set-var sh name (princ-to-string newv))
                       newv)
                     (p-ternary))))
             (p-ternary ()
               (let ((c (p-logor)))
                 (if (eat "?")
                     (let ((then (p-assign)))
                       (eat ":")
                       (let ((else (p-assign)))
                         (if (/= c 0) then else)))
                     c)))
             (p-logor ()
               (let ((v (p-logand)))
                 (loop while (eat "||") do
                   (let ((r (p-logand))) (setf v (if (or (/= v 0) (/= r 0)) 1 0))))
                 v))
             (p-logand ()
               (let ((v (p-bitor)))
                 (loop while (eat "&&") do
                   (let ((r (p-bitor))) (setf v (if (and (/= v 0) (/= r 0)) 1 0))))
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
                    (let ((name (atok-value (next))))
                      (cond
                        ;; postfix: yields the value from before the update
                        ((eat "++") (let ((old (val name)))
                                      (store name (1+ old)) old))
                        ((eat "--") (let ((old (val name)))
                                      (store name (1- old)) old))
                        (t (val name)))))
                   (t (next) 0)))))
      (if (zerop (length toks)) 0 (p-comma)))))
