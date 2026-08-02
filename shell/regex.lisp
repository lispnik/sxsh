;;;; shell/regex.lisp --- POSIX extended regular expressions, for `[[ =~ ]]'.
;;;;
;;;; A small backtracking matcher rather than a dependency. The project has no
;;;; external libraries -- only sb-posix, which ships with SBCL -- and pulling
;;;; in a full regex engine to support one operator would change the build
;;;; story for CI and for anyone cloning. Capture groups have to be tracked
;;;; regardless, because bash exposes them as $BASH_REMATCH.
;;;;
;;;; Supported: literals, `.', `[...]' with ranges/negation/[:class:], `*',
;;;; `+', `?', `{n}' `{n,}' `{n,m}', alternation, groups (capturing), and the
;;;; `^' / `$' anchors. Backreferences are NOT supported; they are not part of
;;;; POSIX ERE.

(in-package #:sxsh-shell)

;;; ---------------------------------------------------------------------------
;;; Parsing to a tree
;;;
;;;   alt    := concat ('|' concat)*
;;;   concat := repeat*
;;;   repeat := atom ('*' | '+' | '?' | '{n,m}')*
;;;   atom   := '(' alt ')' | '[' set ']' | '.' | '^' | '$' | literal
;;; ---------------------------------------------------------------------------

(defstruct (rx (:constructor make-rx (kind &key children ch set negated
                                                min max group)))
  kind                                  ; :alt :cat :rep :char :any :set
                                        ; :bol :eol :group
  children ch set negated min max group)

(defun rx-parse (pattern)
  "Parse PATTERN, returning (values tree group-count)."
  (let ((i 0) (n (length pattern)) (groups 0))
    (labels
        ((peek () (and (< i n) (char pattern i)))
         (eat () (prog1 (char pattern i) (incf i)))
         (parse-alt ()
           (let ((branches (list (parse-cat))))
             (loop while (and (< i n) (char= (peek) #\|))
                   do (eat) (push (parse-cat) branches))
             (if (= 1 (length branches))
                 (first branches)
                 (make-rx :alt :children (nreverse branches)))))
         (parse-cat ()
           (let ((items '()))
             (loop while (and (< i n)
                              (not (member (peek) '(#\| #\)))))
                   do (push (parse-repeat) items))
             (make-rx :cat :children (nreverse items))))
         (parse-repeat ()
           (let ((atom (parse-atom)))
             (loop
               (let ((c (peek)))
                 (cond
                   ((null c) (return atom))
                   ((char= c #\*) (eat) (setf atom (make-rx :rep :children (list atom)
                                                            :min 0 :max nil)))
                   ((char= c #\+) (eat) (setf atom (make-rx :rep :children (list atom)
                                                            :min 1 :max nil)))
                   ((char= c #\?) (eat) (setf atom (make-rx :rep :children (list atom)
                                                            :min 0 :max 1)))
                   ((char= c #\{)
                    (multiple-value-bind (lo hi next) (parse-bound i)
                      (if lo
                          (progn (setf i next)
                                 (setf atom (make-rx :rep :children (list atom)
                                                     :min lo :max hi)))
                          (return atom))))
                   (t (return atom)))))))
         (parse-bound (start)
           ;; `{n}' `{n,}' `{n,m}'. A `{' that is not a valid bound is a
           ;; literal, which is why this reports failure instead of erroring.
           (let ((j (1+ start)) (lo 0) (hi nil) (any nil))
             (loop while (and (< j n) (digit-char-p (char pattern j)))
                   do (setf lo (+ (* 10 lo) (digit-char-p (char pattern j))) any t)
                      (incf j))
             (unless any (return-from parse-bound (values nil nil nil)))
             (cond
               ((and (< j n) (char= (char pattern j) #\}))
                (values lo lo (1+ j)))
               ((and (< j n) (char= (char pattern j) #\,))
                (incf j)
                (let ((h 0) (hany nil))
                  (loop while (and (< j n) (digit-char-p (char pattern j)))
                        do (setf h (+ (* 10 h) (digit-char-p (char pattern j))) hany t)
                           (incf j))
                  (if (and (< j n) (char= (char pattern j) #\}))
                      (values lo (and hany h) (1+ j))
                      (values nil nil nil))))
               (t (values nil nil nil)))))
         (parse-atom ()
           (let ((c (eat)))
             (case c
               (#\( (incf groups)
                    (let ((idx groups))
                      (let ((inner (parse-alt)))
                        (when (and (< i n) (char= (peek) #\))) (eat))
                        (make-rx :group :children (list inner) :group idx))))
               (#\[ (parse-set))
               (#\. (make-rx :any))
               (#\^ (make-rx :bol))
               (#\$ (make-rx :eol))
               (#\\ (if (< i n)
                        (make-rx :char :ch (eat))
                        (make-rx :char :ch #\\)))
               (t (make-rx :char :ch c)))))
         (parse-set ()
           (let ((negated nil) (items '()))
             (when (and (< i n) (char= (peek) #\^)) (eat) (setf negated t))
             ;; A `]' first is a literal, per POSIX.
             (when (and (< i n) (char= (peek) #\])) (eat) (push #\] items))
             (loop while (and (< i n) (char/= (peek) #\]))
                   do (let ((c (eat)))
                        (cond
                          ;; [:alpha:] and friends
                          ((and (char= c #\[) (< i n) (char= (peek) #\:))
                           (let ((close (search ":]" pattern :start2 i)))
                             (if close
                                 (progn (push (intern (string-upcase
                                                       (subseq pattern (1+ i) close))
                                                      :keyword)
                                              items)
                                        (setf i (+ close 2)))
                                 (push c items))))
                          ;; range a-z, unless the `-' is last
                          ((and (< (1+ i) n) (char= (peek) #\-)
                                (char/= (char pattern (1+ i)) #\]))
                           (eat)
                           (push (cons c (eat)) items))
                          ((char= c #\\)
                           (when (< i n) (push (eat) items)))
                          (t (push c items)))))
             (when (and (< i n) (char= (peek) #\])) (eat))
             (make-rx :set :set (nreverse items) :negated negated))))
      (let ((tree (parse-alt)))
        (values tree groups)))))

(defun rx-class-match (class c)
  (case class
    (:alpha (alpha-char-p c))
    (:digit (digit-char-p c))
    (:alnum (alphanumericp c))
    (:space (member c '(#\Space #\Tab #\Newline #\Return #\Page)))
    (:upper (upper-case-p c))
    (:lower (lower-case-p c))
    (:punct (and (graphic-char-p c) (not (alphanumericp c)) (char/= c #\Space)))
    (:xdigit (digit-char-p c 16))
    (:print (graphic-char-p c))
    (:graph (and (graphic-char-p c) (char/= c #\Space)))
    (:cntrl (< (char-code c) 32))
    (:blank (member c '(#\Space #\Tab)))
    (t nil)))

(defun rx-set-match (node c)
  (let ((hit (some (lambda (item)
                     (cond
                       ((characterp item) (char= item c))
                       ((consp item) (char<= (car item) c (cdr item)))
                       ((keywordp item) (rx-class-match item c))
                       (t nil)))
                   (rx-set node))))
    (if (rx-negated node) (not hit) hit)))

;;; ---------------------------------------------------------------------------
;;; Matching
;;;
;;; Continuation-passing backtracker: each node calls K with the position it
;;; reached, and K returns non-NIL to accept. That is what makes alternation
;;; and greedy repetition able to give back characters on failure.
;;; ---------------------------------------------------------------------------

(defun rx-match-node (node s pos caps k)
  (ecase (rx-kind node)
    (:char (and (< pos (length s)) (char= (char s pos) (rx-ch node))
                (funcall k (1+ pos))))
    (:any (and (< pos (length s)) (funcall k (1+ pos))))
    (:set (and (< pos (length s)) (rx-set-match node (char s pos))
               (funcall k (1+ pos))))
    (:bol (and (= pos 0) (funcall k pos)))
    (:eol (and (= pos (length s)) (funcall k pos)))
    (:cat (rx-match-seq (rx-children node) s pos caps k))
    (:alt (some (lambda (b) (rx-match-node b s pos caps k)) (rx-children node)))
    (:group
     (let ((idx (rx-group node)))
       (rx-match-node (first (rx-children node)) s pos caps
                      (lambda (end)
                        ;; Record the span, and undo it if the rest fails --
                        ;; otherwise a group that matched inside a discarded
                        ;; branch would leak into BASH_REMATCH.
                        (let ((old (aref caps idx)))
                          (setf (aref caps idx) (cons pos end))
                          (or (funcall k end)
                              (progn (setf (aref caps idx) old) nil)))))))
    (:rep (rx-match-rep node s pos caps k))))

(defun rx-match-seq (nodes s pos caps k)
  (if (null nodes)
      (funcall k pos)
      (rx-match-node (first nodes) s pos caps
                     (lambda (next)
                       (rx-match-seq (rest nodes) s next caps k)))))

(defun rx-match-rep (node s pos caps k)
  "Greedy repetition with backtracking."
  (let ((child (first (rx-children node)))
        (lo (rx-min node))
        (hi (rx-max node)))
    (labels ((try (count p)
               ;; Prefer one more repetition (greedy), then fall back to
               ;; stopping here if the minimum is satisfied.
               (or (and (or (null hi) (< count hi))
                        (rx-match-node child s p caps
                                       (lambda (next)
                                         ;; An empty match would loop forever.
                                         (and (> next p) (try (1+ count) next)))))
                   (and (>= count lo) (funcall k p)))))
      (try 0 pos))))

(defun regex-match (pattern string)
  "Match PATTERN anywhere in STRING (bash `=~' is a search, not an anchor).

Returns NIL, or a list whose first element is the whole match and whose rest
are the capture groups -- the shape of bash's BASH_REMATCH. A group that did
not participate yields an empty string."
  (multiple-value-bind (tree groups) (rx-parse pattern)
    (let ((caps (make-array (1+ groups) :initial-element nil)))
      (loop for start from 0 to (length string) do
        (fill caps nil)
        (let ((end (rx-match-node tree string start caps #'identity)))
          (when end
            (return (cons (subseq string start end)
                          (loop for g from 1 to groups
                                collect (let ((c (aref caps g)))
                                          (if c
                                              (subseq string (car c) (cdr c))
                                              "")))))))))))
