;;;; lexer.lisp --- POSIX shell token recognition (IEEE Std 1003.1 sec 2.3).
;;;;
;;;; The tokenizer here produces a flat stream of tokens. Because the POSIX
;;;; lexer and parser are coupled, the parser drives token classification:
;;;;   * reserved words are only reserved in command position (the parser
;;;;     decides by calling NEXT-TOKEN with :command-position t);
;;;;   * here-documents are collected by the parser after it sees the
;;;;     operator, via COLLECT-HEREDOCS;
;;;;   * alias substitution is out of scope (it's a shell runtime concern).
;;;;
;;;; Token types produced:
;;;;   :word :assignment-word :name :io-number :newline
;;;;   operator keywords: :and-if :or-if :dsemi :dsemi& :dsemi&& :\; :& :\| :\<
;;;;     :\> :dgreat :dless :dlessdash :lessand :greatand :lessgreat :clobber
;;;;     :lparen :rparen :lbrace :rbrace
;;;;   :eof

(in-package #:posh)

(defstruct (token (:constructor make-token (type text line column)))
  type
  (text "" :type string)
  line
  column)

(defstruct (lexer (:constructor %make-lexer))
  (string "" :type string)
  (pos 0 :type fixnum)
  (len 0 :type fixnum)
  (line 1 :type fixnum)
  (column 1 :type fixnum)
  ;; queue of here-doc redirect nodes awaiting body collection at next newline
  (pending-heredocs '())
  ;; set when a here-doc body ran out of input before its delimiter. POSIX
  ;; makes that a warning, so we still accept what we have -- but an
  ;; incremental reader needs to know the text is not yet a complete command.
  (heredoc-eof nil))

(defun make-lexer (string)
  (%make-lexer :string string :len (length string)))

;;; ---------------------------------------------------------------------------
;;; Low-level character access
;;; ---------------------------------------------------------------------------

(declaim (inline lx-peek lx-peek2 lx-eof-p))

(defun lx-peek (lx &optional (offset 0))
  (let ((i (+ (lexer-pos lx) offset)))
    (if (< i (lexer-len lx))
        (char (lexer-string lx) i)
        nil)))

(defun lx-eof-p (lx)
  (>= (lexer-pos lx) (lexer-len lx)))

(defun lx-advance (lx)
  "Consume and return the current character, tracking line/column."
  (let ((c (char (lexer-string lx) (lexer-pos lx))))
    (incf (lexer-pos lx))
    (if (char= c #\Newline)
        (progn (incf (lexer-line lx)) (setf (lexer-column lx) 1))
        (incf (lexer-column lx)))
    c))

(defparameter +operator-start+ '(#\& #\| #\; #\< #\> #\( #\) #\{ #\}))

(defun blank-char-p (c)
  (and c (or (char= c #\Space) (char= c #\Tab))))

;;; ---------------------------------------------------------------------------
;;; Operator recognition (2.10.1 / 2.3 rules 1-4)
;;; ---------------------------------------------------------------------------

(defparameter +operators+
  ;; Longest-match matters; table is scanned longest-first.
  '((";;&" . :dsemi-and)                ; bash extension, harmless to accept
    ("&&"  . :and-if)
    ("||"  . :or-if)
    (";;"  . :dsemi)
    (";&"  . :semi-and)
    ("<<-" . :dlessdash)
    ("<<"  . :dless)
    (">>"  . :dgreat)
    ("<&"  . :lessand)
    (">&"  . :greatand)
    ("<>"  . :lessgreat)
    (">|"  . :clobber)
    ("&"   . :&)
    ("|"   . :pipe)
    (";"   . :semi)
    ("<"   . :less)
    (">"   . :great)
    ("("   . :lparen)
    (")"   . :rparen)))

(defun match-operator (lx)
  "If an operator begins at point, return (values type text length); else NIL."
  (let ((s (lexer-string lx)) (p (lexer-pos lx)) (n (lexer-len lx)))
    (dolist (pair +operators+ nil)
      (let* ((op (car pair)) (l (length op)))
        (when (and (<= (+ p l) n)
                   (string= op s :start2 p :end2 (+ p l)))
          (return (values (cdr pair) op l)))))))

;;; ---------------------------------------------------------------------------
;;; Quote / expansion scanning helpers. These return the raw text consumed
;;; (including delimiters) so that the word retains verbatim source.
;;; ---------------------------------------------------------------------------

(defun scan-single-quote (lx out)
  "Consume a '...' single-quoted string. No escapes inside."
  (vector-push-extend (lx-advance lx) out)   ; opening '
  (loop
    (when (lx-eof-p lx)
      (parse-error* (lexer-line lx) (lexer-column lx)
                    "unterminated single-quoted string"))
    (let ((c (lx-advance lx)))
      (vector-push-extend c out)
      (when (char= c #\') (return)))))

(defun scan-dollar-quote (lx out)
  "Consume a $'...' literal, in which a backslash escapes the next character
-- including the closing quote.

This differs from an ordinary '...' string, where nothing is special. Scanning
it as one meant `$'it\\'s'' ended the string at the escaped quote and the rest
of the line was read as a new one (\"unterminated single-quoted string\")."
  (vector-push-extend (lx-advance lx) out)   ; $
  (vector-push-extend (lx-advance lx) out)   ; opening '
  (loop
    (when (lx-eof-p lx)
      (parse-error* (lexer-line lx) (lexer-column lx)
                    "unterminated $'...' string"))
    (let ((c (lx-advance lx)))
      (vector-push-extend c out)
      (cond
        ((char= c #\\)
         (unless (lx-eof-p lx) (vector-push-extend (lx-advance lx) out)))
        ((char= c #\') (return))))))

(defun scan-double-quote (lx out)
  "Consume a \"...\" string, honoring \\ escapes and nested $(...) / `...` / ${...}."
  (vector-push-extend (lx-advance lx) out)   ; opening "
  (loop
    (when (lx-eof-p lx)
      (parse-error* (lexer-line lx) (lexer-column lx)
                    "unterminated double-quoted string"))
    (let ((c (lx-peek lx)))
      (cond
        ((char= c #\")
         (vector-push-extend (lx-advance lx) out) (return))
        ((char= c #\\)
         (vector-push-extend (lx-advance lx) out)
         (unless (lx-eof-p lx)
           (vector-push-extend (lx-advance lx) out)))
        ((char= c #\`)
         (scan-backquote lx out))
        ((and (char= c #\$) (eql (lx-peek lx 1) #\())
         (scan-dollar-paren lx out))
        ((and (char= c #\$) (eql (lx-peek lx 1) #\{))
         (scan-dollar-brace lx out))
        (t (vector-push-extend (lx-advance lx) out))))))

(defun scan-backquote (lx out)
  "Consume a `...` command substitution; backslash escapes ` \\ and $."
  (vector-push-extend (lx-advance lx) out)   ; opening `
  (loop
    (when (lx-eof-p lx)
      (parse-error* (lexer-line lx) (lexer-column lx)
                    "unterminated backquote command substitution"))
    (let ((c (lx-peek lx)))
      (cond
        ((char= c #\`)
         (vector-push-extend (lx-advance lx) out) (return))
        ((char= c #\\)
         (vector-push-extend (lx-advance lx) out)
         (unless (lx-eof-p lx) (vector-push-extend (lx-advance lx) out)))
        (t (vector-push-extend (lx-advance lx) out))))))

(defun scan-balanced (lx out open close)
  "Consume from an already-open OPEN paren/brace to its matching CLOSE,
tracking nesting, quotes and nested substitutions. Assumes point is on OPEN."
  (let ((depth 0))
    (loop
      (when (lx-eof-p lx)
        (parse-error* (lexer-line lx) (lexer-column lx)
                      "unterminated substitution (expected ~C)" close))
      (let ((c (lx-peek lx)))
        (cond
          ((char= c open)
           (incf depth) (vector-push-extend (lx-advance lx) out))
          ((char= c close)
           (decf depth) (vector-push-extend (lx-advance lx) out)
           (when (zerop depth) (return)))
          ((and (char= c #\$) (eql (lx-peek lx 1) #\')) (scan-dollar-quote lx out))
          ((char= c #\') (scan-single-quote lx out))
          ((char= c #\") (scan-double-quote lx out))
          ((char= c #\`) (scan-backquote lx out))
          ((char= c #\\)
           (vector-push-extend (lx-advance lx) out)
           (unless (lx-eof-p lx) (vector-push-extend (lx-advance lx) out)))
          ((and (char= c #\$) (eql (lx-peek lx 1) #\())
           (scan-dollar-paren lx out))
          ((and (char= c #\$) (eql (lx-peek lx 1) #\{))
           (scan-dollar-brace lx out))
          (t (vector-push-extend (lx-advance lx) out)))))))

(defun scan-dollar-paren (lx out)
  "$( ... ) command substitution (also handles $(( )) arithmetic naturally)."
  (vector-push-extend (lx-advance lx) out)   ; $
  (scan-balanced lx out #\( #\)))

(defun scan-dollar-brace (lx out)
  "${ ... } parameter expansion."
  (vector-push-extend (lx-advance lx) out)   ; $
  (scan-balanced lx out #\{ #\}))

;;; ---------------------------------------------------------------------------
;;; Word building
;;; ---------------------------------------------------------------------------

(defun word-char-terminator-p (lx)
  "True if the character at point ends the current word."
  (let ((c (lx-peek lx)))
    (or (null c)
        (blank-char-p c)
        (char= c #\Newline)
        ;; operator characters delimit words, EXCEPT { and } which are
        ;; ordinary word characters at the lexical level (they are reserved
        ;; words recognized by position, per rule 1).
        (member c '(#\& #\| #\; #\< #\> #\( #\))))))

(defun skip-blanks-and-comments (lx)
  "Skip spaces/tabs and (# comment) runs, plus line continuations. Returns nil."
  (loop
    (let ((c (lx-peek lx)))
      (cond
        ((blank-char-p c) (lx-advance lx))
        ;; line continuation: backslash-newline is removed entirely
        ((and (eql c #\\) (eql (lx-peek lx 1) #\Newline))
         (lx-advance lx) (lx-advance lx))
        ((eql c #\#)
         ;; comment to end of line (newline not consumed)
         (loop until (or (lx-eof-p lx) (eql (lx-peek lx) #\Newline))
               do (lx-advance lx)))
        (t (return))))))

(defun looks-like-io-number (text next-char)
  "A run of digits immediately followed by < or > is an IO_NUMBER (rule 2/7)."
  (and (plusp (length text))
       (every #'digit-char-p text)
       (member next-char '(#\< #\>))))

(defun scan-word (lx)
  "Scan one WORD token starting at point. Returns the raw text string.
Detects assignment-word shape as a side product via classify."
  (let ((out (make-array 16 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop
      (when (word-char-terminator-p lx) (return))
      (let ((c (lx-peek lx)))
        (cond
          ((char= c #\\)
           (if (eql (lx-peek lx 1) #\Newline)
               ;; line continuation: backslash-newline is removed entirely
               (progn (lx-advance lx) (lx-advance lx))
               (progn (vector-push-extend (lx-advance lx) out)
                      (unless (lx-eof-p lx) (vector-push-extend (lx-advance lx) out)))))
          ((and (char= c #\$) (eql (lx-peek lx 1) #\')) (scan-dollar-quote lx out))
          ((char= c #\') (scan-single-quote lx out))
          ((char= c #\") (scan-double-quote lx out))
          ((char= c #\`) (scan-backquote lx out))
          ((and (char= c #\$) (eql (lx-peek lx 1) #\()) (scan-dollar-paren lx out))
          ((and (char= c #\$) (eql (lx-peek lx 1) #\{)) (scan-dollar-brace lx out))
          (t (vector-push-extend (lx-advance lx) out)))))
    (coerce out 'string)))

;;; ---------------------------------------------------------------------------
;;; Assignment-word / name recognition (grammar rules 5, 7)
;;; ---------------------------------------------------------------------------

(defun valid-name-p (s)
  "POSIX name: [A-Za-z_][A-Za-z0-9_]*"
  (and (plusp (length s))
       (let ((c0 (char s 0)))
         (or (alpha-char-p c0) (char= c0 #\_)))
       (every (lambda (c) (or (alphanumericp c) (char= c #\_))) s)))

(defun assignment-word-split (text)
  "If TEXT has the form NAME=... (with NAME a valid name, no quoting in NAME),
return (values name value-string); else NIL. The '=' must appear before any
quote/expansion so that e.g. `a=b` is an assignment but `a'='b` is a word."
  (let ((eq (position #\= text)))
    (when (and eq (plusp eq))
      (let ((name (subseq text 0 eq)))
        (when (valid-name-p name)
          (values name (subseq text (1+ eq))))))))

;;; ---------------------------------------------------------------------------
;;; The main entry: NEXT-TOKEN
;;; ---------------------------------------------------------------------------

(defun next-token (lx &key command-position accept-assignment)
  "Return the next TOKEN. When COMMAND-POSITION is true, a bare word that is a
valid NAME may be tagged :name (used for function-def / for / case detection);
that decision is mostly left to the parser, so we tag words generically and
let the parser reclassify. When ACCEPT-ASSIGNMENT is true, NAME=WORD shapes
are returned as :assignment-word."
  (skip-blanks-and-comments lx)
  (let ((line (lexer-line lx)) (col (lexer-column lx)))
    (when (lx-eof-p lx)
      (return-from next-token (make-token :eof "" line col)))
    (let ((c (lx-peek lx)))
      (cond
        ;; newline is its own token (list separator / here-doc trigger)
        ((char= c #\Newline)
         (lx-advance lx)
         (make-token :newline (string #\Newline) line col))
        ;; { and } as standalone tokens only matter to the parser as reserved
        ;; words; lexically they are word constituents. But when they stand
        ;; alone we still emit words and let the parser test reserved-ness.
        ;; Operators:
        ((member c +operator-start+ :test #'char=)
         ;; { and } are NOT operators lexically; skip them here so they fold
         ;; into words unless they are a standalone reserved word. We handle
         ;; the standalone case in the parser by checking word text.
         (if (member c '(#\{ #\}))
             (scan-word-token lx line col :accept-assignment accept-assignment)
             (multiple-value-bind (type text len) (match-operator lx)
               (if type
                   (progn (dotimes (_ len) (lx-advance lx))
                          (make-token type text line col))
                   ;; shouldn't happen, but fall back to word
                   (scan-word-token lx line col
                                    :accept-assignment accept-assignment)))))
        ;; digits possibly forming IO_NUMBER
        ((digit-char-p c)
         (scan-word-token lx line col :accept-assignment accept-assignment))
        (t
         (scan-word-token lx line col :accept-assignment accept-assignment))))))

(defun scan-word-token (lx line col &key accept-assignment)
  (let* ((text (scan-word lx)))
    ;; IO_NUMBER: digits directly before a redirection operator, no blanks
    (when (looks-like-io-number text (lx-peek lx))
      (return-from scan-word-token (make-token :io-number text line col)))
    (when accept-assignment
      (multiple-value-bind (name value) (assignment-word-split text)
        (declare (ignore value))
        (when name
          (return-from scan-word-token (make-token :assignment-word text line col)))))
    (make-token :word text line col)))

;;; ---------------------------------------------------------------------------
;;; Here-document collection. The parser registers a here-doc redirect via
;;; QUEUE-HEREDOC when it parses <<word / <<-word; the *body* is read starting
;;; at the next unquoted newline. We expose COLLECT-HEREDOCS for the parser to
;;; call at each newline.
;;; ---------------------------------------------------------------------------

(defun queue-heredoc (lx redirect strip-p delimiter-word)
  "Register REDIRECT (a REDIRECT node) for body collection at next newline.
DELIMITER-WORD is the raw word after << ; quoting in it disables expansion."
  (setf (lexer-pending-heredocs lx)
        (nconc (lexer-pending-heredocs lx)
               (list (list redirect strip-p delimiter-word)))))

(defun unquote-delimiter (word)
  "Compute the literal delimiter string and whether it was quoted."
  (let ((out (make-array (length word) :element-type 'character
                                       :adjustable t :fill-pointer 0))
        (quoted nil) (i 0) (n (length word)))
    (loop while (< i n) do
      (let ((c (char word i)))
        (cond
          ((char= c #\\) (setf quoted t) (incf i)
           (when (< i n) (vector-push-extend (char word i) out) (incf i)))
          ((char= c #\') (setf quoted t) (incf i)
           (loop while (and (< i n) (char/= (char word i) #\'))
                 do (vector-push-extend (char word i) out) (incf i))
           (incf i))
          ((char= c #\") (setf quoted t) (incf i)
           (loop while (and (< i n) (char/= (char word i) #\"))
                 do (vector-push-extend (char word i) out) (incf i))
           (incf i))
          (t (vector-push-extend c out) (incf i)))))
    (values (coerce out 'string) quoted)))

(defun collect-heredocs (lx)
  "Called by the parser right after consuming a NEWLINE. Reads bodies for all
pending here-docs, in order, and attaches them to their redirect nodes."
  (dolist (entry (lexer-pending-heredocs lx))
    (destructuring-bind (redirect strip-p delimiter-word) entry
      (multiple-value-bind (delim quoted) (unquote-delimiter delimiter-word)
        (let ((body (make-array 64 :element-type 'character
                                   :adjustable t :fill-pointer 0)))
          (loop
            (when (lx-eof-p lx)
              ;; POSIX: EOF before delimiter is a warning; accept what we have,
              ;; but flag it so callers reading incrementally keep going.
              (setf (lexer-heredoc-eof lx) t)
              (return))
            ;; read one physical line
            (let ((line-start (lexer-pos lx))
                  (line (make-array 32 :element-type 'character
                                       :adjustable t :fill-pointer 0)))
              (declare (ignore line-start))
              (loop until (or (lx-eof-p lx) (eql (lx-peek lx) #\Newline))
                    do (vector-push-extend (lx-advance lx) line))
              (unless (lx-eof-p lx) (lx-advance lx)) ; consume newline
              (let ((line-str (coerce line 'string)))
                ;; with <<-, leading tabs are stripped for both body and the
                ;; delimiter comparison line
                (let ((cmp (if strip-p (string-left-trim '(#\Tab) line-str) line-str)))
                  (if (string= cmp delim)
                      (return)
                      (progn
                        (loop for ch across (if strip-p cmp line-str)
                              do (vector-push-extend ch body))
                        (vector-push-extend #\Newline body)))))))
          (setf (redirect-heredoc redirect)
                (list delim (coerce body 'string) quoted strip-p))))))
  (setf (lexer-pending-heredocs lx) '()))

;;; ---------------------------------------------------------------------------
;;; Convenience: tokenize a whole string (no parser feedback; here-docs and
;;; reserved words are left generic). Handy for tests / debugging.
;;; ---------------------------------------------------------------------------

(defun tokenize (string &key accept-assignment)
  (let ((lx (make-lexer string)) (acc '()))
    (loop
      (let ((tok (next-token lx :accept-assignment accept-assignment)))
        (push tok acc)
        (when (eq (token-type tok) :eof) (return))))
    (nreverse acc)))
