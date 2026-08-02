;;;; parser.lisp --- Recursive-descent parser for POSIX shell grammar (2.10).
;;;;
;;;; Grammar (abbreviated, from IEEE Std 1003.1):
;;;;
;;;;   program          : linebreak complete_commands linebreak | linebreak
;;;;   complete_command : list separator_op | list
;;;;   list             : list separator_op and_or | and_or
;;;;   and_or           : pipeline | and_or '&&' linebreak pipeline
;;;;                                | and_or '||' linebreak pipeline
;;;;   pipeline         : pipe_sequence | '!' pipe_sequence
;;;;   pipe_sequence    : command | pipe_sequence '|' linebreak command
;;;;   command          : simple_command | compound_command
;;;;                     | compound_command redirect_list | function_definition
;;;;   compound_command : brace_group | subshell | for_clause | case_clause
;;;;                     | if_clause | while_clause | until_clause
;;;;
;;;; Reserved words (if then else elif fi do done case esac while until for
;;;; { } ! in) are recognized only in command position -- the parser checks
;;;; word text against the reserved set exactly where the grammar allows it.

(in-package #:sxsh)

(defstruct (parser (:constructor %make-parser))
  lexer
  cur                                   ; current lookahead token
  (accept-assignment t))

(defun make-parser (string)
  (let ((p (%make-parser :lexer (make-lexer string))))
    (advance p)
    p))

(defun cur (p) (parser-cur p))
(defun cur-type (p) (token-type (parser-cur p)))
(defun cur-text (p) (token-text (parser-cur p)))

(defun advance (p &key (accept-assignment t))
  "Consume the current token, fetch the next. Collects here-doc bodies when a
NEWLINE is consumed."
  (let ((consumed (parser-cur p)))
    (when (and consumed (eq (token-type consumed) :newline))
      (collect-heredocs (parser-lexer p)))
    (setf (parser-cur p)
          (next-token (parser-lexer p) :accept-assignment accept-assignment))
    consumed))

(defun expect (p type)
  (unless (eq (cur-type p) type)
    (perr p "expected ~A but found ~A ~S" type (cur-type p) (cur-text p)))
  (advance p))

(defun perr (p format &rest args)
  (let ((tok (parser-cur p)))
    (apply #'parse-error* (and tok (token-line tok)) (and tok (token-column tok))
           format args)))

;;; ---------------------------------------------------------------------------
;;; Reserved words
;;; ---------------------------------------------------------------------------

(defparameter +reserved+
  '("if" "then" "else" "elif" "fi" "do" "done" "case" "esac"
    "while" "until" "for" "in" "{" "}" "!" "time" "function"))

(defun reservedp (p word)
  "True if the current token is a WORD whose text equals the given reserved
word. Reserved words are only WORDs (never quoted forms)."
  (and (eq (cur-type p) :word)
       (string= (cur-text p) word)))

(defun any-reserved-p (p)
  (and (eq (cur-type p) :word)
       (member (cur-text p) +reserved+ :test #'string=)))

;;; ---------------------------------------------------------------------------
;;; linebreak / separators
;;; ---------------------------------------------------------------------------

(defun skip-newlines (p)
  "linebreak: {newline}"
  (loop while (eq (cur-type p) :newline) do (advance p)))

(defun at-list-end-p (p)
  "Tokens that terminate a list / compound-list in the enclosing context."
  (or (member (cur-type p) '(:eof :rparen))
      (and (eq (cur-type p) :word)
           (member (cur-text p) '("then" "else" "elif" "fi" "do" "done" "esac" "}")
                   :test #'string=))
      (eq (cur-type p) :dsemi)
      (eq (cur-type p) :semi-and)
      (eq (cur-type p) :dsemi-and)))

;;; ---------------------------------------------------------------------------
;;; Top level
;;; ---------------------------------------------------------------------------

(defun parse-program (p)
  "program : linebreak [ complete_commands linebreak ]"
  (skip-newlines p)
  (let ((commands '()))
    (loop
      (when (eq (cur-type p) :eof) (return))
      (push (parse-complete-command p) commands)
      (skip-newlines p))
    (nreverse commands)))

(defun parse-complete-command (p)
  "list of and_or joined by separator_op ( ; or & ), optional trailing sep."
  (let ((entries '()))
    (loop
      (let ((ao (parse-and-or p)))
        (let ((sep (cond ((eq (cur-type p) :semi) (advance p) :semi)
                         ((eq (cur-type p) :&)    (advance p) :async)
                         (t nil))))
          (push (cons ao sep) entries)
          ;; continue the list only if another and_or clearly follows
          (when (or (null sep)
                    (member (cur-type p) '(:eof :newline :rparen))
                    (at-list-end-p p))
            (return)))))
    (make-complete-command (nreverse entries))))

;;; ---------------------------------------------------------------------------
;;; and_or / pipeline
;;; ---------------------------------------------------------------------------

(defun parse-and-or (p)
  "and_or : pipeline { (&& | ||) linebreak pipeline }  (left-associative)"
  (let ((left (parse-pipeline p)))
    (loop
      (let ((op (case (cur-type p)
                  (:and-if :&&)
                  (:or-if  :\|\|)
                  (t nil))))
        (unless op (return left))
        (advance p)
        (skip-newlines p)
        (setf left (make-and-or op left (parse-pipeline p)))))))

(defun parse-pipeline (p)
  "pipeline : ['time' ['-p']] ['!'] command { '|' linebreak command }"
  (let ((timed nil) (bang nil))
    ;; `time` reserved word: times the pipeline. Recognized only when it looks
    ;; like the timing keyword (followed by more of a pipeline), not when
    ;; `time` is itself the command being run.
    (when (and (reservedp p "time") (time-keyword-ahead-p p))
      (advance p)
      (setf timed :time)
      ;; optional -p (POSIX portable output format)
      (when (and (eq (cur-type p) :word) (string= (cur-text p) "-p"))
        (setf timed :time-p)
        (advance p)))
    (when (reservedp p "!")
      (setf bang t)
      (advance p))
    (let ((cmds (list (parse-command p))))
      (loop while (member (cur-type p) '(:pipe :pipe-and)) do
        ;; bash `a |& b' is exactly `a 2>&1 | b', so the stderr dup belongs to
        ;; the command on the LEFT, which we have already parsed.
        (when (eq (cur-type p) :pipe-and)
          (add-stderr-to-stdout (first cmds)))
        (advance p)
        (skip-newlines p)
        (push (parse-command p) cmds))
      (let ((cmds (nreverse cmds)))
        (if (and (null bang) (null timed) (= 1 (length cmds)))
            (first cmds)                ; unwrap trivial pipeline
            (make-pipeline cmds bang timed))))))

(defun add-stderr-to-stdout (node)
  "Append a `2>&1' redirection to NODE, for bash's `|&'.

Every command node type carries a redirect list, so this works for a compound
on the left of the pipe as well as a simple command."
  (let ((r (make-redirect :>& 2 (make-word "1"))))
    (macrolet ((push-redirect (accessor)
                 `(setf (,accessor node) (append (,accessor node) (list r)))))
      (typecase node
        (simple-command (push-redirect simple-command-redirects))
        (subshell       (push-redirect subshell-redirects))
        (brace-group    (push-redirect brace-group-redirects))
        (if-clause      (push-redirect if-clause-redirects))
        (for-clause     (push-redirect for-clause-redirects))
        (while-clause   (push-redirect while-clause-redirects))
        (until-clause   (push-redirect until-clause-redirects))
        (case-clause    (push-redirect case-clause-redirects))
        (t nil))))
  node)

(defun time-keyword-ahead-p (p)
  "True if the current `time` word should be treated as the timing reserved
word rather than a command named time. It is the keyword when followed by
another word/compound (the pipeline to time), or by `-p`, or by end/`!`.
It is NOT the keyword when followed by a pipe/operator/terminator (e.g.
`time | foo`, or `time` alone as a command)."
  (let* ((lx (parser-lexer p))
         (saved-pos (lexer-pos lx))
         (saved-line (lexer-line lx))
         (saved-col (lexer-column lx))
         (saved-hd (copy-list (lexer-pending-heredocs lx))))
    (let ((tok (next-token lx)))
      (setf (lexer-pos lx) saved-pos
            (lexer-line lx) saved-line
            (lexer-column lx) saved-col
            (lexer-pending-heredocs lx) saved-hd)
      (member (token-type tok)
              '(:word :assignment-word :lparen :io-number)))))

;;; ---------------------------------------------------------------------------
;;; command dispatch
;;; ---------------------------------------------------------------------------

(defun parse-command (p)
  (cond
    ;; compound commands by leading reserved word / paren / brace
    ((reservedp p "{")     (parse-brace-group p))
    ((eq (cur-type p) :lparen) (parse-subshell p))
    ((reservedp p "if")    (parse-if p))
    ((reservedp p "for")   (parse-for p))
    ((reservedp p "while") (parse-while p))
    ((reservedp p "until") (parse-until p))
    ((reservedp p "case")  (parse-case p))
    ;; bash `[[ ... ]]' conditional expression
    ((eq (cur-type p) :dlbracket)
     (let ((text (cur-text p)))
       (advance p)
       (make-cond-expr text (parse-redirect-list p))))
    ;; bash `((expr))' as a command
    ((eq (cur-type p) :dlparen)
     (let ((expr (cur-text p)))
       (advance p)
       (make-arith-command expr (parse-redirect-list p))))
    ;; function definition:  NAME ( )   -- and bash's `function NAME [()]'
    ((and (reservedp p "function") (function-keyword-ahead-p p))
     (parse-function-keyword-def p))
    ((function-def-ahead-p p) (parse-function-def p))
    (t (parse-simple-command p))))

(defun function-def-ahead-p (p)
  "Detect NAME '(' ')' at command position without consuming (single-token
lookahead plus a lexer probe). We only need to see WORD then '(' ; the grammar
requires ')' next, which parse-function-def enforces."
  (and (eq (cur-type p) :word)
       (valid-name-p (cur-text p))
       (not (any-reserved-p p))
       ;; probe: is the very next token an lparen?
       (probe-lparen-p p)))

(defun probe-lparen-p (p)
  "Look one token past current without disturbing parser state permanently.
We snapshot the lexer, read a token, then restore."
  (let* ((lx (parser-lexer p))
         (saved-pos (lexer-pos lx))
         (saved-line (lexer-line lx))
         (saved-col (lexer-column lx))
         (saved-heredocs (copy-list (lexer-pending-heredocs lx))))
    (let ((tok (next-token lx)))
      (setf (lexer-pos lx) saved-pos
            (lexer-line lx) saved-line
            (lexer-column lx) saved-col
            (lexer-pending-heredocs lx) saved-heredocs)
      (eq (token-type tok) :lparen))))

;;; ---------------------------------------------------------------------------
;;; simple command
;;; ---------------------------------------------------------------------------

(defun redirect-op-token-p (type)
  (member type '(:less :great :dgreat :dless :dlessdash
                 :lessand :greatand :lessgreat :clobber
                 ;; bash: <<< here-string, &> and &>> stdout+stderr
                 :tless :and-great :and-dgreat)))

(defun parse-simple-command (p)
  "cmd_prefix (assignments/redirs) then words and more redirs."
  (let ((assignments '()) (words '()) (redirects '())
        (start-line (let ((tk (parser-cur p))) (if tk (token-line tk) 0))))
    ;; prefix: assignment_words and redirects, in any order, before first word
    (loop
      (cond
        ((eq (cur-type p) :assignment-word)
         (multiple-value-bind (name value appendp)
             (assignment-word-split (cur-text p))
           (push (make-assignment name
                                  (and (plusp (length value)) (make-word value))
                                  appendp)
                 assignments))
         (advance p))
        ((or (redirect-op-token-p (cur-type p)) (eq (cur-type p) :io-number))
         (push (parse-redirect p) redirects))
        (t (return))))
    ;; command word + suffix (words and redirects)
    (loop
      (cond
        ((eq (cur-type p) :word)
         ;; a reserved word here in command-word position would have been
         ;; dispatched already; as an argument it's an ordinary word.
         (push (make-word (cur-text p)) words)
         (advance p))
        ((eq (cur-type p) :io-number)
         (push (parse-redirect p) redirects))
        ((redirect-op-token-p (cur-type p))
         (push (parse-redirect p) redirects))
        ;; a bare assignment after words is just a word in POSIX, but the lexer
        ;; only tags :assignment-word in prefix position via accept-assignment;
        ;; once we've seen a word it still tags them, so treat as word:
        ((eq (cur-type p) :assignment-word)
         (push (make-word (cur-text p)) words)
         (advance p))
        (t (return))))
    (when (and (null assignments) (null words) (null redirects))
      (perr p "expected a command"))
    (let ((cmd (make-simple-command (nreverse assignments) (nreverse words)
                                    (nreverse redirects))))
      (setf (node-line cmd) start-line)
      cmd)))

;;; ---------------------------------------------------------------------------
;;; redirections
;;; ---------------------------------------------------------------------------

(defun parse-redirect (p)
  (let ((fd nil))
    (when (eq (cur-type p) :io-number)
      (setf fd (parse-integer (cur-text p)))
      (advance p))
    (let ((op-type (cur-type p)))
      (unless (redirect-op-token-p op-type)
        (perr p "expected redirection operator"))
      (let ((heredoc-p (member op-type '(:dless :dlessdash)))
            (strip-p (eq op-type :dlessdash))
            (op-kw (case op-type
                     (:less :<) (:great :>) (:dgreat :>>)
                     (:dless :<<) (:dlessdash :<<-)
                     (:lessand :<&) (:greatand :>&)
                     (:lessgreat :<>) (:clobber :>\|)
                     ;; A here-string is not a here-doc: its body is the word
                     ;; itself, so nothing is queued for collection at the
                     ;; next newline.
                     (:tless :<<<)
                     (:and-great :&>) (:and-dgreat :&>>))))
        (advance p)
        ;; target word (filename or fd)
        (unless (member (cur-type p) '(:word :assignment-word :io-number))
          (perr p "expected redirection target after ~A" op-kw))
        (let* ((target-text (cur-text p))
               (target (make-word target-text))
               (r (make-redirect op-kw fd target)))
          (advance p)
          (when heredoc-p
            (queue-heredoc (parser-lexer p) r strip-p target-text))
          r)))))

(defun parse-redirect-list (p)
  "Zero or more trailing redirections after a compound command."
  (let ((redirs '()))
    (loop while (or (redirect-op-token-p (cur-type p)) (eq (cur-type p) :io-number))
          do (push (parse-redirect p) redirs))
    (nreverse redirs)))

;;; ---------------------------------------------------------------------------
;;; compound-list  (used inside all compound commands)
;;; ---------------------------------------------------------------------------

(defun parse-compound-list (p)
  "compound_list : linebreak term [separator]
   term          : term separator and_or | and_or
Returns a COMPLETE-COMMAND node (reusing the same structure)."
  (skip-newlines p)
  (let ((entries '()))
    (loop
      (when (at-list-end-p p) (return))
      (let ((ao (parse-and-or p)))
        (let ((sep (cond ((eq (cur-type p) :semi) (advance p) :semi)
                         ((eq (cur-type p) :&)    (advance p) :async)
                         ((eq (cur-type p) :newline) :newline)
                         (t nil))))
          (push (cons ao (if (eq sep :newline) :semi sep)) entries)
          (skip-newlines p)
          (when (or (null sep) (at-list-end-p p)) (return)))))
    (make-complete-command (nreverse entries))))

;;; ---------------------------------------------------------------------------
;;; brace group / subshell
;;; ---------------------------------------------------------------------------

(defun parse-brace-group (p)
  (advance p)                           ; consume {
  (let ((body (parse-compound-list p)))
    (unless (reservedp p "}")
      (perr p "expected } to close brace group"))
    (advance p)
    (make-brace-group body (parse-redirect-list p))))

(defun parse-subshell (p)
  (expect p :lparen)
  (let ((body (parse-compound-list p)))
    (expect p :rparen)
    (make-subshell body (parse-redirect-list p))))

;;; ---------------------------------------------------------------------------
;;; if
;;; ---------------------------------------------------------------------------

(defun parse-if (p)
  (advance p)                           ; if
  (let ((cond (parse-compound-list p)))
    (unless (reservedp p "then") (perr p "expected 'then'"))
    (advance p)
    (let ((then (parse-compound-list p))
          (else nil))
      (cond
        ((reservedp p "elif")
         (setf else (parse-if-tail-elif p)))
        ((reservedp p "else")
         (advance p)
         (setf else (parse-compound-list p))
         (unless (reservedp p "fi") (perr p "expected 'fi'"))
         (advance p))
        ((reservedp p "fi")
         (advance p))
        (t (perr p "expected 'elif', 'else', or 'fi'")))
      (make-if-clause cond then else (parse-redirect-list p)))))

(defun parse-if-tail-elif (p)
  "Handle an elif as a nested if-clause in the else slot."
  (advance p)                           ; elif
  (let ((cond (parse-compound-list p)))
    (unless (reservedp p "then") (perr p "expected 'then' after elif"))
    (advance p)
    (let ((then (parse-compound-list p)) (else nil))
      (cond
        ((reservedp p "elif") (setf else (parse-if-tail-elif p)))
        ((reservedp p "else")
         (advance p)
         (setf else (parse-compound-list p))
         (unless (reservedp p "fi") (perr p "expected 'fi'"))
         (advance p))
        ((reservedp p "fi") (advance p))
        (t (perr p "expected 'elif', 'else', or 'fi'")))
      (make-if-clause cond then else nil))))

;;; ---------------------------------------------------------------------------
;;; while / until
;;; ---------------------------------------------------------------------------

(defun parse-do-group (p)
  (unless (reservedp p "do") (perr p "expected 'do'"))
  (advance p)
  (let ((body (parse-compound-list p)))
    (unless (reservedp p "done") (perr p "expected 'done'"))
    (advance p)
    body))

(defun parse-while (p)
  (advance p)
  (let ((cond (parse-compound-list p)))
    (let ((body (parse-do-group p)))
      (make-while-clause cond body (parse-redirect-list p)))))

(defun parse-until (p)
  (advance p)
  (let ((cond (parse-compound-list p)))
    (let ((body (parse-do-group p)))
      (make-until-clause cond body (parse-redirect-list p)))))

;;; ---------------------------------------------------------------------------
;;; for
;;; ---------------------------------------------------------------------------

(defun parse-for (p)
  (advance p)                           ; for
  ;; bash C-style `for ((init; cond; step))'. The lexer already delivered the
  ;; whole `((...))' as one token, so the three clauses are split here.
  (when (eq (cur-type p) :dlparen)
    (return-from parse-for (parse-arith-for p)))
  (unless (and (eq (cur-type p) :word) (valid-name-p (cur-text p)))
    (perr p "expected name after 'for'"))
  (let ((name (cur-text p))
        (words :default))
    (advance p)
    (skip-newlines p)                   ; linebreak allowed before `in`
    ;; a sequential separator may follow the name when there is no `in` list
    ;; (e.g. `for i; do ...` / `for i\n do ...`)
    (when (and (not (reservedp p "in")) (eq (cur-type p) :semi))
      (advance p)
      (skip-newlines p))
    (when (reservedp p "in")
      (advance p)
      (setf words '())
      (loop while (member (cur-type p) '(:word :assignment-word :io-number))
            do (push (make-word (cur-text p)) words) (advance p))
      (setf words (nreverse words))
      ;; sequential sep required after the word list
      (cond ((eq (cur-type p) :semi) (advance p))
            ((eq (cur-type p) :newline) (skip-newlines p))
            (t nil)))
    (skip-newlines p)
    (let ((body (parse-do-group p)))
      (make-for-clause name words body (parse-redirect-list p)))))

;;; ---------------------------------------------------------------------------
;;; case
;;; ---------------------------------------------------------------------------

(defun parse-arith-for (p)
  "bash: for ((init; cond; step)) do ... done"
  (let ((parts (split-arith-for-clauses (cur-text p))))
    (advance p)
    ;; an optional `;' or newline may separate the header from `do'
    (loop while (member (cur-type p) '(:semi :newline)) do (advance p))
    (unless (reservedp p "do") (perr p "expected 'do' after for (( ))"))
    (advance p)
    (let ((body (parse-compound-list p)))
      (unless (reservedp p "done") (perr p "expected 'done'"))
      (advance p)
      (make-arith-for (first parts) (second parts) (third parts)
                      body (parse-redirect-list p)))))

(defun split-arith-for-clauses (text)
  "Split `init; cond; step' on top-level semicolons. An omitted clause is the
empty string, which the executor reads as `absent' -- an empty condition is
true, as in C."
  (let ((parts '()) (start 0) (depth 0) (n (length text)) (i 0))
    (loop while (< i n) do
      (let ((c (char text i)))
        (cond
          ((char= c #\() (incf depth) (incf i))
          ((char= c #\)) (decf depth) (incf i))
          ((and (char= c #\;) (zerop depth))
           (push (subseq text start i) parts) (setf start (1+ i)) (incf i))
          (t (incf i)))))
    (push (subseq text start) parts)
    (let ((out (nreverse parts)))
      (list (or (first out) "") (or (second out) "") (or (third out) "")))))

(defun parse-case (p)
  (advance p)                           ; case
  (unless (member (cur-type p) '(:word :assignment-word))
    (perr p "expected word after 'case'"))
  (let ((word (make-word (cur-text p))))
    (advance p)
    (skip-newlines p)
    (unless (reservedp p "in") (perr p "expected 'in' after case word"))
    (advance p)
    (skip-newlines p)
    (let ((items '()))
      (loop
        (when (reservedp p "esac") (return))
        (push (parse-case-item p) items)
        ;; parse-case-item stops after its terminator or at esac
        (skip-newlines p))
      (unless (reservedp p "esac") (perr p "expected 'esac'"))
      (advance p)
      (make-case-clause word (nreverse items) (parse-redirect-list p)))))

(defun parse-case-item (p)
  ;; optional leading (
  (when (eq (cur-type p) :lparen) (advance p))
  (let ((patterns '()))
    ;; first pattern
    (unless (member (cur-type p) '(:word :assignment-word))
      (perr p "expected pattern in case item"))
    (push (make-word (cur-text p)) patterns)
    (advance p)
    (loop while (eq (cur-type p) :pipe) do
      (advance p)
      (unless (member (cur-type p) '(:word :assignment-word))
        (perr p "expected pattern after '|'"))
      (push (make-word (cur-text p)) patterns)
      (advance p))
    (expect p :rparen)
    (skip-newlines p)
    (let ((body (if (or (reservedp p "esac")
                        (member (cur-type p) '(:dsemi :semi-and :dsemi-and)))
                    nil
                    (parse-compound-list p)))
          (terminator nil))
      (case (cur-type p)
        (:dsemi     (setf terminator :\;\;)  (advance p))
        (:semi-and  (setf terminator :\;&)   (advance p))
        (:dsemi-and (setf terminator :\;\;&) (advance p)))
      (make-case-item (nreverse patterns) body terminator))))

;;; ---------------------------------------------------------------------------
;;; function definition
;;; ---------------------------------------------------------------------------

(defun function-keyword-ahead-p (p)
  "True if the current `function' word introduces a bash function definition.

`function' is not reserved in POSIX, so it has to stay usable as an ordinary
command name -- `function' alone, or `function | cat', must still run a
program called function. It is the keyword only when a NAME follows."
  (let* ((lx (parser-lexer p))
         (saved-pos (lexer-pos lx))
         (saved-line (lexer-line lx))
         (saved-col (lexer-column lx))
         (saved-hd (copy-list (lexer-pending-heredocs lx))))
    (let ((tok (next-token lx)))
      (setf (lexer-pos lx) saved-pos
            (lexer-line lx) saved-line
            (lexer-column lx) saved-col
            (lexer-pending-heredocs lx) saved-hd)
      (and (eq (token-type tok) :word)
           (valid-name-p (token-text tok))))))

(defun parse-function-keyword-def (p)
  "bash: `function NAME { ... }' or `function NAME () { ... }'.

The parentheses are optional after the keyword, which is the whole point of
the form; the body is otherwise the same compound command."
  (advance p)                           ; `function'
  (let ((name (cur-text p)))
    (advance p)                         ; name
    (when (eq (cur-type p) :lparen)     ; optional ()
      (advance p)
      (expect p :rparen))
    (skip-newlines p)
    (let ((body (parse-command p)))
      (make-function-def name body (parse-redirect-list p)))))

(defun parse-function-def (p)
  (let ((name (cur-text p)))
    (advance p)                         ; name
    (expect p :lparen)
    (expect p :rparen)
    (skip-newlines p)
    (let ((body (parse-command p)))     ; must be a compound command per grammar
      (make-function-def name body (parse-redirect-list p)))))

;;; ---------------------------------------------------------------------------
;;; Public entry points
;;; ---------------------------------------------------------------------------

(defun parse-string (string)
  "Parse STRING as a shell program.

Returns (values complete-commands incomplete-p). INCOMPLETE-P is true when a
here-document body ran out of input before its delimiter: the AST is still
usable (POSIX makes that a warning, not an error), but a reader feeding the
parser line by line must keep reading rather than treat this as a command.
Without it, `cat <<EOF\' on its own parses as a complete command and the
here-doc body is then executed as shell source."
  (let ((p (make-parser string)))
    (let ((program (parse-program p)))
      (unless (eq (cur-type p) :eof)
        (perr p "unexpected trailing token ~A ~S" (cur-type p) (cur-text p)))
      (values program (lexer-heredoc-eof (parser-lexer p))))))

(defun parse-stream (stream)
  "Parse all text from STREAM."
  (let ((s (make-string-output-stream)))
    (loop for line = (read-line stream nil :eof)
          until (eq line :eof)
          do (write-line line s))
    (parse-string (get-output-stream-string s))))
