;;;; ast.lisp --- Abstract syntax tree node definitions.
;;;;
;;;; Node hierarchy mirrors the POSIX shell grammar nonterminals fairly
;;;; closely, collapsed where the distinctions are purely syntactic.

(in-package #:sxsh)

(defstruct (node (:constructor nil))
  ;; Source line this node started on, stamped by the parser. Every
  ;; constructor is BOA, so this stays at 0 unless someone sets it; the
  ;; executor uses it to maintain $LINENO.
  (line 0 :type fixnum))

(defun node-type (node)
  "Return a keyword naming the kind of NODE (:pipeline, :if-clause, ...)."
  (intern (string-upcase (subseq (string (type-of node)) 0)) :keyword))

;;; A word: an expanded-later token plus its raw text. We keep the text
;;; verbatim; expansion (parameter, command, arithmetic, field splitting)
;;; is an execution concern, not a parsing one.
(defstruct (word (:include node) (:constructor make-word (text)))
  (text "" :type string))

;;; name=value assignment prefix
(defstruct (assignment (:include node)
                       (:constructor make-assignment (name value &optional append)))
  (name "" :type string)
  value                                 ; a WORD or NIL for `name=`
  append)                               ; bash `name+=value': append, not replace

;;; Redirection. OP is a keyword: :< :> :>> :<< :<<- :<& :>& :<> :>|
;;; FD is the optional left-hand file descriptor (integer) or NIL.
;;; TARGET is a WORD (filename or fd word). HEREDOC holds delimiter/body.
(defstruct (redirect (:include node) (:constructor make-redirect (op fd target)))
  op
  fd
  target
  heredoc)                              ; (delimiter body quoted-p strip-p) once collected

;;; bash `select NAME in WORDS; do ... done' -- menu loop
(defstruct (select-clause (:include node)
                          (:constructor make-select-clause (name words body redirects)))
  name words body redirects)

;;; bash `[[ ... ]]' -- conditional expression, kept as raw text and parsed
;;; at execution time, where expansion happens
(defstruct (cond-expr (:include node)
                      (:constructor make-cond-expr (text redirects)))
  (text "" :type string)
  redirects)

;;; bash `((expr))' -- evaluate arithmetic, status 0 if non-zero
(defstruct (arith-command (:include node)
                          (:constructor make-arith-command (expr redirects)))
  (expr "" :type string)
  redirects)

;;; bash `for ((init; cond; step)) do ... done'
(defstruct (arith-for (:include node)
                      (:constructor make-arith-for (init cond step body redirects)))
  init cond step body redirects)

;;; simple command: assignments + words + redirects, any of which may be empty
(defstruct (simple-command (:include node)
                           (:constructor make-simple-command (assignments words redirects)))
  assignments
  words
  redirects)

;;; pipeline: a list of commands joined by |, with optional leading !
(defstruct (pipeline (:include node) (:constructor make-pipeline (commands bang &optional timed)))
  commands                              ; list of command nodes
  bang                                  ; T if preceded by `!`
  timed)                                ; :time / :time-p when preceded by `time [-p]`

;;; and-or list: left OP right, OP being :&& or :||. Left-associative.
(defstruct (and-or (:include node) (:constructor make-and-or (op left right)))
  op left right)

;;; complete command: list of and-or lists with their separators
;;; entries are (node . separator) where separator is :\; or :& or NIL (last)
(defstruct (complete-command (:include node) (:constructor make-complete-command (entries)))
  entries)

;;; ( compound-list )
(defstruct (subshell (:include node) (:constructor make-subshell (body redirects)))
  body redirects)

;;; { compound-list }
(defstruct (brace-group (:include node) (:constructor make-brace-group (body redirects)))
  body redirects)

;;; if / elif / else
(defstruct (if-clause (:include node) (:constructor make-if-clause (condition then else redirects)))
  condition then else redirects)

(defstruct (for-clause (:include node)
                       (:constructor make-for-clause (name words body redirects)))
  name
  words                                 ; NIL means "in "$@"" default; :default marks absence of `in`
  body redirects)

(defstruct (while-clause (:include node)
                         (:constructor make-while-clause (condition body redirects)))
  condition body redirects)

(defstruct (until-clause (:include node)
                         (:constructor make-until-clause (condition body redirects)))
  condition body redirects)

(defstruct (case-clause (:include node) (:constructor make-case-clause (word items redirects)))
  word items redirects)

(defstruct (case-item (:include node) (:constructor make-case-item (patterns body terminator)))
  patterns                              ; list of WORD
  body                                  ; compound-list or NIL
  terminator)                           ; :\;\; :\;& :\;\;&  or NIL

(defstruct (function-def (:include node)
                         (:constructor make-function-def (name body redirects)))
  name body redirects)
