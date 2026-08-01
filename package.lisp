;;;; package.lisp

(defpackage #:sxsh
  (:use #:cl)
  (:export
   ;; entry points
   #:parse-string
   #:parse-stream
   ;; conditions
   #:shell-parse-error
   #:shell-parse-error-message
   #:shell-parse-error-line
   #:shell-parse-error-column
   ;; lexer (exposed for testing / REPL use)
   #:tokenize
   #:make-lexer
   #:next-token
   #:token
   #:token-type
   #:token-text
   #:token-line
   #:token-column
   ;; AST node accessors -- see ast.lisp for the full set
   #:node-type
   #:node-line
   #:complete-command
   #:and-or #:and-or-left #:and-or-right #:and-or-op
   #:pipeline #:pipeline-commands #:pipeline-bang #:pipeline-timed
   #:simple-command #:simple-command-assignments
   #:simple-command-words #:simple-command-redirects
   #:assignment #:assignment-name #:assignment-value
   #:redirect #:redirect-op #:redirect-fd #:redirect-target #:redirect-heredoc
   #:subshell #:subshell-body #:subshell-redirects
   #:brace-group #:brace-group-body #:brace-group-redirects
   #:if-clause #:if-clause-condition #:if-clause-then #:if-clause-else
   #:if-clause-redirects
   #:for-clause #:for-clause-name #:for-clause-words #:for-clause-body
   #:for-clause-redirects
   #:while-clause #:while-clause-condition #:while-clause-body #:while-clause-redirects
   #:until-clause #:until-clause-condition #:until-clause-body #:until-clause-redirects
   #:case-clause #:case-clause-word #:case-clause-items #:case-clause-redirects
   #:case-item #:case-item-patterns #:case-item-body #:case-item-terminator
   #:function-def #:function-def-name #:function-def-body #:function-def-redirects
   #:word #:word-text))
