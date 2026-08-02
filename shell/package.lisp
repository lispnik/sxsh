;;;; shell/package.lisp

(defpackage #:sxsh-shell
  (:use #:cl)
  (:nicknames #:sxs)
  (:import-from #:sxsh
                #:parse-string
                ;; ast accessors
                #:complete-command #:complete-command-entries
                #:and-or #:and-or-op #:and-or-left #:and-or-right
                #:pipeline #:pipeline-commands #:pipeline-bang #:pipeline-timed
                #:simple-command #:simple-command-assignments
                #:simple-command-words #:simple-command-redirects
                #:assignment #:assignment-name #:assignment-value
                #:assignment-append
                #:cond-expr #:cond-expr-text #:cond-expr-redirects
                #:arith-command #:arith-command-expr #:arith-command-redirects
                #:arith-for #:arith-for-init #:arith-for-cond #:arith-for-step
                #:arith-for-body #:arith-for-redirects
                #:redirect #:redirect-op #:redirect-fd #:redirect-target
                #:redirect-heredoc
                #:subshell #:subshell-body #:subshell-redirects
                #:brace-group #:brace-group-body #:brace-group-redirects
                #:if-clause #:if-clause-condition #:if-clause-then #:if-clause-else
                #:if-clause-redirects
                #:for-clause #:for-clause-name #:for-clause-words #:for-clause-body
                #:for-clause-redirects
                #:while-clause #:while-clause-condition #:while-clause-body
                #:while-clause-redirects
                #:until-clause #:until-clause-condition #:until-clause-body
                #:until-clause-redirects
                #:case-clause #:case-clause-word #:case-clause-items
                #:case-clause-redirects
                #:case-item #:case-item-patterns #:case-item-body #:case-item-terminator
                #:function-def #:function-def-name #:function-def-body
                #:function-def-redirects
                #:word #:word-text
                #:node-line)
  (:export #:make-shell #:shell #:run #:run-string #:repl #:main
           #:shell-last-status #:spawn))

(in-package #:sxsh-shell)

;; struct predicate helpers used by the executor dispatch
(defun ast-type (node)
  (typecase node
    (complete-command :list)
    (and-or :and-or)
    (pipeline :pipeline)
    (simple-command :simple)
    (subshell :subshell)
    (brace-group :brace)
    (if-clause :if)
    (for-clause :for)
    (while-clause :while)
    (until-clause :until)
    (case-clause :case)
    (function-def :func)
    (arith-command :arith)
    (cond-expr :cond)
    (arith-for :arith-for)
    (t (error "unknown AST node ~S" node))))
