;;;; posh.asd --- POSIX shell grammar parser in Common Lisp

(asdf:defsystem "posh"
  :description "A parser for the POSIX shell command language (IEEE Std 1003.1, Section 2.10)."
  :author "Matthew"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "ast")
               (:file "conditions")
               (:file "lexer")
               (:file "parser"))
  :in-order-to ((asdf:test-op (asdf:test-op "posh/test"))))

(asdf:defsystem "posh/shell"
  :description "A POSIX shell executor built on the posh parser, using posix_spawn."
  ;; sb-posix is used pervasively from state.lisp onward (open/dup2/pipe/waitpid/
  ;; opendir/getpwnam/getrusage). Without this the first file fails to compile
  ;; with "Package SB-POSIX does not exist".
  :depends-on ("posh" (:require :sb-posix))
  :serial t
  :components ((:module "shell"
                :serial t
                :components ((:file "package")
                             (:file "state")
                             (:file "spawn")
                             (:file "arith")
                             (:file "expand")
                             (:file "redir")
                             (:file "deparse")
                             (:file "jobs")
                             (:file "builtins")
                             (:file "exec")
                             (:file "driver"))))
  :in-order-to ((asdf:test-op (asdf:test-op "posh/shell/test"))))

(asdf:defsystem "posh/test"
  :description "Parser test suite."
  :depends-on ("posh")
  :serial t
  :components ((:module "test"
                :components ((:file "tests"))))
  :perform (asdf:test-op (o c)
             (declare (ignore o c))
             (multiple-value-bind (pass fail)
                 (uiop:symbol-call :posh/test :run-all)
               (declare (ignore pass))
               (unless (zerop fail)
                 (error "posh parser tests: ~D failed" fail)))))

(asdf:defsystem "posh/shell/test"
  :description "Executor test suite."
  :depends-on ("posh/shell")
  :serial t
  :components ((:module "shell"
                :components ((:file "test-shell"))))
  :perform (asdf:test-op (o c)
             (declare (ignore o c))
             (multiple-value-bind (pass fail)
                 (uiop:symbol-call :posh-shell/test :run-all)
               (declare (ignore pass))
               (unless (zerop fail)
                 (error "posh executor tests: ~D failed" fail)))))
