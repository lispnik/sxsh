;;;; sxsh.asd --- POSIX shell grammar parser in Common Lisp

(asdf:defsystem "sxsh"
  :description "A parser for the POSIX shell command language (IEEE Std 1003.1, Section 2.10)."
  :author "Matthew"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "ast")
               (:file "conditions")
               (:file "lexer")
               (:file "parser"))
  :in-order-to ((asdf:test-op (asdf:test-op "sxsh/test"))))

(asdf:defsystem "sxsh/shell"
  :description "A POSIX shell executor built on the sxsh parser, using posix_spawn."
  ;; sb-posix is used pervasively from state.lisp onward (open/dup2/pipe/waitpid/
  ;; opendir/getpwnam/getrusage). Without this the first file fails to compile
  ;; with "Package SB-POSIX does not exist".
  :depends-on ("sxsh" (:require :sb-posix))
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
  :in-order-to ((asdf:test-op (asdf:test-op "sxsh/shell/test"))))

(asdf:defsystem "sxsh/test"
  :description "Parser test suite."
  :depends-on ("sxsh")
  :serial t
  :components ((:module "test"
                :components ((:file "tests"))))
  :perform (asdf:test-op (o c)
             (declare (ignore o c))
             (multiple-value-bind (pass fail)
                 (uiop:symbol-call :sxsh/test :run-all)
               (declare (ignore pass))
               (unless (zerop fail)
                 (error "sxsh parser tests: ~D failed" fail)))))

(asdf:defsystem "sxsh/shell/test"
  :description "Executor test suite."
  :depends-on ("sxsh/shell")
  :serial t
  :components ((:module "shell"
                :components ((:file "test-shell"))))
  :perform (asdf:test-op (o c)
             (declare (ignore o c))
             (multiple-value-bind (pass fail)
                 (uiop:symbol-call :sxsh-shell/test :run-all)
               (declare (ignore pass))
               (unless (zerop fail)
                 (error "sxsh executor tests: ~D failed" fail)))))
