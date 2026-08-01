;;;; test/tests.lisp

(defpackage #:posh/test
  (:use #:cl #:posh)
  (:export #:run-all #:sx))

(in-package #:posh/test)

;;; Render an AST node as a compact s-expression for structural comparison.
(defgeneric sx (node))

(defmethod sx ((n null)) nil)

(defmethod sx ((n list)) (mapcar #'sx n))

(defmethod sx ((n posh::word)) (list :w (posh::word-text n)))

(defmethod sx ((n posh::assignment))
  (list :assign (posh::assignment-name n) (sx (posh::assignment-value n))))

(defmethod sx ((n posh::redirect))
  (append (list :redir (posh::redirect-op n))
          (when (posh::redirect-fd n) (list :fd (posh::redirect-fd n)))
          (list (sx (posh::redirect-target n)))
          (when (posh::redirect-heredoc n)
            (list :heredoc (second (posh::redirect-heredoc n))))))

(defmethod sx ((n posh::simple-command))
  (list* :cmd
         (append
          (when (posh::simple-command-assignments n)
            (list (cons :assigns (sx (posh::simple-command-assignments n)))))
          (when (posh::simple-command-words n)
            (list (cons :words (sx (posh::simple-command-words n)))))
          (when (posh::simple-command-redirects n)
            (list (cons :redirs (sx (posh::simple-command-redirects n))))))))

(defmethod sx ((n posh::pipeline))
  (list* :pipe (if (posh::pipeline-bang n) '(:!) nil)
         (mapcar #'sx (posh::pipeline-commands n))))

(defmethod sx ((n posh::and-or))
  (list (posh::and-or-op n) (sx (posh::and-or-left n)) (sx (posh::and-or-right n))))

(defmethod sx ((n posh::complete-command))
  (list* :list
         (mapcar (lambda (e) (if (cdr e) (list (sx (car e)) (cdr e)) (sx (car e))))
                 (posh::complete-command-entries n))))

(defmethod sx ((n posh::subshell))
  (list :subshell (sx (posh::subshell-body n))
        (sx (posh::subshell-redirects n))))

(defmethod sx ((n posh::brace-group))
  (list :brace (sx (posh::brace-group-body n))
        (sx (posh::brace-group-redirects n))))

(defmethod sx ((n posh::if-clause))
  (list :if (sx (posh::if-clause-condition n))
        (sx (posh::if-clause-then n))
        (sx (posh::if-clause-else n))))

(defmethod sx ((n posh::for-clause))
  (list :for (posh::for-clause-name n)
        (if (eq (posh::for-clause-words n) :default)
            :default (sx (posh::for-clause-words n)))
        (sx (posh::for-clause-body n))))

(defmethod sx ((n posh::while-clause))
  (list :while (sx (posh::while-clause-condition n)) (sx (posh::while-clause-body n))))

(defmethod sx ((n posh::until-clause))
  (list :until (sx (posh::until-clause-condition n)) (sx (posh::until-clause-body n))))

(defmethod sx ((n posh::case-clause))
  (list :case (sx (posh::case-clause-word n))
        (mapcar #'sx (posh::case-clause-items n))))

(defmethod sx ((n posh::case-item))
  (list :item (sx (posh::case-item-patterns n))
        (sx (posh::case-item-body n))
        (posh::case-item-terminator n)))

(defmethod sx ((n posh::function-def))
  (list :func (posh::function-def-name n) (sx (posh::function-def-body n))))

;;; ---------------------------------------------------------------------------

(defvar *pass* 0)
(defvar *fail* 0)

(defun p1 (src)
  "Parse a single complete-command and return its sx."
  (let ((prog (parse-string src)))
    (sx (first prog))))

(defmacro check (name src expected)
  `(let ((got (handler-case (p1 ,src)
                (error (e) (list :error (princ-to-string e))))))
     (if (equal got ,expected)
         (progn (incf *pass*))
         (progn (incf *fail*)
                (format t "~&FAIL ~A~%  src: ~S~%  want: ~S~%  got:  ~S~%"
                        ,name ,src ,expected got)))))

(defmacro check-error (name src)
  `(let ((got (handler-case (progn (parse-string ,src) :no-error)
                (shell-parse-error () :error)
                (error () :error))))
     (if (eq got :error)
         (incf *pass*)
         (progn (incf *fail*)
                (format t "~&FAIL ~A (expected parse error)~%  src: ~S~%" ,name ,src)))))

(defun run-all ()
  (setf *pass* 0 *fail* 0)

  ;; simple commands
  (check "bare" "echo hello"
         '(:list (:cmd (:words (:w "echo") (:w "hello")))))
  (check "args" "ls -l /tmp"
         '(:list (:cmd (:words (:w "ls") (:w "-l") (:w "/tmp")))))

  ;; assignments
  (check "assign-prefix" "FOO=bar echo hi"
         '(:list (:cmd (:assigns (:assign "FOO" (:w "bar")))
                       (:words (:w "echo") (:w "hi")))))
  (check "assign-only" "X=1 Y=2"
         '(:list (:cmd (:assigns (:assign "X" (:w "1")) (:assign "Y" (:w "2"))))))
  (check "assign-empty" "EMPTY="
         '(:list (:cmd (:assigns (:assign "EMPTY" nil)))))

  ;; quotes stay verbatim in the word
  (check "single-quote" "echo 'a b c'"
         '(:list (:cmd (:words (:w "echo") (:w "'a b c'")))))
  (check "double-quote" "echo \"a $x b\""
         '(:list (:cmd (:words (:w "echo") (:w "\"a $x b\"")))))
  (check "cmdsub" "echo $(date -u)"
         '(:list (:cmd (:words (:w "echo") (:w "$(date -u)")))))
  (check "backtick" "echo `whoami`"
         '(:list (:cmd (:words (:w "echo") (:w "`whoami`")))))
  (check "paramexp" "echo ${HOME:-/root}"
         '(:list (:cmd (:words (:w "echo") (:w "${HOME:-/root}")))))
  (check "nested-sub" "echo $(echo $(echo x))"
         '(:list (:cmd (:words (:w "echo") (:w "$(echo $(echo x))")))))

  ;; pipelines
  (check "pipe" "ls | wc -l"
         '(:list (:pipe nil
                  (:cmd (:words (:w "ls")))
                  (:cmd (:words (:w "wc") (:w "-l"))))))
  (check "bang-pipe" "! grep x file"
         '(:list (:pipe (:!) (:cmd (:words (:w "grep") (:w "x") (:w "file"))))))

  ;; and-or, left assoc
  (check "and" "a && b"
         '(:list (:&& (:cmd (:words (:w "a"))) (:cmd (:words (:w "b"))))))
  (check "and-or-chain" "a && b || c"
         '(:list (:\|\| (:&& (:cmd (:words (:w "a"))) (:cmd (:words (:w "b"))))
                  (:cmd (:words (:w "c"))))))

  ;; lists and separators
  (check "semi" "a; b"
         '(:list ((:cmd (:words (:w "a"))) :semi) (:cmd (:words (:w "b")))))
  (check "async" "sleep 1 &"
         '(:list ((:cmd (:words (:w "sleep") (:w "1"))) :async)))

  ;; redirections
  (check "redir-out" "echo hi > file"
         '(:list (:cmd (:words (:w "echo") (:w "hi"))
                       (:redirs (:redir :> (:w "file"))))))
  (check "redir-append" "echo hi >> log"
         '(:list (:cmd (:words (:w "echo") (:w "hi"))
                       (:redirs (:redir :>> (:w "log"))))))
  (check "redir-fd" "cmd 2> err"
         '(:list (:cmd (:words (:w "cmd"))
                       (:redirs (:redir :> :fd 2 (:w "err"))))))
  (check "redir-dup" "cmd 2>&1"
         '(:list (:cmd (:words (:w "cmd"))
                       (:redirs (:redir :>& :fd 2 (:w "1"))))))
  (check "redir-in" "wc < file"
         '(:list (:cmd (:words (:w "wc")) (:redirs (:redir :< (:w "file"))))))
  (check "redir-prefix" "> out echo hi"
         '(:list (:cmd (:words (:w "echo") (:w "hi"))
                       (:redirs (:redir :> (:w "out"))))))

  ;; subshell & brace group
  (check "subshell" "(cd /tmp; ls)"
         '(:list (:subshell
                  (:list ((:cmd (:words (:w "cd") (:w "/tmp"))) :semi)
                         (:cmd (:words (:w "ls"))))
                  nil)))
  (check "brace" "{ echo a; echo b; }"
         '(:list (:brace
                  (:list ((:cmd (:words (:w "echo") (:w "a"))) :semi)
                         ((:cmd (:words (:w "echo") (:w "b"))) :semi))
                  nil)))

  ;; if
  (check "if" "if true; then echo y; fi"
         '(:list (:if (:list ((:cmd (:words (:w "true"))) :semi))
                  (:list ((:cmd (:words (:w "echo") (:w "y"))) :semi))
                  nil)))
  (check "if-else" "if a; then b; else c; fi"
         '(:list (:if (:list ((:cmd (:words (:w "a"))) :semi))
                  (:list ((:cmd (:words (:w "b"))) :semi))
                  (:list ((:cmd (:words (:w "c"))) :semi)))))
  (check "if-elif" "if a; then b; elif c; then d; fi"
         '(:list (:if (:list ((:cmd (:words (:w "a"))) :semi))
                  (:list ((:cmd (:words (:w "b"))) :semi))
                  (:if (:list ((:cmd (:words (:w "c"))) :semi))
                   (:list ((:cmd (:words (:w "d"))) :semi))
                   nil))))

  ;; loops
  (check "while" "while read x; do echo $x; done"
         '(:list (:while (:list ((:cmd (:words (:w "read") (:w "x"))) :semi))
                  (:list ((:cmd (:words (:w "echo") (:w "$x"))) :semi)))))
  (check "until" "until false; do work; done"
         '(:list (:until (:list ((:cmd (:words (:w "false"))) :semi))
                  (:list ((:cmd (:words (:w "work"))) :semi)))))
  (check "for-in" "for i in 1 2 3; do echo $i; done"
         '(:list (:for "i" ((:w "1") (:w "2") (:w "3"))
                  (:list ((:cmd (:words (:w "echo") (:w "$i"))) :semi)))))
  (check "for-default" "for i; do echo $i; done"
         '(:list (:for "i" :default
                  (:list ((:cmd (:words (:w "echo") (:w "$i"))) :semi)))))

  ;; case
  (check "case" "case $x in a) echo A;; b|c) echo BC;; esac"
         '(:list (:case (:w "$x")
                  ((:item ((:w "a")) (:list (:cmd (:words (:w "echo") (:w "A")))) :\;\;)
                   (:item ((:w "b") (:w "c"))
                          (:list (:cmd (:words (:w "echo") (:w "BC")))) :\;\;)))))
  (check "case-esac-empty" "case $x in *) ;; esac"
         '(:list (:case (:w "$x") ((:item ((:w "*")) nil :\;\;)))))

  ;; function definition
  (check "func" "greet() { echo hi; }"
         '(:list (:func "greet"
                  (:brace (:list ((:cmd (:words (:w "echo") (:w "hi"))) :semi)) nil))))

  ;; here-doc
  (check "heredoc" (format nil "cat <<EOF~%line one~%line two~%EOF~%")
         '(:list (:cmd (:words (:w "cat"))
                       (:redirs (:redir :<< (:w "EOF")
                                 :heredoc "line one
line two
")))))
  (check "heredoc-strip" (format nil "cat <<-END~%~Cindented~%~CEND~%" #\Tab #\Tab)
         '(:list (:cmd (:words (:w "cat"))
                       (:redirs (:redir :<<- (:w "END") :heredoc "indented
")))))

  ;; newlines as separators inside compound
  (check "multiline-if" (format nil "if true~%then~%  echo ok~%fi")
         '(:list (:if (:list ((:cmd (:words (:w "true"))) :semi))
                  (:list ((:cmd (:words (:w "echo") (:w "ok"))) :semi))
                  nil)))

  ;; comments
  (check "comment" (format nil "echo hi # a comment~%")
         '(:list (:cmd (:words (:w "echo") (:w "hi")))))

  ;; line continuation
  (check "continuation" (format nil "echo a\\~%b")
         '(:list (:cmd (:words (:w "echo") (:w "ab")))))

  ;; reserved word as argument is ordinary
  (check "reserved-as-arg" "echo if then fi"
         '(:list (:cmd (:words (:w "echo") (:w "if") (:w "then") (:w "fi")))))

  ;; nested compound
  (check "pipe-of-compound" "if a; then b; fi | cat"
         '(:list (:pipe nil
                  (:if (:list ((:cmd (:words (:w "a"))) :semi))
                   (:list ((:cmd (:words (:w "b"))) :semi)) nil)
                  (:cmd (:words (:w "cat"))))))

  ;; compound with trailing redirect
  (check "while-redir" "while read l; do echo $l; done < input"
         '(:list (:while (:list ((:cmd (:words (:w "read") (:w "l"))) :semi))
                  (:list ((:cmd (:words (:w "echo") (:w "$l"))) :semi)))))

  ;; error cases
  (check-error "unterminated-quote" "echo 'oops")
  (check-error "missing-fi" "if a; then b")
  (check-error "missing-done" "while a; do b")
  (check-error "unbalanced-paren" "(a; b")
  (check-error "empty" "&& b")

  (format t "~&~%~D passed, ~D failed~%" *pass* *fail*)
  (values *pass* *fail*))
