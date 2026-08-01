;;;; shell/deparse.lisp --- turn a parsed AST back into shell source.
;;;;
;;;; Used for job control: to run a compound command (brace group, subshell,
;;;; loop, ...) as a genuine asynchronous job in its own process, we re-exec
;;;; the posh binary with `-c <source>`. Words retain their raw source text
;;;; from parsing, so the reconstruction is exact for the parts that matter
;;;; (quoting, expansions); only structural tokens are re-synthesized.

(in-package #:posh-shell)

(defun deparse (node)
  "Return shell source text equivalent to NODE."
  (with-output-to-string (s) (dp node s)))

(defgeneric dp (node stream))

(defmethod dp ((n word) s) (write-string (word-text n) s))

(defmethod dp ((n assignment) s)
  (write-string (assignment-name n) s)
  (write-char #\= s)
  (when (assignment-value n) (dp (assignment-value n) s)))

(defmethod dp ((n redirect) s)
  (when (redirect-fd n) (format s "~D" (redirect-fd n)))
  (write-string (ecase (redirect-op n)
                  (:< "<") (:> ">") (:>> ">>") (:<< "<<") (:<<- "<<-")
                  (:<& "<&") (:>& ">&") (:<> "<>") (:>\| ">|"))
                s)
  (dp (redirect-target n) s)
  ;; here-doc bodies: emit inline with a delimiter already present in target
  (when (and (member (redirect-op n) '(:<< :<<-)) (redirect-heredoc n))
    (destructuring-bind (delim body quoted strip) (redirect-heredoc n)
      (declare (ignore quoted strip))
      (format s "~%~A~A" body delim))))

(defun dp-list (items s &optional (sep " "))
  (loop for x in items for first = t then nil
        do (unless first (write-string sep s)) (dp x s)))

(defmethod dp ((n simple-command) s)
  (let ((parts (append (simple-command-assignments n)
                       (simple-command-words n)
                       (simple-command-redirects n))))
    (dp-list parts s)))

(defmethod dp ((n pipeline) s)
  (when (pipeline-timed n)
    (write-string "time " s)
    (when (eq (pipeline-timed n) :time-p) (write-string "-p " s)))
  (when (pipeline-bang n) (write-string "! " s))
  (loop for c in (pipeline-commands n) for first = t then nil
        do (unless first (write-string " | " s)) (dp c s)))

(defmethod dp ((n and-or) s)
  (dp (and-or-left n) s)
  (write-string (ecase (and-or-op n) (:&& " && ") (:\|\| " || ")) s)
  (dp (and-or-right n) s))

(defmethod dp ((n complete-command) s)
  (loop for (child . sep) in (complete-command-entries n)
        do (dp child s)
           (write-string (case sep (:async " & ") (t "; ")) s)))

(defmethod dp ((n subshell) s)
  (write-char #\( s) (dp (subshell-body n) s) (write-char #\) s)
  (dp-redirs (subshell-redirects n) s))

(defmethod dp ((n brace-group) s)
  (write-string "{ " s) (dp (brace-group-body n) s) (write-string " }" s)
  (dp-redirs (brace-group-redirects n) s))

(defun dp-redirs (redirs s)
  (dolist (r redirs) (write-char #\Space s) (dp r s)))

(defmethod dp ((n if-clause) s)
  (write-string "if " s) (dp (if-clause-condition n) s)
  (write-string " then " s) (dp (if-clause-then n) s)
  (let ((else (if-clause-else n)))
    (when else
      (if (if-clause-p else)
          (progn (write-string " el" s)  ; elif: render nested if without leading 'if'
                 (dp-elif else s))
          (progn (write-string " else " s) (dp else s)))))
  (write-string " fi" s)
  (dp-redirs (if-clause-redirects n) s))

(defun if-clause-p (x) (typep x 'if-clause))

(defun dp-elif (n s)
  (write-string "if " s) (dp (if-clause-condition n) s)
  (write-string " then " s) (dp (if-clause-then n) s)
  (let ((else (if-clause-else n)))
    (when else
      (if (if-clause-p else)
          (progn (write-string " el" s) (dp-elif else s))
          (progn (write-string " else " s) (dp else s))))))

(defmethod dp ((n while-clause) s)
  (write-string "while " s) (dp (while-clause-condition n) s)
  (write-string " do " s) (dp (while-clause-body n) s) (write-string " done" s)
  (dp-redirs (while-clause-redirects n) s))

(defmethod dp ((n until-clause) s)
  (write-string "until " s) (dp (until-clause-condition n) s)
  (write-string " do " s) (dp (until-clause-body n) s) (write-string " done" s)
  (dp-redirs (until-clause-redirects n) s))

(defmethod dp ((n for-clause) s)
  (format s "for ~A" (for-clause-name n))
  (unless (eq (for-clause-words n) :default)
    (write-string " in " s) (dp-list (for-clause-words n) s))
  (write-string "; do " s) (dp (for-clause-body n) s) (write-string " done" s)
  (dp-redirs (for-clause-redirects n) s))

(defmethod dp ((n case-clause) s)
  (write-string "case " s) (dp (case-clause-word n) s) (write-string " in " s)
  (dolist (item (case-clause-items n))
    (dp-list (case-item-patterns item) s "|")
    (write-string ") " s)
    (when (case-item-body item) (dp (case-item-body item) s))
    (write-string " ;; " s))
  (write-string "esac" s)
  (dp-redirs (case-clause-redirects n) s))

(defmethod dp ((n function-def) s)
  (format s "~A() " (function-def-name n))
  (dp (function-def-body n) s)
  (dp-redirs (function-def-redirects n) s))
