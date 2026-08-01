;;;; conditions.lisp

(in-package #:sxsh)

(define-condition shell-parse-error (error)
  ((message :initarg :message :reader shell-parse-error-message)
   (line    :initarg :line    :reader shell-parse-error-line    :initform nil)
   (column  :initarg :column  :reader shell-parse-error-column  :initform nil))
  (:report (lambda (c stream)
             (format stream "Shell parse error~@[ at line ~D~]~@[, column ~D~]: ~A"
                     (shell-parse-error-line c)
                     (shell-parse-error-column c)
                     (shell-parse-error-message c)))))

(defun parse-error* (line column format &rest args)
  (error 'shell-parse-error
         :line line :column column
         :message (apply #'format nil format args)))
