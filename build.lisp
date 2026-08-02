;;;; build.lisp --- produce a standalone `bin/sxsh` executable.
;;;;
;;;;   sbcl --non-interactive --load build.lisp
;;;;
;;;; :save-runtime-options t is essential: without it the SBCL runtime parses
;;;; argv itself and would swallow shell arguments such as `--eval` or `--end-
;;;; runtime-options` before main ever sees them.

(require :asdf)
(asdf:load-system "sxsh/shell")

(defun sxsh-toplevel ()
  ;; Flush explicitly, and swallow a failure. The last write of a coprocess
  ;; whose reader has gone away fails HERE, after MAIN has returned, so no
  ;; handler inside the shell can see it -- and SBCL would print its own
  ;; stream error on the way out. Every other program dies silently on
  ;; SIGPIPE; a shell must too.
  (let ((code (handler-case (sxsh-shell:main)
                       (sb-sys:interactive-interrupt () 130)
                       ;; A write to a closed pipe. Every other program dies
                       ;; silently on SIGPIPE here, and a shell must too:
                       ;; `sxsh -c "..." | head -1' otherwise prints a Lisp
                       ;; stream error after the reader has gone away. SBCL
                       ;; ignores SIGPIPE and reinstates that at startup, so
                       ;; resetting the disposition before exec does not reach
                       ;; a child that is itself an sxsh -- it has to be caught
                       ;; here. 141 is 128 + SIGPIPE, the status a real
                       ;; SIGPIPE death reports.
                       (stream-error (e)
                         (if (member (stream-error-stream e)
                                     (list *standard-output* *error-output*))
                             141
                             (error e))))))
    (unless (ignore-errors (finish-output *standard-output*) t)
      (setf code 141))
    (ignore-errors (finish-output *error-output*))
    (sb-ext:exit :code code :abort t)))

(ensure-directories-exist "bin/")

(sb-ext:save-lisp-and-die
 "bin/sxsh"
 :executable t
 :save-runtime-options t
 :toplevel #'sxsh-toplevel)
