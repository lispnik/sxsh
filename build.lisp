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
  (sb-ext:exit :code (handler-case (sxsh-shell:main)
                       (sb-sys:interactive-interrupt () 130))
               :abort t))

(ensure-directories-exist "bin/")

(sb-ext:save-lisp-and-die
 "bin/sxsh"
 :executable t
 :save-runtime-options t
 :toplevel #'sxsh-toplevel)
