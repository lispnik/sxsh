;;;; shell/driver.lisp --- top-level drivers and REPL.

(in-package #:sxsh-shell)

(defun run-string-capturing (sh src out)
  "Parse and run SRC in the CURRENT execution environment, sending command
output to the current fd 1 (already set up by the caller). OUT is the builtin's
stream; we finish it first so ordering with child output is sane. Returns the
last status.

Control flow is deliberately not caught here. `eval' and `.' run their input
inline, not in a subshell, so `exit', `return', `break' and `continue' have to
reach the enclosing shell, function or loop -- bash, zsh and dash agree on all
four. That rules out RUN, which catches SHELL-EXIT and FUNC-RETURN as a
top-level backstop: routing through it made `eval \"exit 0\"' a no-op, and
libtool's --config and --version, which end in a function reached via `eval',
ran on past their `exit' into a usage error. The `.' builtin adds back a
FUNC-RETURN handler of its own, since `return' ends the sourced script rather
than the caller."
  (declare (ignore out))
  (let ((nodes (handler-case (parse-string src)
                 (sxsh:shell-parse-error (e)
                   (format *error-output* "sxsh: ~A~%" e)
                   (return-from run-string-capturing 2)))))
    (dolist (node nodes (shell-last-status sh))
      (exec-node sh node))))

(defun slurp-file (path)
  "Read an entire file into a string. Works for non-seekable files such as
/dev/stdin and pipes, where file-length is unavailable."
  (with-open-file (s path :element-type 'character)
    (let ((buf (make-string-output-stream)))
      (loop with chunk = (make-string 4096)
            for n = (read-sequence chunk s)
            do (write-string chunk buf :end n)
            while (= n (length chunk)))
      (get-output-stream-string buf))))

(defun run-file (sh path)
  (run-string sh (slurp-file path)))

(defun stdin-is-interactive-p ()
  "POSIX 2.5.3: a shell invoked with no operands is interactive only when both
standard input and standard error are attached to a terminal.

Deciding this from `no arguments' alone -- as this used to -- makes
`sxsh < script' and `cmd | sxsh' print PS1 prompts into their own output and
switch on job control. Every such invocation produced `$ ' noise interleaved
with the script's real output."
  (and (plusp (sb-unix:unix-isatty 0))
       (plusp (sb-unix:unix-isatty 2))))

(defun repl (sh)
  "Read-eval-print loop. Prompts only when the shell is interactive; otherwise
this is simply the reader for a script arriving on standard input."
  (init-job-control sh)
  (loop
    (when (shell-interactive sh)
      ;; report any background jobs that finished since the last prompt
      (poll-jobs sh)
      (notify-finished-jobs sh *error-output*)
      (let ((ps1 (or (nth-value 0 (get-var sh "PS1")) "$ ")))
        (write-string ps1) (finish-output)))
    (let ((line (read-complete-command *standard-input*)))
      (when (null line)
        ;; the newline moves the terminal past a Ctrl-D; on a piped script it
        ;; would be a spurious blank line appended to the program's output
        (when (shell-interactive sh) (terpri))
        (return))
      (unless (string= (string-trim '(#\Space #\Tab #\Newline) line) "")
        (handler-case
            (run-string sh line)
          (sxsh:shell-parse-error (e)
            (format *error-output* "sxsh: ~A~%" e))
          (shell-exit (e)
            (return (or (shell-exit-code e) 0)))
          (error (e)
            (format *error-output* "sxsh: ~A~%" e)))))))

(defun init-job-control (sh)
  "Apply the startup default for monitor mode: on for an interactive shell with
a controlling terminal, off otherwise (POSIX 2.11). `set -m` / `set +m` can
change it afterwards. Best-effort; silently degrades when there is no tty."
  (set-monitor sh (and (shell-interactive sh) t)))

(defun read-complete-command (stream)
  "Read lines until we have a parseable command (handles simple continuation
when a parse error is 'unexpected EOF'-like). Returns NIL on EOF."
  (let ((buffer (make-string-output-stream)) (got-any nil))
    (loop
      (let ((line (read-line stream nil :eof)))
        (when (eq line :eof)
          (return (if got-any (get-output-stream-string buffer) nil)))
        (setf got-any t)
        (write-line line buffer)
        (let ((src (get-output-stream-string buffer)))
          ;; try to parse; if it parses, return it; else keep reading
          (handler-case
              (multiple-value-bind (program incomplete) (parse-string src)
                (declare (ignore program))
                ;; A here-doc whose delimiter has not arrived yet parses fine
                ;; but is not a complete command; keep reading or the body
                ;; would be executed as shell source.
                (if incomplete
                    (write-string src buffer)
                    (return src)))
            (sxsh:shell-parse-error ()
              ;; incomplete -> keep buffering
              (write-string src buffer))
            (error () (return src)))))))) ; other errors: let run report

(defun supported-platform-p ()
  "sxsh/shell relies on POSIX (posix_spawn, dup2, pipe, waitpid). It runs on
Linux and macOS; anything else (notably Windows) is unsupported."
  (and (member :unix *features*) t))

(defun parse-shell-options (sh argv)
  "Consume leading command-line options, applying them to SH.

POSIX: sh [-abCefhimnuvx] [-o option] [+abCefhimnuvx] [+o option]
       [command_file [argument...]] | -c command_string [command_name ...]

Returns (values mode operand rest), where MODE is :command, :stdin or :file.
Without this, `sxsh -x script' took -x for the script's name -- the shell
accepted none of the set options on its command line."
  (let ((mode nil) (operand nil))
    (loop
      (let ((a (first argv)))
        (cond
          ((null a) (return))
          ((string= a "--") (pop argv) (return))
          ((string= a "-") (pop argv) (return))
          ((and (> (length a) 1) (member (char a 0) '(#\- #\+)))
           (let ((enable (char= (char a 0) #\-))
                 (letters (subseq a 1)))
             (pop argv)
             (loop for i from 0 below (length letters) do
               (let ((c (char letters i)))
                 (case c
                   (#\c (setf mode :command))
                   (#\s (setf mode :stdin))
                   (#\i (setf (shell-interactive sh) t))
                   (#\o (let ((name (pop argv)))
                          (cond
                            ((null name) (print-options sh *standard-output*
                                                        (not enable)))
                            (t (let ((entry (option-by-name name)))
                                 (unless entry
                                   (format *error-output*
                                           "sxsh: ~A: invalid option name~%" name)
                                   (return-from parse-shell-options
                                     (values :error nil nil)))
                                 (set-option sh (third entry) enable))))))
                   (t (let ((entry (option-by-letter c)))
                        (unless entry
                          (format *error-output* "sxsh: -~C: invalid option~%" c)
                          (return-from parse-shell-options (values :error nil nil)))
                        (set-option sh (third entry) enable))))))))
          (t (return)))))
    ;; what remains: the command string, or a script name, or nothing
    (case mode
      (:command (setf operand (pop argv)))
      (:stdin)
      (t (when argv (setf mode :file operand (pop argv)))))
    (values (or mode :stdin) operand argv)))

(defun main (&optional (argv (rest sb-ext:*posix-argv*)))
  "Entry point. Supports the POSIX sh synopsis: options, -c, a script, or an
interactive/stdin session."
  (unless (supported-platform-p)
    (format *error-output*
            "sxsh: unsupported platform; requires a POSIX system (Linux or macOS)~%")
    (return-from main 1))
  (let ((sh (make-shell :interactive (and (null argv)
                                          (stdin-is-interactive-p)))))
    (multiple-value-bind (mode operand rest) (parse-shell-options sh argv)
      (when (eq mode :error) (return-from main 2))
      (unwind-protect
           (handler-case
               (ecase mode
                 (:command
                  (unless operand
                    (format *error-output* "sxsh: -c: option requires an argument~%")
                    (return-from main 2))
                  ;; `sh -c cmd name args' sets $0 from the next operand
                  (when rest (setf (shell-name sh) (pop rest)))
                  (set-positional sh rest)
                  (run-string sh operand))
                 (:file
                  (setf (shell-name sh) operand)
                  (set-positional sh rest)
                  (let ((text (handler-case (slurp-file operand)
                                (error ()
                                  (format *error-output*
                                          "sxsh: ~A: No such file or directory~%"
                                          operand)
                                  (return-from main 127)))))
                    (run-string sh text)))
                 (:stdin
                  (set-positional sh rest)
                  (if (and (shell-interactive sh) (stdin-is-interactive-p))
                      (repl sh)
                      ;; A non-interactive shell reading a script from standard
                      ;; input parses the whole text at once, exactly as it would
                      ;; a file. Feeding it to the line-at-a-time reader instead
                      ;; makes constructs that span lines depend on the reader
                      ;; guessing where a command ends.
                      (run-string sh (slurp-file "/dev/stdin")))))
           (shell-exit (e) (setf (shell-last-status sh) (or (shell-exit-code e) 0)))
           (sxsh:shell-parse-error (e)
             (format *error-output* "sxsh: ~A~%" e)
             (setf (shell-last-status sh) 2))
           ;; A failed write -- in practice a closed pipe. Die quietly with
           ;; 141 (128 + SIGPIPE), as every other program does. Caught BEFORE
           ;; the generic handler below, which would otherwise print a Lisp
           ;; stream error after the reader has gone away: visible whenever
           ;; sxsh is a pipeline producer, and in every coprocess whose output
           ;; nobody reads. SBCL ignores SIGPIPE and reinstates that at
           ;; startup, so resetting the disposition before exec cannot help a
           ;; child that is itself an sxsh -- it has to be handled here.
           ;;
           ;; Deliberately NOT narrowed by comparing the condition's stream to
           ;; *standard-output*: RUN-BUILTIN rebinds that to a fresh fd-stream,
           ;; so the identity test failed and the message came out anyway. A
           ;; stream error reaching this point is an I/O failure on the
           ;; shell's own output either way, and no Lisp-level report of it
           ;; has ever helped anyone.
           (stream-error () (setf (shell-last-status sh) 141))
           (error (e)
             (format *error-output* "sxsh: ~A~%" e)
             (setf (shell-last-status sh) 1)))
        ;; POSIX: the EXIT trap runs when the shell terminates, whatever the
        ;; cause
        (run-exit-traps sh))
      (shell-last-status sh))))
