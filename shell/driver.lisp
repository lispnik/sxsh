;;;; shell/driver.lisp --- top-level drivers and REPL.

(in-package #:posh-shell)

(defun run-string-capturing (sh src out)
  "Parse and run SRC, sending command output to the current fd 1 (already set
up by the caller). OUT is the builtin's stream; we finish it first so ordering
with child output is sane. Returns the last status."
  (declare (ignore out))
  (handler-case
      (run sh (parse-string src))
    (posh:shell-parse-error (e)
      (format *error-output* "posh: ~A~%" e)
      2)))

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
`posh < script' and `cmd | posh' print PS1 prompts into their own output and
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
          (posh:shell-parse-error (e)
            (format *error-output* "posh: ~A~%" e))
          (shell-exit (e)
            (return (or (shell-exit-code e) 0)))
          (error (e)
            (format *error-output* "posh: ~A~%" e)))))))

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
            (posh:shell-parse-error ()
              ;; incomplete -> keep buffering
              (write-string src buffer))
            (error () (return src)))))))) ; other errors: let run report

(defun supported-platform-p ()
  "posh/shell relies on POSIX (posix_spawn, dup2, pipe, waitpid). It runs on
Linux and macOS; anything else (notably Windows) is unsupported."
  (and (member :unix *features*) t))

(defun main (&optional (argv (rest sb-ext:*posix-argv*)))
  "Entry point. Supports: posh script.sh [args], posh -c 'cmd', or interactive."
  (unless (supported-platform-p)
    (format *error-output*
            "posh: unsupported platform; requires a POSIX system (Linux or macOS)~%")
    (return-from main 1))
  (let ((sh (make-shell :interactive (and (null argv)
                                          (stdin-is-interactive-p)))))
    (unwind-protect
         (handler-case
             (cond
               ((null argv)
                (if (shell-interactive sh)
                    (repl sh)
                    ;; A non-interactive shell reading a script from standard
                    ;; input parses the whole text at once, exactly as it would
                    ;; a file. Feeding it to the line-at-a-time reader instead
                    ;; makes constructs that span lines depend on the reader
                    ;; guessing where a command ends.
                    (run-string sh (slurp-file "/dev/stdin"))))
               ((string= (first argv) "-c")
                (set-positional sh (cddr argv))
                (run-string sh (second argv)))
               (t
                (setf (shell-name sh) (first argv))
                (set-positional sh (rest argv))
                (run-string sh (slurp-file (first argv)))))
           (shell-exit (e) (setf (shell-last-status sh) (or (shell-exit-code e) 0)))
           (posh:shell-parse-error (e)
             (format *error-output* "posh: ~A~%" e)
             (setf (shell-last-status sh) 2))
           (error (e)
             (format *error-output* "posh: ~A~%" e)
             (setf (shell-last-status sh) 1)))
      ;; POSIX: the EXIT trap runs when the shell terminates, whatever the cause
      (run-exit-traps sh))
    (shell-last-status sh)))
