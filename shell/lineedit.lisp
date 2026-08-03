;;;; shell/lineedit.lisp --- readline-style line editing.
;;;;
;;;; Sits on shell/term.lisp (raw mode, key decoding) and supplies lines to the
;;;; REPL. Two structural decisions shape everything here:
;;;;
;;;; Raw mode is PER LINE, not per REPL. EDIT-ONE-LINE enters raw mode and
;;;; restores on exit, so between prompts -- i.e. for the whole duration of
;;;; every command the shell runs -- the terminal is in whatever cooked state
;;;; the user's stty left it. Children then inherit a sane line discipline with
;;;; no coordination, job control needs no changes, and a command that
;;;; legitimately reconfigures the terminal becomes the new baseline instead of
;;;; being silently reverted.
;;;;
;;;; We edit ONE PHYSICAL LINE and loop above it. READ-COMPLETE-COMMAND already
;;;; has a contract -- read a line, append, re-parse, keep going while the parse
;;;; is incomplete -- and EDIT-READ-COMMAND mirrors it rather than trying to be
;;;; a whole-command editor. A multi-line buffer would need the redraw to handle
;;;; embedded newlines AND make Up/Down ambiguous (move within the buffer, or
;;;; walk history?). The renderer is newline-aware anyway, so a recalled
;;;; multi-line entry still displays correctly.

(in-package #:sxsh-shell)

(defvar *lineedit-disabled* nil
  "Set after the editor fails once; never re-entered afterwards. The shell
falls back to the plain reader rather than repeating whatever went wrong.")

(defvar *kill-ring* '()
  "Newest first, capped at +kill-ring-max+. A GLOBAL, deliberately: killing on
one line and yanking on the next is the whole point of a kill ring, so it must
outlive a single prompt.")

(defconstant +kill-ring-max+ 32)
(defconstant +undo-max+ 100)

(defstruct led
  (buf (make-array 64 :element-type 'character :adjustable t :fill-pointer 0))
  (point 0)
  (prompt "")
  (prompt-width 0)
  (cols 80)
  (rows-used 1)                         ; rows the previous paint occupied
  (cursor-row 0)                        ; which of them the cursor ended on
  (hist-index nil)                      ; NIL = on the fresh line
  (hist-saved nil)                      ; the fresh line, stashed on first Up
  (undo '())
  (last-fn nil)
  (yank-start nil)
  (yank-end nil)
  (search nil)
  (done nil))                           ; :accept :eof :interrupt

(defstruct led-search
  (query "") (index nil) (failed nil) (saved-buf "") (saved-point 0))

;;; ---------------------------------------------------------------------------
;;; Buffer helpers
;;; ---------------------------------------------------------------------------

(defun led-text (led)
  "A FRESH copy of the buffer contents.

SUBSEQ, not COERCE. The buffer is an adjustable fill-pointer string, so
(coerce buf 'string) returns the buffer ITSELF rather than a copy -- every
saved snapshot was then a live alias that changed as the user kept typing.
Undo restored the buffer to whatever it already was, `hist-saved' came back as
the recalled entry instead of the typed line, and C-g restored the search hit
instead of the line the search started from. One aliasing bug, three symptoms."
  (subseq (led-buf led) 0 (fill-pointer (led-buf led))))

(defun led-set-text (led text &optional (point (length text)))
  (let ((buf (led-buf led)))
    (setf (fill-pointer buf) 0)
    (loop for c across text do (vector-push-extend c buf))
    (setf (led-point led) (min point (fill-pointer buf)))))

(defun led-len (led) (fill-pointer (led-buf led)))

(defun led-snapshot (led &optional coalesce-as)
  "Record the buffer for undo.

COALESCE-AS is the command doing the recording, given only by commands that
should merge into a run -- self-insert, so a typed word undoes as a word. Any
other command always snapshots. Testing only the PREVIOUS command (as this did
at first) suppresses the snapshot for whatever follows a burst of typing, so
the very kill or yank the user wants to undo is the one never recorded."
  (unless (and coalesce-as (eq (led-last-fn led) coalesce-as))
    (push (cons (led-text led) (led-point led)) (led-undo led))
    (when (> (length (led-undo led)) +undo-max+)
      (setf (led-undo led) (subseq (led-undo led) 0 +undo-max+)))))

(defun led-insert (led string)
  (let ((buf (led-buf led)) (p (led-point led)))
    (loop for c across string do (vector-push-extend c buf))
    ;; shift the tail right
    (let ((n (length string)) (len (fill-pointer buf)))
      (loop for i from (1- len) downto (+ p n)
            do (setf (aref buf i) (aref buf (- i n))))
      (loop for i from 0 below n
            do (setf (aref buf (+ p i)) (char string i))))
    (incf (led-point led) (length string))))

(defun led-delete-region (led start end)
  "Remove [START, END) and return the text removed."
  (let* ((buf (led-buf led))
         (start (max 0 (min start (fill-pointer buf))))
         (end (max start (min end (fill-pointer buf))))
         (removed (coerce (subseq buf start end) 'string)))
    (replace buf buf :start1 start :start2 end)
    (decf (fill-pointer buf) (- end start))
    (setf (led-point led) (min (led-point led) (fill-pointer buf)))
    (when (> (led-point led) start)
      (setf (led-point led) (max start (- (led-point led) (- end start)))))
    removed))

(defun kill-push (text &optional append-p backward-p)
  "Add TEXT to the kill ring. Consecutive kills accumulate into one entry --
that is what makes repeated C-k on a wrapped line yank back as one string."
  (when (plusp (length text))
    (if (and append-p *kill-ring*)
        (setf (first *kill-ring*)
              (if backward-p
                  (concatenate 'string text (first *kill-ring*))
                  (concatenate 'string (first *kill-ring*) text)))
        (push text *kill-ring*))
    (when (> (length *kill-ring*) +kill-ring-max+)
      (setf *kill-ring* (subseq *kill-ring* 0 +kill-ring-max+)))))

(defun kill-p (fn)
  (member fn '(ed-kill-line ed-backward-kill-line ed-unix-word-rubout
               ed-kill-word ed-backward-kill-word)))

;;; ---------------------------------------------------------------------------
;;; Layout and redraw
;;; ---------------------------------------------------------------------------

(defun display-width (string)
  "Columns STRING occupies, ignoring CSI and OSC escape sequences.

A prompt may contain colour codes; counting those as printable makes every
subsequent column calculation wrong. Handles the two forms that actually
appear -- ESC [ ... final, and ESC ] ... BEL/ST."
  (let ((n 0) (i 0) (len (length string)))
    (loop while (< i len) do
      (let ((c (char string i)))
        (cond
          ;; A `\[ ... \]' run from the prompt occupies no columns at all --
          ;; that is what the markers are for. Counting it made every column
          ;; calculation after a coloured prompt wrong.
          ((char= c +prompt-hide-start+)
           (incf i)
           (loop while (and (< i len) (char/= (char string i) +prompt-hide-end+))
                 do (incf i))
           (incf i))
          ((char= c +prompt-hide-end+) (incf i))
          ((and (char= c #\Escape) (< (1+ i) len) (char= (char string (1+ i)) #\[))
           (incf i 2)
           (loop while (and (< i len)
                            (not (<= #x40 (char-code (char string i)) #x7E)))
                 do (incf i))
           (incf i))
          ((and (char= c #\Escape) (< (1+ i) len) (char= (char string (1+ i)) #\]))
           (incf i 2)
           (loop while (and (< i len)
                            (/= (char-code (char string i)) 7)
                            (not (char= (char string i) #\Escape)))
                 do (incf i))
           (incf i))
          (t (incf n) (incf i)))))
    n))

(defun led-layout (led)
  "Return (values display cursor-row cursor-col rows).

DISPLAY renders control characters as ^X and keeps newlines, which the painter
turns into CR LF. The cursor position is computed in the same pass, so it can
never disagree with the text that was painted."
  (let* ((cols (max 1 (led-cols led)))
         (out (make-string-output-stream))
         (row 0) (col (led-prompt-width led))
         (crow 0) (ccol (led-prompt-width led))
         (buf (led-buf led)))
    (flet ((advance (n)
             (incf col n)
             (loop while (>= col cols) do (incf row) (decf col cols))))
      (dotimes (i (fill-pointer buf))
        (when (= i (led-point led)) (setf crow row ccol col))
        (let* ((ch (aref buf i)) (code (char-code ch)))
          (cond
            ((char= ch #\Newline)
             (write-char #\Newline out) (incf row) (setf col 0))
            ((< code 32)
             (write-char #\^ out)
             (write-char (code-char (+ 64 code)) out)
             (advance 2))
            ((= code 127) (write-string "^?" out) (advance 2))
            (t (write-char ch out) (advance 1)))))
      (when (= (led-point led) (fill-pointer buf))
        (setf crow row ccol col)))
    (values (get-output-stream-string out) crow ccol (1+ row))))

(defparameter +csi+ (format nil "~C[" #\Escape))

(defun led-redraw (led)
  "Repaint the prompt and buffer in place, then position the cursor.

Assembled into one buffer and written once -- a redraw per character would
flicker. The block of rows starts where the prompt started; ROWS-USED and
CURSOR-ROW remember where the last paint left things, because the terminal
gives us no way to ask."
  (multiple-value-bind (display crow ccol rows) (led-layout led)
    ;; 1. back to the top-left of the block
    (term-write (string #\Return))
    (when (plusp (led-cursor-row led))
      (term-write (format nil "~A~DA" +csi+ (led-cursor-row led))))
    ;; 2. prompt + text, clearing to end of each line as we go. The \[ \]
    ;; markers are ours, not the terminal's, so they come out here.
    (term-write (prompt-strip-markers (led-prompt led)))
    (loop for ch across display
          do (if (char= ch #\Newline)
                 (progn (term-write (format nil "~A0K" +csi+))
                        (term-write (coerce (list #\Return #\Newline) 'string)))
                 (term-write (string ch))))
    (term-write (format nil "~A0K" +csi+))
    ;; 3. wipe any rows the previous paint used that this one does not
    (let ((extra (- (led-rows-used led) rows)))
      (when (plusp extra)
        (dotimes (_ extra)
          (term-write (coerce (list #\Return #\Newline) 'string))
          (term-write (format nil "~A0K" +csi+)))
        (term-write (format nil "~A~DA" +csi+ extra))))
    ;; 4. up from the last painted row to the cursor's row, then across
    (let ((up (- (1- rows) crow)))
      (when (plusp up) (term-write (format nil "~A~DA" +csi+ up))))
    (term-write (string #\Return))
    (when (plusp ccol) (term-write (format nil "~A~DC" +csi+ ccol)))
    (setf (led-rows-used led) rows
          (led-cursor-row led) crow)
    (term-flush)))

;;; ---------------------------------------------------------------------------
;;; Word boundaries
;;;
;;; Two definitions, both needed and genuinely different. M-f/M-b move over
;;; alphanumeric runs (readline); C-w rubs out a whitespace-delimited word.
;;; They disagree on `/usr/bin/foo' and users notice immediately.
;;; ---------------------------------------------------------------------------

(defun word-forward-pos (led)
  (let ((buf (led-buf led)) (i (led-point led)) (len (led-len led)))
    (loop while (and (< i len) (not (alphanumericp (aref buf i)))) do (incf i))
    (loop while (and (< i len) (alphanumericp (aref buf i))) do (incf i))
    i))

(defun word-backward-pos (led)
  (let ((buf (led-buf led)) (i (led-point led)))
    (loop while (and (plusp i) (not (alphanumericp (aref buf (1- i))))) do (decf i))
    (loop while (and (plusp i) (alphanumericp (aref buf (1- i)))) do (decf i))
    i))

(defun blank-word-backward-pos (led)
  (let ((buf (led-buf led)) (i (led-point led)))
    (loop while (and (plusp i) (member (aref buf (1- i)) '(#\Space #\Tab)))
          do (decf i))
    (loop while (and (plusp i) (not (member (aref buf (1- i)) '(#\Space #\Tab))))
          do (decf i))
    i))

;;; ---------------------------------------------------------------------------
;;; Editing commands
;;; ---------------------------------------------------------------------------

(defun ed-self-insert (led sh key)
  (declare (ignore sh))
  (led-snapshot led 'ed-self-insert)
  (led-insert led (string (if (characterp key) key #\?))))

(defun ed-beginning-of-line (led sh key) (declare (ignore sh key))
  (setf (led-point led) 0))
(defun ed-end-of-line (led sh key) (declare (ignore sh key))
  (setf (led-point led) (led-len led)))
(defun ed-forward-char (led sh key) (declare (ignore sh key))
  (when (< (led-point led) (led-len led)) (incf (led-point led))))
(defun ed-backward-char (led sh key) (declare (ignore sh key))
  (when (plusp (led-point led)) (decf (led-point led))))
(defun ed-forward-word (led sh key) (declare (ignore sh key))
  (setf (led-point led) (word-forward-pos led)))
(defun ed-backward-word (led sh key) (declare (ignore sh key))
  (setf (led-point led) (word-backward-pos led)))

(defun ed-delete-char (led sh key) (declare (ignore sh key))
  (when (< (led-point led) (led-len led))
    (led-snapshot led)
    (led-delete-region led (led-point led) (1+ (led-point led)))))

(defun ed-backward-delete-char (led sh key) (declare (ignore sh key))
  (when (plusp (led-point led))
    (led-snapshot led)
    (let ((p (led-point led)))
      (setf (led-point led) (1- p))
      (led-delete-region led (1- p) p))))

(defun ed-kill-line (led sh key) (declare (ignore sh key))
  (led-snapshot led)
  (kill-push (led-delete-region led (led-point led) (led-len led))
             (kill-p (led-last-fn led)) nil))

(defun ed-backward-kill-line (led sh key) (declare (ignore sh key))
  (led-snapshot led)
  (let ((p (led-point led)))
    (setf (led-point led) 0)
    (kill-push (led-delete-region led 0 p) (kill-p (led-last-fn led)) t)))

(defun ed-unix-word-rubout (led sh key) (declare (ignore sh key))
  (led-snapshot led)
  (let ((start (blank-word-backward-pos led)) (p (led-point led)))
    (setf (led-point led) start)
    (kill-push (led-delete-region led start p) (kill-p (led-last-fn led)) t)))

(defun ed-kill-word (led sh key) (declare (ignore sh key))
  (led-snapshot led)
  (kill-push (led-delete-region led (led-point led) (word-forward-pos led))
             (kill-p (led-last-fn led)) nil))

(defun ed-backward-kill-word (led sh key) (declare (ignore sh key))
  (led-snapshot led)
  (let ((start (word-backward-pos led)) (p (led-point led)))
    (setf (led-point led) start)
    (kill-push (led-delete-region led start p) (kill-p (led-last-fn led)) t)))

(defun ed-yank (led sh key) (declare (ignore sh key))
  (when *kill-ring*
    (led-snapshot led)
    (setf (led-yank-start led) (led-point led))
    (led-insert led (first *kill-ring*))
    (setf (led-yank-end led) (led-point led))))

(defun ed-yank-pop (led sh key) (declare (ignore sh key))
  ;; Only meaningful straight after a yank; otherwise it would delete text the
  ;; user never yanked.
  (when (and (member (led-last-fn led) '(ed-yank ed-yank-pop))
             (> (length *kill-ring*) 1)
             (led-yank-start led))
    (setf *kill-ring* (append (rest *kill-ring*) (list (first *kill-ring*))))
    (setf (led-point led) (led-yank-start led))
    (led-delete-region led (led-yank-start led) (led-yank-end led))
    (setf (led-point led) (led-yank-start led))
    (led-insert led (first *kill-ring*))
    (setf (led-yank-end led) (led-point led))))

(defun ed-transpose-chars (led sh key) (declare (ignore sh key))
  (let ((buf (led-buf led)) (p (led-point led)) (len (led-len led)))
    (when (>= len 2)
      (led-snapshot led)
      (let ((i (if (= p len) (- p 2) (max 0 (1- p)))))
        (rotatef (aref buf i) (aref buf (1+ i)))
        (setf (led-point led) (min len (+ i 2)))))))

(defun map-word-case (led fn)
  (let ((end (word-forward-pos led)) (buf (led-buf led)))
    (led-snapshot led)
    (loop for i from (led-point led) below end
          do (setf (aref buf i) (funcall fn (aref buf i))))
    (setf (led-point led) end)))

(defun ed-upcase-word (led sh key) (declare (ignore sh key))
  (map-word-case led #'char-upcase))
(defun ed-downcase-word (led sh key) (declare (ignore sh key))
  (map-word-case led #'char-downcase))

(defun ed-capitalize-word (led sh key) (declare (ignore sh key))
  (let ((buf (led-buf led)) (end (word-forward-pos led)) (first t))
    (led-snapshot led)
    (loop for i from (led-point led) below end
          do (when (alphanumericp (aref buf i))
               (setf (aref buf i) (if first
                                      (char-upcase (aref buf i))
                                      (char-downcase (aref buf i)))
                     first nil)))
    (setf (led-point led) end)))

(defun ed-clear-screen (led sh key) (declare (ignore sh key))
  ;; The user's escape hatch from any redraw desync, so it must be trivially
  ;; correct: home the cursor, erase everything, forget what we painted.
  (term-write (format nil "~AH~A2J" +csi+ +csi+))
  (setf (led-rows-used led) 1 (led-cursor-row led) 0))

(defun ed-undo (led sh key) (declare (ignore sh key))
  (let ((entry (pop (led-undo led))))
    (if entry
        (led-set-text led (car entry) (cdr entry))
        (term-beep))))

(defun ed-abort (led sh key) (declare (ignore sh key))
  (if (led-search led)
      (let ((s (led-search led)))
        (led-set-text led (led-search-saved-buf s) (led-search-saved-point s))
        (setf (led-search led) nil))
      (term-beep)))

(defun ed-accept-line (led sh key) (declare (ignore sh key))
  (setf (led-search led) nil (led-done led) :accept))

(defun ed-eof-or-delete (led sh key)
  (cond
    ((plusp (led-len led)) (ed-delete-char led sh key))
    ;; `set -o ignoreeof': an empty-line C-d says how to leave instead.
    ((opt sh :ignoreeof)
     (term-write (format nil "~C~CUse \"exit\" to leave the shell.~C~C"
                         #\Return #\Newline #\Return #\Newline))
     (setf (led-rows-used led) 1 (led-cursor-row led) 0))
    (t (setf (led-done led) :eof))))

(defun ed-insert-paste (led)
  "Insert everything up to the paste-end marker literally.

Without this, pasting a multi-line snippet executes it a line at a time as it
arrives -- so a half-finished paste starts running -- and every TAB in the
pasted text fires completion instead of being text. Newlines are kept in the
buffer; the renderer handles them, and the command runs only when the user
presses Return."
  (led-snapshot led)
  (loop
    (let ((k (read-key)))
      (cond
        ((eq k :paste-end) (return))
        ;; Anything that ends input mid-paste has to stop the loop, or a
        ;; disconnected terminal would spin here forever.
        ((member k '(:eof :interrupt)) (return))
        ((characterp k)
         (led-insert led (string (if (char= k #\Return) #\Newline k))))
        (t nil)))))

(defun ed-quoted-insert (led sh key) (declare (ignore key))
  (let ((k (read-key)))
    (when (characterp k) (ed-self-insert led sh k))))

;;; --- history navigation ----------------------------------------------------

(defun ed-previous-history (led sh key) (declare (ignore key))
  (let ((n (history-count sh)))
    (cond
      ((zerop n) (term-beep))
      ((null (led-hist-index led))
       ;; Stash whatever was being typed, so Down brings it back.
       (setf (led-hist-saved led) (led-text led)
             (led-hist-index led) (1- n))
       (led-set-text led (aref (shell-history sh) (1- n))))
      ((plusp (led-hist-index led))
       (decf (led-hist-index led))
       (led-set-text led (aref (shell-history sh) (led-hist-index led))))
      (t (term-beep)))))

(defun ed-next-history (led sh key) (declare (ignore key))
  (let ((n (history-count sh)) (i (led-hist-index led)))
    (cond
      ((null i) (term-beep))
      ((< (1+ i) n)
       (incf (led-hist-index led))
       (led-set-text led (aref (shell-history sh) (led-hist-index led))))
      (t
       (setf (led-hist-index led) nil)
       (led-set-text led (or (led-hist-saved led) ""))))))

(defun ed-beginning-of-history (led sh key) (declare (ignore key))
  (when (plusp (history-count sh))
    (when (null (led-hist-index led))
      (setf (led-hist-saved led) (led-text led)))
    (setf (led-hist-index led) 0)
    (led-set-text led (aref (shell-history sh) 0))))

(defun ed-end-of-history (led sh key) (declare (ignore key))
  (setf (led-hist-index led) nil)
  (led-set-text led (or (led-hist-saved led) "")))

;;; --- reverse incremental search --------------------------------------------

(defun search-prompt (led)
  (let ((s (led-search led)))
    (format nil "(~:[~;failed ~]reverse-i-search)`~A': "
            (led-search-failed s) (led-search-query s))))

(defun search-run (led sh)
  "Find the newest entry at or before the cursor containing the query."
  (let* ((s (led-search led))
         (q (led-search-query s))
         (start (or (led-search-index s) (1- (history-count sh)))))
    (if (zerop (length q))
        (setf (led-search-failed s) nil)
        (let ((hit (loop for i from start downto 0
                         when (search q (aref (shell-history sh) i))
                           do (return i))))
          (if hit
              (progn (setf (led-search-index s) hit
                           (led-search-failed s) nil)
                     (led-set-text led (aref (shell-history sh) hit)))
              (setf (led-search-failed s) t))))))

(defun ed-reverse-search-history (led sh key) (declare (ignore key))
  (if (led-search led)
      ;; Another C-r: step further back from the current hit.
      (let ((s (led-search led)))
        (when (led-search-index s)
          (setf (led-search-index s) (1- (led-search-index s))))
        (search-run led sh))
      (setf (led-search led)
            (make-led-search :saved-buf (led-text led)
                             :saved-point (led-point led)
                             :index (1- (history-count sh))))))

;;; ---------------------------------------------------------------------------
;;; Keymap
;;; ---------------------------------------------------------------------------

(defun ctrl (ch) (code-char (logand (char-code (char-upcase ch)) #x1F)))

(defparameter +emacs-keymap+
  (let ((m (make-hash-table :test 'equal)))
    (macrolet ((bind (key fn) `(setf (gethash ,key m) ,fn)))
      (bind (ctrl #\a) 'ed-beginning-of-line)
      (bind (ctrl #\e) 'ed-end-of-line)
      (bind (ctrl #\b) 'ed-backward-char)
      (bind (ctrl #\f) 'ed-forward-char)
      (bind (ctrl #\p) 'ed-previous-history)
      (bind (ctrl #\n) 'ed-next-history)
      (bind (ctrl #\d) 'ed-eof-or-delete)
      (bind (ctrl #\h) 'ed-backward-delete-char)
      (bind (ctrl #\k) 'ed-kill-line)
      (bind (ctrl #\u) 'ed-backward-kill-line)
      (bind (ctrl #\w) 'ed-unix-word-rubout)
      (bind (ctrl #\y) 'ed-yank)
      (bind (ctrl #\t) 'ed-transpose-chars)
      (bind (ctrl #\l) 'ed-clear-screen)
      (bind (ctrl #\r) 'ed-reverse-search-history)
      (bind (ctrl #\v) 'ed-quoted-insert)
      (bind (ctrl #\g) 'ed-abort)
      (bind (code-char 31) 'ed-undo)    ; C-_
      (bind #\Tab 'ed-complete)
      (bind #\Return 'ed-accept-line)
      (bind (ctrl #\j) 'ed-accept-line)
      (bind :backspace 'ed-backward-delete-char)
      (bind :up 'ed-previous-history)
      (bind :down 'ed-next-history)
      (bind :left 'ed-backward-char)
      (bind :right 'ed-forward-char)
      (bind :ctrl-left 'ed-backward-word)
      (bind :ctrl-right 'ed-forward-word)
      (bind :home 'ed-beginning-of-line)
      (bind :end 'ed-end-of-line)
      (bind :delete 'ed-delete-char)
      (bind '(:meta . #\b) 'ed-backward-word)
      (bind '(:meta . #\f) 'ed-forward-word)
      (bind '(:meta . #\d) 'ed-kill-word)
      (bind '(:meta . #\u) 'ed-upcase-word)
      (bind '(:meta . #\l) 'ed-downcase-word)
      (bind '(:meta . #\c) 'ed-capitalize-word)
      (bind '(:meta . #\y) 'ed-yank-pop)
      (bind '(:meta . #\<) 'ed-beginning-of-history)
      (bind '(:meta . #\>) 'ed-end-of-history)
      (bind (cons :meta :backspace) 'ed-backward-kill-word)
      (bind '(:meta . #\Rubout) 'ed-backward-kill-word))
    m))

;;; ---------------------------------------------------------------------------
;;; The edit loop
;;; ---------------------------------------------------------------------------

(defun search-handle-key (led sh key)
  "While C-r is active, printable keys extend the query and most others end
the search, leaving the found line in the buffer to be edited or accepted."
  (let ((s (led-search led)))
    (cond
      ((characterp key)
       (cond
         ((char= key (ctrl #\r)) (ed-reverse-search-history led sh key) t)
         ((char= key (ctrl #\g)) (ed-abort led sh key) t)
         ((or (char= key #\Return) (char= key (ctrl #\j)))
          (setf (led-search led) nil) nil)     ; fall through to accept
         ((>= (char-code key) 32)
          (setf (led-search-query s)
                (concatenate 'string (led-search-query s) (string key)))
          (search-run led sh) t)
         (t (setf (led-search led) nil) nil)))
      ((eq key :backspace)
       (let ((q (led-search-query s)))
         (when (plusp (length q))
           (setf (led-search-query s) (subseq q 0 (1- (length q))))
           (setf (led-search-index s) (1- (history-count sh)))
           (search-run led sh)))
       t)
      (t (setf (led-search led) nil) nil))))

(defun edit-one-line (sh prompt)
  "Read one physical line with editing. Returns a string, :EOF or :INTERRUPT.

Every exit runs WITH-RAW-TERMINAL's cleanup, so the terminal cannot be left
raw -- that is the invariant the whole file is arranged around."
  (let ((led (make-led :prompt prompt
                       :prompt-width (display-width prompt)
                       :cols (terminal-columns))))
    (let ((r (with-raw-terminal
               (term-write +paste-on+)
               (led-redraw led)
               (loop
                 (let ((key (read-key)))
                   (case key
                     (:eof (when (zerop (led-len led))
                             (setf (led-done led) :eof)))
                     (:interrupt
                      ;; Abandon the line, exactly as bash does: show ^C, drop
                      ;; the buffer, and let the REPL start a fresh prompt.
                      (term-write (format nil "^C~C~C" #\Return #\Newline))
                      (term-flush)
                      (setf (led-done led) :interrupt))
                     ((:winch :cont)
                      (setf (led-cols led) (terminal-columns))
                      (when (eq key :cont)
                        ;; A stop/continue leaves the terminal cooked again.
                        (ignore-errors (raw-mode-apply (tty-fd))))
                      (setf (led-rows-used led) 1 (led-cursor-row led) 0)
                      (led-redraw led))
                     (:paste-start (ed-insert-paste led))
                     ((:escape :unknown :paste-end))
                     (t
                      (let ((handled (and (led-search led)
                                          (search-handle-key led sh key))))
                        (unless handled
                          (let ((fn (or (gethash key +emacs-keymap+)
                                        (and (characterp key)
                                             (>= (char-code key) 32)
                                             'ed-self-insert))))
                            (when fn
                              ;; Per KEYSTROKE, not per line: a bug in one
                              ;; command should ring the bell, not end the
                              ;; user's session.
                              (handler-case (funcall fn led sh key)
                                (error () (term-beep)))
                              (setf (led-last-fn led) fn))))))))
                 (when (led-done led) (return))
                 (if (led-search led)
                     (let ((saved (led-prompt led))
                           (savedw (led-prompt-width led)))
                       (setf (led-prompt led) (search-prompt led)
                             (led-prompt-width led) (display-width (search-prompt led)))
                       (led-redraw led)
                       (setf (led-prompt led) saved (led-prompt-width led) savedw))
                     (led-redraw led)))
               (led-done led))))
      (case r
        (:no-tty :no-tty)
        (:eof :eof)
        (:interrupt :interrupt)
        (t (led-text led))))))

;;; ---------------------------------------------------------------------------
;;; Whole commands
;;; ---------------------------------------------------------------------------

(defun command-complete-p (src)
  "True when SRC parses AND no here-doc is still waiting for its delimiter.

Shared by both readers so the editor and the plain reader cannot disagree
about where a command ends."
  (handler-case
      (multiple-value-bind (program incomplete) (parse-string src)
        (declare (ignore program))
        (not incomplete))
    (sxsh:shell-parse-error () nil)
    (error () t)))

(defun edit-read-command (sh)
  "One logical command, read with editing. Returns a string, NIL at EOF, or
:INTERRUPT.

Mirrors READ-COMPLETE-COMMAND's contract -- keep reading while the accumulated
text is not a complete command -- but sources lines from the editor and
switches to $PS2 for continuations. PS2 was defined and never used until now."
  (let ((acc ""))
    (loop
      (let* ((prompt (if (string= acc "")
                         (expand-prompt sh (or (nth-value 0 (get-var sh "PS1"))
                                               "\\s-\\v\\$ "))
                         (expand-prompt sh (or (nth-value 0 (get-var sh "PS2"))
                                               "> "))))
             (line (edit-one-line sh prompt)))
        (case line
          (:no-tty (return :no-tty))
          (:interrupt (return :interrupt))
          (:eof (return (if (string= acc "") nil :interrupt)))
          (t
           (setf acc (if (string= acc "")
                         line
                         (concatenate 'string acc (string #\Newline) line)))
           (when (command-complete-p acc)
             (return acc))))))))

(defun line-editing-active-p (sh)
  "Whether to use the editor for the next prompt.

Every clause is an escape hatch someone will need: SXSH_NO_LINEEDIT for a
terminal we mishandle, `set +o emacs' for preference, TERM=dumb for Emacs's
shell buffer, and *LINEEDIT-DISABLED* for after we have already failed once."
  (and (shell-interactive sh)
       (opt sh :emacs)
       (not *lineedit-disabled*)
       (null (sb-posix:getenv "SXSH_NO_LINEEDIT"))
       (let ((term (nth-value 0 (get-var sh "TERM"))))
         (and term (not (string= term "dumb"))))
       (plusp (sb-unix:unix-isatty 0))
       (have-tty-p)))
