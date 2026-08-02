;;;; shell/complete.lisp --- TAB completion for the line editor.
;;;;
;;;; Finding the word to complete does NOT go through the parser. The line
;;;; being typed is usually not parseable -- that is the normal case, not an
;;;; edge case -- so COMPLETION-CONTEXT is a purpose-built left-scan that
;;;; tolerates unterminated quotes and substitutions.

(in-package #:sxsh-shell)

(defconstant +completion-list-threshold+ 100
  "Above this many candidates, ask before listing. A bare TAB in /usr/bin
otherwise dumps thousands of lines over the prompt.")

(defparameter +word-delimiters+ '(#\Space #\Tab #\Newline #\| #\& #\; #\< #\>
                                  #\( #\)))

(defparameter +command-introducers+
  '(";" "&" "|" "&&" "||" "(" ")" "{" "}" "if" "then" "elif" "else" "do"
    "done" "!" "time" "while" "until")
  "Tokens after which a following word starts a command rather than an
argument.")

;;; ---------------------------------------------------------------------------
;;; Locating the word
;;; ---------------------------------------------------------------------------

(defun completion-context (line point)
  "Return (values start end word quote kind).

QUOTE is NIL, #\\' or #\\\" -- the quoting in force at POINT. KIND is :command,
:file or :variable. START/END bound the word being completed, so the caller can
replace exactly that span."
  (let ((i 0) (start 0) (quote nil) (depth 0))
    ;; Left-to-right so quote state is always known; the word starts after the
    ;; last delimiter seen OUTSIDE quotes.
    (loop while (< i point) do
      (let ((c (char line i)))
        (cond
          ((and (null quote) (char= c #\\)) (incf i 2))
          ((and (null quote) (member c '(#\' #\"))) (setf quote c) (incf i))
          ((and quote (char= c quote)) (setf quote nil) (incf i))
          (quote (incf i))
          ((and (char= c #\$) (< (1+ i) (length line))
                (member (char line (1+ i)) '(#\( #\{)))
           (incf depth) (incf i 2))
          ((and (plusp depth) (member c '(#\) #\}))) (decf depth) (incf i))
          ((and (zerop depth) (member c +word-delimiters+))
           (setf start (1+ i)) (incf i))
          (t (incf i)))))
    ;; The word begins after the opening quote, if any.
    (let* ((qchar (let ((q nil))
                    (loop for j from start below point
                          do (cond ((and (null q) (member (char line j) '(#\' #\")))
                                    (setf q (char line j)))
                                   ((and q (char= (char line j) q)) (setf q nil))))
                    q))
           (wstart (if (and qchar (< start point) (char= (char line start) qchar))
                       (1+ start)
                       start))
           (word (subseq line wstart point)))
      (values wstart point word qchar
              (completion-kind line start word)))))

(defun completion-kind (line start word)
  "Decide what WORD names, from what precedes it."
  (cond
    ;; $foo / ${foo
    ((and (plusp (length word)) (char= (char word 0) #\$)) :variable)
    ;; A word containing a slash is a path even in command position, so
    ;; `./scr<TAB>' completes files rather than scanning $PATH.
    ((find #\/ word) :file)
    (t
     (let* ((before (string-right-trim '(#\Space #\Tab) (subseq line 0 start)))
            (last-tok (let ((p (position-if (lambda (c) (member c +word-delimiters+))
                                            before :from-end t)))
                        (if p (subseq before p) before))))
       (cond
         ((string= (string-trim '(#\Space #\Tab) before) "") :command)
         ;; After a redirection operator the word is always a filename.
         ((and (plusp (length before))
               (member (char before (1- (length before))) '(#\< #\>)))
          :file)
         ((member (string-trim '(#\Space #\Tab) last-tok)
                  +command-introducers+ :test #'string=)
          :command)
         ;; `VAR=value cmd' -- an assignment prefix still leaves us in command
         ;; position for the next word.
         ((let ((tok (string-trim '(#\Space #\Tab) last-tok)))
            (and (find #\= tok)
                 (let ((eq (position #\= tok)))
                   (and (plusp eq) (sxsh::valid-name-p (subseq tok 0 eq))))))
          :command)
         (t :file))))))

;;; ---------------------------------------------------------------------------
;;; Candidates
;;; ---------------------------------------------------------------------------

(defun path-directory-p (path)
  "Directory test by stat, not TRUENAME.

DIRECTORYP goes through TRUENAME, which is slow across hundreds of entries and
signals on a broken symlink -- both of which a completion list hits routinely."
  (handler-case
      (= (logand (sb-posix:stat-mode (sb-posix:stat path)) #o170000) #o040000)
    (error () nil)))

(defun file-candidates (sh word)
  "Pathnames matching WORD. Returns a list of (text . suffix)."
  (let* ((slash (position #\/ word :from-end t))
         (dirpart (if slash (subseq word 0 (1+ slash)) ""))
         (prefix (if slash (subseq word (1+ slash)) word))
         (dir (cond ((string= dirpart "") ".")
                    ((char= (char dirpart 0) #\~)
                     (multiple-value-bind (expanded next)
                         (expand-tilde sh dirpart 0 (length dirpart))
                       (declare (ignore next))
                       (if (plusp (length expanded)) expanded dirpart)))
                    (t dirpart)))
         (out '()))
    (dolist (name (directory-entries dir) (nreverse out))
      (unless (or (string= name ".") (string= name ".."))
        (when (and (>= (length name) (length prefix))
                   (string= prefix name :end2 (length prefix))
                   ;; A dotfile only shows when the user typed the dot, the
                   ;; same rule globbing uses.
                   (or (plusp (length prefix))
                       (not (char= (char name 0) #\.))))
          (let ((full (concatenate 'string
                                   (if (string= dir ".") "" dir) name)))
            (push (cons (concatenate 'string dirpart name)
                        (if (path-directory-p full) "/" " "))
                  out)))))))

(defvar *command-cache* nil)
(defvar *command-cache-path* nil)

(defun command-names (sh)
  "Every name usable in command position: builtins, functions, reserved words,
and each executable on $PATH.

Cached per $PATH value: a TAB on an empty word otherwise stats several thousand
files, and the two-TAB listing convention means doing it twice in a row."
  (let ((path (or (nth-value 0 (get-var sh "PATH")) "")))
    (unless (and *command-cache* (equal path *command-cache-path*))
      (let ((names '()))
        (maphash (lambda (k v) (declare (ignore v)) (push k names)) *builtins*)
        (maphash (lambda (k v) (declare (ignore v)) (push k names))
                 (shell-functions sh))
        (dolist (r sxsh::+reserved+) (push r names))
        (dolist (dir (split-string path #\:))
          (let ((d (if (string= dir "") "." dir)))
            (dolist (name (directory-entries d))
              (unless (or (string= name ".") (string= name ".."))
                (when (executable-p (concatenate 'string d "/" name))
                  (push name names))))))
        (setf *command-cache* (sort (remove-duplicates names :test #'string=)
                                    #'string<)
              *command-cache-path* path)))
    *command-cache*))

(defun command-candidates (sh word)
  (loop for name in (command-names sh)
        when (and (>= (length name) (length word))
                  (string= word name :end2 (length word)))
          collect (cons name " ")))

(defun variable-candidates (sh word)
  (let* ((bare (string-left-trim "${" word))
         (prefix (if (and (plusp (length word)) (char= (char word 0) #\$))
                     (subseq word 1)
                     word))
         (braced (and (> (length word) 1) (char= (char word 1) #\{)))
         (prefix (if braced bare prefix))
         (out '()))
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (and (>= (length k) (length prefix))
                          (string= prefix k :end2 (length prefix)))
                 (push (cons (concatenate 'string (if braced "${" "$") k)
                             (if braced "} " " "))
                       out)))
             (shell-vars sh))
    (sort out #'string< :key #'car)))

;;; ---------------------------------------------------------------------------
;;; Quoting
;;; ---------------------------------------------------------------------------

(defparameter +needs-escape+ " \\t|&;<>()$`\\\"'*?[]#~=%")

(defun escape-for-completion (text quote)
  "Make TEXT safe to insert at the cursor.

Only the INSERTED text is escaped -- SHELL-QUOTE is the wrong tool here, since
it wraps the whole string in single quotes and would re-quote the prefix the
user already typed. Inside an existing quote, nothing needs escaping."
  (if quote
      text
      (with-output-to-string (o)
        (loop for c across text
              do (when (find c +needs-escape+) (write-char #\\ o))
                 (write-char c o)))))

(defun common-prefix (strings)
  (if (null strings)
      ""
      (let ((p (first strings)))
        (dolist (s (rest strings) p)
          (let ((n (min (length p) (length s))))
            (setf p (subseq p 0 (or (mismatch p s :end1 n :end2 n) n))))))))

;;; ---------------------------------------------------------------------------
;;; The TAB command
;;; ---------------------------------------------------------------------------

(defun list-candidates (led names)
  "Print candidates in columns, then let the caller repaint the prompt."
  (let* ((cols (led-cols led))
         (widest (+ 2 (reduce #'max names :key #'length :initial-value 0)))
         (per-row (max 1 (floor cols widest))))
    (term-write (format nil "~C~C" #\Return #\Newline))
    (loop for name in names
          for i from 0
          do (term-write (format nil "~VA" widest name))
             (when (zerop (mod (1+ i) per-row))
               (term-write (format nil "~C~C" #\Return #\Newline))))
    (unless (zerop (mod (length names) per-row))
      (term-write (format nil "~C~C" #\Return #\Newline)))
    ;; The listing scrolled the prompt away; forget where we painted.
    (setf (led-rows-used led) 1 (led-cursor-row led) 0)))

(defun ed-complete (led sh key)
  (declare (ignore key))
  (multiple-value-bind (start end word quote kind)
      (completion-context (led-text led) (led-point led))
    (let* ((cands (ecase kind
                    (:command (command-candidates sh word))
                    (:file (file-candidates sh word))
                    (:variable (variable-candidates sh word))))
           (names (mapcar #'car cands)))
      (cond
        ((null cands) (term-beep))
        ((= 1 (length cands))
         (led-snapshot led)
         (let ((text (concatenate 'string
                                  (escape-for-completion (caar cands) quote)
                                  (cdar cands))))
           (setf (led-point led) start)
           (led-delete-region led start end)
           (setf (led-point led) start)
           (led-insert led text)))
        (t
         (let ((prefix (common-prefix names)))
           (if (> (length prefix) (length word))
               ;; Extend as far as everything agrees, and say nothing.
               (progn
                 (led-snapshot led)
                 (setf (led-point led) start)
                 (led-delete-region led start end)
                 (setf (led-point led) start)
                 (led-insert led (escape-for-completion prefix quote)))
               ;; Nothing more to add: a SECOND consecutive TAB lists.
               (when (eq (led-last-fn led) 'ed-complete)
                 (if (> (length names) +completion-list-threshold+)
                     (progn
                       (term-write
                        (format nil "~C~CDisplay all ~D possibilities? (y or n) "
                                #\Return #\Newline (length names)))
                       (term-flush)
                       (let ((k (read-key)))
                         (setf (led-rows-used led) 1 (led-cursor-row led) 0)
                         (when (and (characterp k) (char-equal k #\y))
                           (list-candidates led names))))
                     (list-candidates led names))))))))))
