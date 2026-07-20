;;;; src/movement.lisp - cursor and word motion

(in-package #:ted)

;;; ---------------------------------------------------------------
;;;  6.  Movement
;;; ---------------------------------------------------------------

(defun move-up    () (when (> (buf-row *buf*) 0) (decf (buf-row *buf*))) (clamp-col))
(defun move-down  () (when (< (buf-row *buf*) (1- (line-count))) (incf (buf-row *buf*))) (clamp-col))
(defun move-left  ()
  (if (> (buf-col *buf*) 0) (decf (buf-col *buf*))
      (when (> (buf-row *buf*) 0)
        (decf (buf-row *buf*)) (setf (buf-col *buf*) (length (cur-line))))))
(defun move-right ()
  (if (< (buf-col *buf*) (length (cur-line))) (incf (buf-col *buf*))
      (when (< (buf-row *buf*) (1- (line-count)))
        (incf (buf-row *buf*)) (setf (buf-col *buf*) 0))))
(defun move-home  () (setf (buf-col *buf*) 0))
(defun move-end   () (setf (buf-col *buf*) (length (cur-line))))
(defun page-up    (n)
  (setf (buf-row *buf*) (max 0 (- (buf-row *buf*) n)))
  (clamp-col))
(defun page-down  (n)
  (setf (buf-row *buf*) (min (1- (line-count)) (+ (buf-row *buf*) n)))
  (clamp-col))

(defun goto-line-number (n)
  "Move point to 1-based line N, clamped to the buffer's line range.
   Column is clamped to the destination line's length.  Returns NIL."
  (setf (buf-row *buf*) (clamp (1- n) 0 (1- (line-count))))
  (clamp-col)
  nil)

(defun goto-line ()
  "Prompt for a 1-based line number on the status bar and jump to it.
   Returns NIL on success or an empty entry, or a flash string when the
   entry is not a positive integer."
  (let ((s (mini-prompt "Go to line: ")))
    (cond
      ((or (null s) (zerop (length s))) nil)
      (t (let ((n (parse-integer s :junk-allowed t)))
           (if (and n (plusp n))
               (goto-line-number n)
               " Bad line number"))))))

;;; ----- Word-wise motion and deletion -----------------------------
;;; A "word" is a maximal run of symbol-char-p characters, so Lisp
;;; identifiers like foo-bar and list->vector move and delete as one
;;; unit.  Motion crosses line boundaries, treating the line break as a
;;; word separator, the way Emacs M-f / M-b do.

(defun move-word-forward ()
  "Move point to the end of the next word."
  (let ((nlines (line-count)))
    ;; skip non-word characters (and line breaks) up to the next word
    (loop
      (let ((line (cur-line)) (col (buf-col *buf*)))
        (cond
          ((>= col (length line))
           (if (< (buf-row *buf*) (1- nlines))
               (setf (buf-row *buf*) (1+ (buf-row *buf*)) (buf-col *buf*) 0)
               (return-from move-word-forward)))
          ((symbol-char-p (char line col)) (return))
          (t (incf (buf-col *buf*))))))
    ;; consume the word itself (never crosses the line break)
    (loop
      (let ((line (cur-line)) (col (buf-col *buf*)))
        (if (and (< col (length line)) (symbol-char-p (char line col)))
            (incf (buf-col *buf*))
            (return))))))

(defun move-word-backward ()
  "Move point to the start of the previous word."
  ;; skip non-word characters (and line breaks) back to the prior word
  (loop
    (let ((col (buf-col *buf*)))
      (cond
        ((<= col 0)
         (if (> (buf-row *buf*) 0)
             (progn
               (decf (buf-row *buf*))
               (setf (buf-col *buf*) (length (cur-line))))
             (return-from move-word-backward)))
        ((symbol-char-p (char (cur-line) (1- col))) (return))
        (t (decf (buf-col *buf*))))))
  ;; consume the word itself (never crosses the line break)
  (loop
    (let ((col (buf-col *buf*)))
      (if (and (> col 0) (symbol-char-p (char (cur-line) (1- col))))
          (decf (buf-col *buf*))
          (return)))))

(defun kill-word-forward ()
  "Kill from point to the end of the next word, appending to the current
   kill-ring entry on consecutive kills.  No-op at end of buffer.  Returns
   NIL."
  (let ((sr (buf-row *buf*)) (sc (buf-col *buf*)))
    (move-word-forward)
    (when (or (/= sr (buf-row *buf*)) (/= sc (buf-col *buf*)))
      (setf (buf-mark-row *buf*) sr (buf-mark-col *buf*) sc)
      (let ((text (region-text)))
        (record-undo)
        (if *last-cmd-was-kill*
            (setf (first *kill-ring*)
                  (concatenate 'string (first *kill-ring*) text))
            (ring-push text))
        (delete-region)
        (setf *last-cmd-was-kill* t))))
  nil)

(defun kill-word-backward ()
  "Kill from the start of the previous word to point, prepending to the
   current kill-ring entry on consecutive kills.  No-op at start of buffer.
   Returns NIL."
  (let ((er (buf-row *buf*)) (ec (buf-col *buf*)))
    (move-word-backward)
    (when (or (/= er (buf-row *buf*)) (/= ec (buf-col *buf*)))
      (setf (buf-mark-row *buf*) er (buf-mark-col *buf*) ec)
      (let ((text (region-text)))
        (record-undo)
        (if *last-cmd-was-kill*
            (setf (first *kill-ring*)
                  (concatenate 'string text (first *kill-ring*)))
            (ring-push text))
        (delete-region)
        (setf *last-cmd-was-kill* t))))
  nil)

;;; ----- Shift-select: extend the region with Shift + movement --------
;;; The shifted movement keycodes are discovered at startup from terminfo
;;; (tigetstr + key_defined), so nothing is hardcoded and a terminal that
;;; lacks a capability simply leaves that key unbound.

(defun shift-move (mover)
  "Extend a Shift-selection: anchor the mark at point if none is set (tagging
   the selection as shift-started), then run MOVER so the region drags with the
   cursor.  Returns NIL."
  (let ((had-mark (buf-mark-row *buf*)))
    (ensure-mark)
    (unless had-mark (setf (buf-mark-shift *buf*) t)))
  (funcall mover)
  nil)

(defun plain-move (mover)
  "Run an unshifted movement.  A Shift-started selection ends first (plain
   movement collapses it); a Ctrl-Space selection is left intact so it keeps
   extending Emacs-style.  Returns NIL."
  (when (buf-mark-shift *buf*) (clear-mark))
  (funcall mover)
  nil)

(defvar *shift-keys* nil
  "Alist of (keycode . mover) for the shifted movement keys, built by
   setup-shift-keys at startup.  Empty until then (e.g. under tests).")

(defun terminfo-key-code (capname)
  "The keycode ncurses assigns to terminfo string capability CAPNAME on this
   terminal, or NIL if the capability is absent or unbound.  Must run after
   keypad is enabled.  All CAPNAMEs used here are string capabilities, so
   tigetstr returns a null pointer (not the (char *)-1 sentinel) when absent."
  (let ((seq (%tigetstr capname)))
    (unless (null-pointer-p seq)
      (let ((code (%key-defined (foreign-string-to-lisp seq))))
        (when (> code 0) code)))))

(defun setup-shift-keys ()
  "Discover the shifted movement keycodes and build *shift-keys*.  A key whose
   capability is missing on this terminal is simply left unbound."
  (setf *shift-keys*
        (loop for (cap . mover) in
              (list (cons "kLFT" #'move-left)
                    (cons "kRIT" #'move-right)
                    (cons "kri"  #'move-up)
                    (cons "kind" #'move-down)
                    (cons "kHOM" #'move-home)
                    (cons "kEND" #'move-end)
                    (cons "kPRV" (lambda () (page-up   (- (rows) 3))))
                    (cons "kNXT" (lambda () (page-down (- (rows) 3)))))
              for code = (terminfo-key-code cap)
              when code collect (cons code mover))))

