;;;; src/editing.lisp - insert/delete, undo/redo, auto-pair

(in-package #:ted)

;;; ---------------------------------------------------------------
;;;  5.  Editing
;;; ---------------------------------------------------------------

(defparameter *undo-limit* 200
  "Maximum number of snapshots retained on the undo stack.")

(defvar *last-cmd-was-insert* nil
  "T iff the previous handle-key call was a printable insert.  Used by
   insert-char to coalesce a run of single-char inserts into one undo
   step.  Set/cleared by the run loop based on the keycode.")

(defvar *last-cmd-was-delete* nil
  "T iff the previous handle-key call was a backward/forward delete.  Used
   by delete-backward and delete-forward to coalesce a run of single-char
   deletes into one undo step.  Set/cleared by the run loop based on the
   keycode.")

(defun snapshot ()
  "Capture the current buffer state as a tuple (LINES ROW COL DIRTY).
   Lines is COPY-LIST'd because set-cur-line mutates a cell in place
   via (setf (nth ...)); without the copy the snapshot would track the
   live buffer instead of the pre-edit state."
  (list (copy-list (buf-lines *buf*))
        (buf-row   *buf*)
        (buf-col   *buf*)
        (buf-dirty *buf*)))

(defun restore (s)
  "Restore the buffer state from a snapshot S."
  (setf (buf-lines *buf*) (first  s)
        (buf-row   *buf*) (second s)
        (buf-col   *buf*) (third  s)
        (buf-dirty *buf*) (fourth s)))

(defun record-undo ()
  "Push a snapshot of the current buffer state onto buf-undo, truncate
   the stack to *undo-limit* entries, and clear buf-redo (a fresh edit
   invalidates any pending redo)."
  (push (snapshot) (buf-undo *buf*))
  (when (> (length (buf-undo *buf*)) *undo-limit*)
    (setf (buf-undo *buf*) (subseq (buf-undo *buf*) 0 *undo-limit*)))
  (setf (buf-redo *buf*) nil))

(defun do-undo ()
  "Pop the top of buf-undo, save the current state onto buf-redo, and
   restore the popped snapshot.  Returns NIL on success, or a flash
   string if there is nothing to undo."
  (if (null (buf-undo *buf*))
      " Nothing to undo"
      (let ((s (pop (buf-undo *buf*))))
        (push (snapshot) (buf-redo *buf*))
        (restore s)
        nil)))

(defun do-redo ()
  "Pop the top of buf-redo, save the current state onto buf-undo, and
   restore the popped snapshot.  Returns NIL on success, or a flash
   string if there is nothing to redo."
  (if (null (buf-redo *buf*))
      " Nothing to redo"
      (let ((s (pop (buf-redo *buf*))))
        (push (snapshot) (buf-undo *buf*))
        (restore s)
        nil)))

(defun insert-char (ch)
  (unless *last-cmd-was-insert*
    (record-undo))
  (clear-mark)
  (let ((ln (cur-line)) (col (buf-col *buf*)))
    (set-cur-line (concatenate 'string
                               (subseq ln 0 col) (string ch) (subseq ln col)))
    (incf (buf-col *buf*))
    (setf (buf-dirty *buf*) t))
  (setf *last-cmd-was-insert* t))

(defun close-paren-first-p (s)
  "True if the first non-whitespace character of S is a close paren."
  (let ((trimmed (string-left-trim '(#\Space #\Tab) s)))
    (and (plusp (length trimmed)) (char= (char trimmed 0) #\)))))

(defun insert-newline ()
  (record-undo)
  (clear-mark)
  (let* ((ln   (cur-line))
         (col  (buf-col *buf*))
         (row  (buf-row *buf*))
         (head (subseq ln 0 col))
         (tail (subseq ln col)))
    ;; Check if the cursor is currently inside a string or block comment literal
    (multiple-value-bind (st-str st-blk) (state-at-line row)
      (multiple-value-bind (toks after-str after-blk)
          (tokenize-line head st-str st-blk)
        (declare (ignore toks))
        (let ((in-literal (or after-str (plusp after-blk))))
          ;; Standard newline execution for both normal text and parenthesis
          (set-cur-line head)
          (setf (buf-lines *buf*)
                (append (subseq (buf-lines *buf*) 0 (1+ row))
                        (list tail)
                        (subseq (buf-lines *buf*) (1+ row))))
          (incf (buf-row *buf*))
          (setf (buf-col *buf*) 0 (buf-dirty *buf*) t)
          ;; Calculate regular Lisp indentation unless we are inside a literal string/comment
          (unless in-literal
            (multiple-value-bind (orow ocol)
                (enclosing-open-paren (buf-row *buf*) 0)
              (when orow
                (let ((n (compute-newline-indent orow ocol)))
                  (when (plusp n)
                    (set-cur-line
                     (concatenate 'string
                                  (make-string n :initial-element #\Space)
                                  (cur-line)))
                    (setf (buf-col *buf*) n)))))))))))


(defun delete-backward ()
  (unless *last-cmd-was-delete*
    (record-undo))
  (clear-mark)
  (let ((col (buf-col *buf*)) (row (buf-row *buf*)))
    (cond
      ;; empty auto-pair straddling the cursor: delete both delimiters
      ((and (> col 0)
            (let ((ln (cur-line)))
              (and (< col (length ln))
                   (eql (pair-closer (char ln (1- col))) (char ln col)))))
       (let ((ln (cur-line)))
         (set-cur-line (concatenate 'string
                                    (subseq ln 0 (1- col)) (subseq ln (1+ col))))
         (decf (buf-col *buf*))))
      ((> col 0)
       (let ((ln (cur-line)))
         (set-cur-line (concatenate 'string
                                    (subseq ln 0 (1- col)) (subseq ln col)))
         (decf (buf-col *buf*))))
      ((> row 0)
       (let* ((prev (nth (1- row) (buf-lines *buf*)))
              (nc   (length prev)))
         (setf (buf-lines *buf*)
               (append (subseq (buf-lines *buf*) 0 (1- row))
                       (list (concatenate 'string prev (cur-line)))
                       (subseq (buf-lines *buf*) (1+ row))))
         (decf (buf-row *buf*))
         (setf (buf-col *buf*) nc))))
    (setf (buf-dirty *buf*) t)))

(defun delete-forward ()
  (unless *last-cmd-was-delete*
    (record-undo))
  (clear-mark)
  (let* ((ln (cur-line)) (col (buf-col *buf*)) (row (buf-row *buf*)))
    (cond
      ((< col (length ln))
       (set-cur-line (concatenate 'string
                                  (subseq ln 0 col) (subseq ln (1+ col)))))
      ((< row (1- (line-count)))
       (set-cur-line (concatenate 'string ln (nth (1+ row) (buf-lines *buf*))))
       (setf (buf-lines *buf*)
             (append (subseq (buf-lines *buf*) 0 (1+ row))
                     (subseq (buf-lines *buf*) (+ row 2))))))
    (setf (buf-dirty *buf*) t)))

;;; ---------------------------------------------------------------
;;;  5b. Auto-pair (electric parens and quotes)
;;; ---------------------------------------------------------------

(defparameter *auto-pairs* '((#\( . #\)) (#\" . #\"))
  "Opening delimiters that auto-insert their closer.  `'` is deliberately
   excluded: 'foo is pervasive in Lisp and pairing it would fight the user.")

(defun pair-closer (open)
  "Closer paired with OPEN, or NIL if OPEN is not an auto-pair opener."
  (cdr (assoc open *auto-pairs* :test #'char=)))

(defun closer-char-p (ch)
  "True if CH is the closing delimiter of some auto-pair."
  (member ch *auto-pairs* :key #'cdr :test #'char=))

(defun in-literal-p (row col)
  "True if (ROW, COL) sits inside a string, comment, or #\\X char literal
   per the tokenizer.  Auto-pairing is suppressed there so typed
   delimiters stay literal."
  (let ((line (nth row (buf-lines *buf*))))
    (when line
      (multiple-value-bind (st-str st-blk) (state-at-line row)
        (or st-str
            (plusp st-blk)
            (paren-skippable-p col (tokenize-line line st-str st-blk)))))))

(defun char-at-cursor ()
  "Character under the cursor, or NIL at end of line."
  (let ((ln (cur-line)) (col (buf-col *buf*)))
    (when (< col (length ln)) (char ln col))))

(defun region-active-p ()
  "True if a non-empty region is selected."
  (multiple-value-bind (sr sc er ec) (region-bounds)
    (and sr (not (and (= sr er) (= sc ec))))))

(defun insert-pair (open close)
  "Insert OPEN immediately followed by CLOSE, leaving point between them."
  (let ((ln (cur-line)) (col (buf-col *buf*)))
    (set-cur-line (concatenate 'string
                               (subseq ln 0 col)
                               (string open) (string close)
                               (subseq ln col)))
    (incf (buf-col *buf*))
    (setf (buf-dirty *buf*) t)))

(defun wrap-region (open close)
  "Surround the active region with OPEN..CLOSE; point lands just past the
   inserted CLOSE.  Assumes a region is active (read under region-bounds)."
  (multiple-value-bind (sr sc er ec) (region-bounds)
    ;; insert CLOSE first so inserting OPEN can't shift its index
    (let ((eln (nth er (buf-lines *buf*))))
      (setf (nth er (buf-lines *buf*))
            (concatenate 'string (subseq eln 0 ec)
                         (string close) (subseq eln ec))))
    (let ((sln (nth sr (buf-lines *buf*))))
      (setf (nth sr (buf-lines *buf*))
            (concatenate 'string (subseq sln 0 sc)
                         (string open) (subseq sln sc))))
    (setf (buf-row *buf*) er
          ;; on a one-line region the OPEN insertion bumped CLOSE right by 1
          (buf-col *buf*) (if (= sr er) (+ ec 2) (1+ ec))
          (buf-dirty *buf*) t)))

(defun self-insert (ch)
  "Insert CH with electric-pair behavior for the delimiters in
   *auto-pairs*: step over an existing closer, wrap an active region,
   auto-insert a matching closer, else insert literally.  Pairing is
   suppressed inside strings, comments, and char literals."
  (let ((literal (in-literal-p (buf-row *buf*) (buf-col *buf*)))
        (region  (region-active-p))
        (closer  (pair-closer ch)))
    (cond
      ;; type a closer right where one already sits -> step over it
      ((and (closer-char-p ch) (eql (char-at-cursor) ch))
       (incf (buf-col *buf*)))

      ;; opener with an active region -> wrap the selection
      ((and closer region (not literal))
       (record-undo)
       (wrap-region ch closer)
       (clear-mark))

      ;; opener in code -> auto-insert the closer, point between
      ((and closer (not literal))
       (unless *last-cmd-was-insert* (record-undo))
       (clear-mark)
       (insert-pair ch closer))

      ;; everything else (incl. delimiters inside literals) -> plain insert
      (t (insert-char ch)))))

