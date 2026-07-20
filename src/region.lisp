;;;; src/region.lisp - mark, selection, kill ring

(in-package #:ted)

;;; ---------------------------------------------------------------
;;;  5a. Selection + kill ring
;;; ---------------------------------------------------------------

(defun set-mark ()
  "Anchor the selection at the current cursor.  Clears the shift-origin flag,
   so a Ctrl-Space selection is Emacs-style (plain movement extends it)."
  (setf (buf-mark-row *buf*) (buf-row *buf*)
        (buf-mark-col *buf*) (buf-col *buf*)
        (buf-mark-shift *buf*) nil))

(defun ensure-mark ()
  "Set the mark at point only if no selection is currently active."
  (unless (buf-mark-row *buf*) (set-mark)))

(defun clear-mark ()
  "Discard any active selection."
  (setf (buf-mark-row *buf*) nil
        (buf-mark-col *buf*) nil
        (buf-mark-shift *buf*) nil))

(defun region-bounds ()
  "Return (values start-row start-col end-row end-col) with start <= end,
   or NIL if no mark is set.  An empty region (mark at cursor) returns four
   equal values; callers guard emptiness themselves."
  (let ((mr (buf-mark-row *buf*))
        (mc (buf-mark-col *buf*))
        (cr (buf-row *buf*))
        (cc (buf-col *buf*)))
    (when (and mr mc)
      (if (or (< mr cr) (and (= mr cr) (< mc cc)))
          (values mr mc cr cc)
          (values cr cc mr mc)))))

(defun region-text ()
  "Return the selected text as one string with embedded #\\Newline.
   Returns NIL when no region is active."
  (multiple-value-bind (sr sc er ec) (region-bounds)
    (when sr
      (cond
        ((= sr er)
         (subseq (nth sr (buf-lines *buf*)) sc ec))
        (t
         (with-output-to-string (out)
           (write-string (subseq (nth sr (buf-lines *buf*)) sc) out)
           (loop for r from (1+ sr) below er do
             (terpri out)
             (write-string (nth r (buf-lines *buf*)) out))
           (terpri out)
           (write-string (subseq (nth er (buf-lines *buf*)) 0 ec) out)))))))

(defun delete-region ()
  "Remove the selected range from the buffer.  Cursor lands at the
   region's start.  Clears the mark.  No-op if no region is active or
   the region is empty."
  (multiple-value-bind (sr sc er ec) (region-bounds)
    (when (and sr (not (and (= sr er) (= sc ec))))
      (let* ((lines  (buf-lines *buf*))
             (start  (nth sr lines))
             (end    (nth er lines))
             (joined (concatenate 'string
                                  (subseq start 0 sc)
                                  (subseq end ec))))
        (setf (buf-lines *buf*)
              (append (subseq lines 0 sr)
                      (list joined)
                      (subseq lines (1+ er))))
        (setf (buf-row *buf*) sr
              (buf-col *buf*) sc
              (buf-dirty *buf*) t)))
    (clear-mark)))

(defparameter *kill-ring-limit* 60
  "Maximum number of entries retained on *kill-ring*.")

(defvar *kill-ring* nil
  "Most-recent-first list of killed strings.  Capped at *kill-ring-limit*.")

(defvar *last-cmd-was-kill* nil
  "T if the previous dispatched command was a kill operation.  Used to
   coalesce consecutive kills into a single ring entry.")

(defun ring-push (s)
  "Push S onto *kill-ring*, truncating to *kill-ring-limit*."
  (push s *kill-ring*)
  (when (> (length *kill-ring*) *kill-ring-limit*)
    (setf *kill-ring* (subseq *kill-ring* 0 *kill-ring-limit*))))

(defun kill-region ()
  "Push the selected text onto *kill-ring* and delete it.  No-op if no
   region is active or it is empty.  Sets *last-cmd-was-kill* to T on
   success."
  (let ((text (region-text)))
    (cond
      ((or (null text) (zerop (length text)))
       (clear-mark)
       " Region empty")
      (t
       (record-undo)
       (ring-push text)
       (delete-region)
       (setf *last-cmd-was-kill* t)
       nil))))

(defun copy-region ()
  "Push the selected text onto *kill-ring* without deleting it.
   No-op if no region is active or it is empty.  Clears the mark."
  (let ((text (region-text)))
    (cond
      ((or (null text) (zerop (length text)))
       (clear-mark)
       " Region empty")
      (t
       (ring-push text)
       (clear-mark)
       (setf *last-cmd-was-kill* t)
       nil))))

(defun kill-line ()
  "Kill from cursor to end of line.  At EOL, kill the trailing newline
   (joins the next line onto this one).  At the EOL of the last line of
   the buffer, no-op.  Consecutive kill-line calls append onto the
   same kill-ring entry instead of pushing a new one."
  (let* ((ln  (cur-line))
         (col (buf-col *buf*))
         (row (buf-row *buf*))
         (eol (= col (length ln))))
    (cond
      ;; mid-line: take the tail
      ((not eol)
       (record-undo)
       (let ((tail (subseq ln col)))
         (set-cur-line (subseq ln 0 col))
         (if *last-cmd-was-kill*
             (setf (first *kill-ring*)
                   (concatenate 'string (first *kill-ring*) tail))
             (ring-push tail))
         (setf (buf-dirty *buf*) t
               *last-cmd-was-kill* t)
         nil))
      ;; EOL but not on last line: kill the newline by joining
      ((< row (1- (line-count)))
       (record-undo)
       (let ((next (nth (1+ row) (buf-lines *buf*))))
         (set-cur-line (concatenate 'string ln next))
         (setf (buf-lines *buf*)
               (append (subseq (buf-lines *buf*) 0 (1+ row))
                       (subseq (buf-lines *buf*) (+ row 2))))
         (if *last-cmd-was-kill*
             (setf (first *kill-ring*)
                   (concatenate 'string (first *kill-ring*)
                                (string #\Newline)))
             (ring-push (string #\Newline)))
         (setf (buf-dirty *buf*) t
               *last-cmd-was-kill* t)
         nil))
      ;; EOL of last line: nothing to kill
      (t nil))))

(defun yank ()
  "Splice (first *kill-ring*) at the cursor.  Multi-line entries split
   the current line and insert new rows.  Returns a flash string when
   the kill ring is empty."
  (cond
    ((null *kill-ring*) " Kill ring empty")
    (t
     (record-undo)
     (let* ((text  (first *kill-ring*))
            (parts (let ((acc nil) (start 0))
                     (dotimes (i (length text))
                       (when (char= (char text i) #\Newline)
                         (push (subseq text start i) acc)
                         (setf start (1+ i))))
                     (push (subseq text start) acc)
                     (nreverse acc)))
            (ln    (cur-line))
            (col   (buf-col *buf*))
            (row   (buf-row *buf*))
            (head  (subseq ln 0 col))
            (tail  (subseq ln col)))
       (cond
         ;; single-line yank: parts has one chunk, no newlines
         ((= 1 (length parts))
          (set-cur-line (concatenate 'string head (first parts) tail))
          (setf (buf-col *buf*) (+ col (length (first parts)))))
         ;; multi-line yank: head joins first chunk; middle chunks
         ;; become their own lines; tail joins last chunk
         (t
          (let* ((first-part (first parts))
                 (last-part  (car (last parts)))
                 (middle     (butlast (rest parts))))
            (setf (buf-lines *buf*)
                  (append (subseq (buf-lines *buf*) 0 row)
                          (list (concatenate 'string head first-part))
                          middle
                          (list (concatenate 'string last-part tail))
                          (subseq (buf-lines *buf*) (1+ row))))
            (setf (buf-row *buf*) (+ row (1- (length parts)))
                  (buf-col *buf*) (length last-part)))))
       (setf (buf-dirty *buf*) t)
       nil))))

