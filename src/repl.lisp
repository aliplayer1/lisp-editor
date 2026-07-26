;;;; src/repl.lisp - inferior Lisp (REPL) state and stdio frame protocol

(in-package #:ted)

;;; ---------------------------------------------------------------
;;;  7a. Inferior Lisp (REPL): state and stdio frame protocol
;;; ---------------------------------------------------------------

(defconstant +frame-soh+ 1 "Start-of-frame control char from the shim.")
(defconstant +frame-stx+ 2 "End-of-frame control char from the shim.")

(defparameter *transcript-limit* 2000
  "Maximum transcript lines retained (newest first).")

(defstruct repl
  process                     ; sb-ext process object or NIL
  shim-path                   ; temp file holding the shim, for cleanup
  (visible nil)               ; pane shown?
  (dead nil)                  ; T after EOF/broken pipe
  (pending nil)               ; an evaluation is in flight
  (package "CL-USER")         ; prompt package, from the last P frame
  (transcript nil)            ; list of lines, NEWEST FIRST
  (line-acc (make-array 0 :element-type 'character
                          :adjustable t :fill-pointer t))
  (parse-state :text)         ; :text | :type | :body
  (frame-type nil)
  (frame-acc (make-array 0 :element-type 'character
                           :adjustable t :fill-pointer t))
  (input "")                  ; pane input line
  (input-col 0)
  (history nil)               ; submitted inputs, newest first
  (hist-index nil)
  (hist-stash ""))

(defvar *repl* nil "The single inferior-Lisp connection, or NIL.")
(defvar *focus* :edit "Which pane has the keyboard: :edit or :repl.")

(defun split-lines (text)
  "TEXT split on #\\Newline into a list of lines (no empties dropped)."
  (let ((acc nil) (start 0))
    (dotimes (i (length text))
      (when (char= (char text i) #\Newline)
        (push (subseq text start i) acc)
        (setf start (1+ i))))
    (push (subseq text start) acc)
    (nreverse acc)))

(defun first-line (s)
  (let ((p (position #\Newline s)))
    (if p (subseq s 0 p) s)))

(defun transcript-push (r line)
  (push line (repl-transcript r))
  (when (> (length (repl-transcript r)) *transcript-limit*)
    (setf (repl-transcript r)
          (subseq (repl-transcript r) 0 *transcript-limit*))))

(defun transcript-push-text (r text)
  (dolist (ln (split-lines text)) (transcript-push r ln)))

(defun repl-flush-line (r)
  (when (plusp (fill-pointer (repl-line-acc r)))
    (transcript-push r (coerce (repl-line-acc r) 'simple-string))
    (setf (fill-pointer (repl-line-acc r)) 0)))

(defun repl-finish-frame (r)
  "Interpret a completed frame.  Returns a flash string or NIL."
  (let ((type    (repl-frame-type r))
        (payload (coerce (repl-frame-acc r) 'simple-string)))
    (setf (fill-pointer (repl-frame-acc r)) 0
          (repl-parse-state r) :text
          (repl-frame-type r)  nil)
    (case type
      (#\P (setf (repl-package r) payload) nil)
      (#\R (setf (repl-pending r) nil)
       (repl-flush-line r)
       (transcript-push-text r (concatenate 'string "=> " payload))
       (format nil " => ~a" (first-line payload)))
      (#\E (setf (repl-pending r) nil)
       (repl-flush-line r)
       (transcript-push-text r (concatenate 'string "ERROR: " payload))
       (format nil " Error: ~a" (first-line payload)))
      (t nil))))

(defun repl-ingest-char (r ch)
  "Feed one child-output char into the parser.  Returns flash or NIL."
  (case (repl-parse-state r)
    (:text (cond
             ((char= ch (code-char +frame-soh+))
              (setf (repl-parse-state r) :type) nil)
             ((char= ch #\Newline) (repl-flush-line r) nil)
             ((char= ch #\Return) nil)
             (t (vector-push-extend ch (repl-line-acc r)) nil)))
    (:type (setf (repl-frame-type r) ch
                 (repl-parse-state r) :body)
     nil)
    (:body (if (char= ch (code-char +frame-stx+))
               (repl-finish-frame r)
               (progn (vector-push-extend ch (repl-frame-acc r)) nil)))))

(defun repl-ingest-string (r s)
  "Feed a whole string; returns the LAST flash produced, or NIL."
  (let ((flash nil))
    (loop for ch across s
          do (let ((f (repl-ingest-char r ch)))
               (when f (setf flash f))))
    flash))
