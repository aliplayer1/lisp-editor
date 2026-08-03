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

;;; ---------------------------------------------------------------
;;;  7b. Inferior Lisp: the shim and the child process
;;; ---------------------------------------------------------------

(defparameter *shim-forms*
  '((defun ted-pkg-name ()
      (let ((names (cons (package-name *package*)
                         (package-nicknames *package*))))
        (first (sort (copy-list names) #'< :key #'length))))
    (defun ted-shim-loop ()
      (loop
        (format t "~cP~a~c" (code-char 1) (ted-pkg-name) (code-char 2))
        (force-output)
        (let ((form (handler-case (read *standard-input* nil :ted-eof)
                      (error (e)
                        (clear-input)
                        (format t "~cE~a~c" (code-char 1) e (code-char 2))
                        (force-output)
                        :ted-read-error))))
          (cond
            ((eq form :ted-eof) (sb-ext:exit))
            ((eq form :ted-read-error) nil)
            (t
             (handler-case
                 (let ((vals (multiple-value-list (eval form))))
                   (force-output)
                   (format t "~cR~{~s~^, ~}~c" (code-char 1) vals (code-char 2)))
               ;; interactive-interrupt is a serious-condition but NOT an
               ;; error, so the clause below would not catch it: without
               ;; this one, --disable-debugger kills the child on C-c.
               (sb-sys:interactive-interrupt ()
                 (format t "~cEInterrupted~c" (code-char 1) (code-char 2)))
               (error (e)
                 (format t "~cE~a~c" (code-char 1) e (code-char 2))))
             (force-output))))))
    (ted-shim-loop))
  "The child's replacement toplevel, kept as data and printed to a temp
   file at launch.  See write-shim-file for the package-printing rule.")

(defun write-shim-file (path)
  (with-open-file (s path :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (with-standard-io-syntax
      ;; Print with *package* = TED so TED-homed symbols print bare and
      ;; the child (reading in its CL-USER) interns them locally.  This
      ;; LET must stay INSIDE with-standard-io-syntax, which binds
      ;; *package* to CL-USER itself.
      (let ((*package* (find-package :ted)))
        (dolist (form *shim-forms*)
          (prin1 form s)
          (terpri s))))))

(defun repl-reset-parser (r)
  (setf (repl-parse-state r) :text
        (repl-frame-type r) nil
        (fill-pointer (repl-frame-acc r)) 0
        (fill-pointer (repl-line-acc r)) 0
        (repl-pending r) nil
        (repl-dead r) nil))

(defun repl-start ()
  "Launch the child SBCL with the shim.  Reuses *repl* (keeping the
   transcript) across restarts.  Returns T on success, NIL if sbcl
   could not be started."
  (let ((r (or *repl* (setf *repl* (make-repl))))
        (path (format nil "/tmp/ted-shim-~d-~d.lisp"
                      (get-universal-time) (random 1000000))))
    (handler-case
        (progn
          (write-shim-file path)
          (setf (repl-process r)
                (sb-ext:run-program
                 "sbcl"
                 (list "--noinform" "--disable-debugger" "--load" path)
                 :search t :wait nil
                 :input :stream :output :stream :error :output))
          (setf (repl-shim-path r) path)
          (repl-reset-parser r)
          t)
      (error () nil))))

(defun repl-alive-p ()
  (let ((r *repl*))
    (and r (repl-process r) (not (repl-dead r))
         (eq (sb-ext:process-status (repl-process r)) :running))))

(defun repl-ensure-started ()
  (or (repl-alive-p) (repl-start)))

(defun repl-send (text)
  "Write TEXT as a line to the child's stdin.  Returns T, or NIL after
   marking the connection dead on a broken pipe.  Callers must have gone
   through repl-ensure-started: this assumes *repl* is non-NIL."
  (handler-case
      (let ((in (sb-ext:process-input (repl-process *repl*))))
        (write-line text in)
        (force-output in)
        t)
    (error () (setf (repl-dead *repl*) t) nil)))

(defun repl-drain ()
  "Read whatever the child has written, bounded per call so a
   fast-printing child cannot starve the UI.  Returns the last
   result/error flash produced, or NIL."
  (let ((r *repl*) (flash nil))
    (when (and r (repl-process r) (not (repl-dead r)))
      (let ((out (sb-ext:process-output (repl-process r))))
        (handler-case
            (loop repeat 65536
                  for ch = (read-char-no-hang out nil :eof)
                  while ch
                  do (if (eq ch :eof)
                         (progn (setf (repl-dead r) t)
                                (repl-flush-line r)
                                (transcript-push r "[repl process exited]")
                                (setf flash " REPL process died (C-x r restarts)")
                                (return))
                         (let ((f (repl-ingest-char r ch)))
                           (when f (setf flash f)))))
          (error () (setf (repl-dead r) t)))))
    flash))

(defun repl-interrupt ()
  "SIGINT the child; its shim reports the abort as an E frame."
  (when (repl-alive-p)
    (ignore-errors (sb-ext:process-kill (repl-process *repl*) 2))))

(defun repl-kill ()
  "Terminate the child (TERM + wait) and delete the shim temp file.
   Safe to call at any time, including with no child."
  (let ((r *repl*))
    (when (and r (repl-process r))
      (ignore-errors
        (when (eq (sb-ext:process-status (repl-process r)) :running)
          (sb-ext:process-kill (repl-process r) 15)
          (sb-ext:process-wait (repl-process r))))
      (setf (repl-dead r) t))
    (when (and r (repl-shim-path r))
      (ignore-errors (delete-file (repl-shim-path r)))
      (setf (repl-shim-path r) nil))))
