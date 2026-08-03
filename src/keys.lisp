;;;; src/keys.lisp - mini-prompt, meta dispatch, key dispatch

(in-package #:ted)

;;; ---------------------------------------------------------------
;;;  9.  Mini-prompt on status bar
;;; ---------------------------------------------------------------

(defun mini-prompt (prompt-str)
  ;; Force blocking reads: the main loop leaves a 100 ms timeout set
  ;; while a child Lisp is alive, which would spin this loop redrawing
  ;; the prompt ten times a second.  read-c-x-key does the same.
  (%timeout -1)
  (let ((input ""))
    (loop
      (%move (1- (rows)) 0)
      (%attron +a-reverse+)
      (addstr-fit (concatenate 'string prompt-str input "_"
                               (make-string (cols) :initial-element #\Space))
                  (cols))
      (%attroff +a-reverse+)
      (%refresh)
      (let ((k (%getch)))
        (cond
          ((= k 27)  (return nil))
          ((or (= k 10) (= k 13)) (return input))
          ((or (= k 127) (= k 8) (= k +key-backspace+))
           (when (plusp (length input))
             (setf input (subseq input 0 (1- (length input))))))
          ((and (>= k 32) (< k 127))
           (setf input (concatenate 'string input (string (code-char k))))))))))

;;; ---------------------------------------------------------------
;;;  9a. Meta-key dispatch
;;; ---------------------------------------------------------------

(defun read-key ()
  "Wrap %getch so an Esc (27) followed within ~50 ms by another key is
   returned as the cons (27 . K2) for Meta-prefix bindings.  A lone Esc
   returns 27.  Restores blocking mode before returning."
  (let ((k (%getch)))
    (cond
      ((= k 27)
       (%timeout 50)
       (let ((k2 (%getch)))
         (%timeout -1)
         (if (= k2 -1) 27 (cons 27 k2))))
      (t k))))

;;; ---------------------------------------------------------------
;;;  10. Key dispatch
;;; ---------------------------------------------------------------

(defun key-name (k)
  "Human-readable name of keycode K for flash messages"
  (cond
    ((and (>= k 32) (< k 127)) (string (code-char k)))
    ((and (>= k 1) (<= k 26))  (format nil "C-~a" (code-char (+ k 96))))
    (t (format nil "#~a" k))))

(defun dispatch-c-x (k2)
  "Dispatch the key typed adter the C-x prefix. Returns a handle-key
   style result. Late features add clauses here (C-x C-e, ...)"
  (cond
    ((eql k2 +ctrl-e+) (eval-last-sexp))
    ((eql k2 +ctrl-g+) (repl-interrupt-cmd))
    ((eql k2 27) nil)
    (t (format nil " C-x ~a is undefined" (key-name k2)))))

(defun read-c-x-key ()
  "Show the C-x- prefix on the status bar, read the followup key
  blocking, and dispatch it."
  (%move (1- (rows)) 0)
  (%attron +a-reverse+)
  (addstr-fit (concatenate 'string " C-x-"
                           (make-string (cols) :initial-element #\Space))
              (cols))
  (%attroff +a-reverse+)
  (%refresh)
  (%timeout -1)
  (dispatch-c-x (%getch)))

(defun handle-key (k)
  (cond
    ;; Meta-prefix: (cons 27 . k2)
    ((consp k)
     (cond
       ((eql (cdr k) (char-code #\w))
        (copy-region))
       ((eql (cdr k) (char-code #\g))
        (goto-line))
       ((eql (cdr k) (char-code #\f))
        (move-word-forward) nil)
       ((eql (cdr k) (char-code #\b))
        (move-word-backward) nil)
       ((eql (cdr k) (char-code #\d))
        (kill-word-forward))
       ((eql (cdr k) (char-code #\)))
        (slurp-forward))
       ((eql (cdr k) (char-code #\}))
        (barf-forward))
       ((or (eql (cdr k) 127) (eql (cdr k) 8)
            (eql (cdr k) +key-backspace+))
        (kill-word-backward))
       (t nil)))

    ((= k +ctrl-q+)
     (if (buf-dirty *buf*)
         (let ((a (mini-prompt "Unsaved changes - quit? (y/n): ")))
           (if (and a (string-equal a "y")) :quit nil))
         :quit))

    ((= k +ctrl-s+)
     (let ((p (or (buf-filename *buf*) (mini-prompt "Save as: "))))
       (when p
         (multiple-value-bind (ok err) (save-file p)
           (if ok (format nil " Saved: ~a" p)
               (format nil " ERROR: ~a" err))))))

    ((= k +ctrl-o+)
     (let ((a (if (buf-dirty *buf*)
                  (mini-prompt "Discard changes? (y/n): ") "y")))
       (when (and a (string-equal a "y"))
         (let ((p (mini-prompt "Open file: ")))
           (when p
             (multiple-value-bind (ok err) (load-file p)
               (declare (ignore ok))
               (format nil " ~a~@[ (~a)~]" p err)))))))

    ((= k +ctrl-n+)
     (let ((a (if (buf-dirty *buf*)
                  (mini-prompt "Discard changes? (y/n): ") "y")))
       (when (and a (string-equal a "y"))
         (setf *buf* (make-buf)) " [new buffer]")))

    ;; --- Stage 5 selection / kill / yank ---

    ((= k +ctrl-space+) (set-mark) " Mark set")
    ((= k +ctrl-w+)     (kill-region))
    ((= k +ctrl-y+)     (yank))
    ((= k +ctrl-k+)     (kill-line))
    ((= k +ctrl-g+)     (clear-mark) " Mark cleared")
    ((= k +ctrl-x+)     (read-c-x-key))

    ((= k +key-up+)    (plain-move #'move-up))
    ((= k +key-down+)  (plain-move #'move-down))
    ((= k +key-left+)  (plain-move #'move-left))
    ((= k +key-right+) (plain-move #'move-right))
    ((or (= k +key-home+) (= k +ctrl-a+))  (plain-move #'move-home))
    ((or (= k +key-end+)  (= k +ctrl-e+))  (plain-move #'move-end))
    ((= k +key-ppage+) (plain-move (lambda () (page-up   (- (rows) 3)))))
    ((= k +key-npage+) (plain-move (lambda () (page-down (- (rows) 3)))))

    ;; Shift + movement extends the selection (keycodes discovered at startup)
    ((and (integerp k) (assoc k *shift-keys*))
     (shift-move (cdr (assoc k *shift-keys*))))

    ((= k +ctrl-underscore+) (do-undo))
    ((= k +ctrl-r+)          (do-redo))

    ((= k 9) (reindent-line))

    ((or (= k 10) (= k 13))               (insert-newline)   nil)
    ((or (= k 127) (= k 8)
         (= k +key-backspace+))            (delete-backward) nil)
    ((or (= k +key-dc+) (= k +ctrl-d+))   (delete-forward)  nil)

    ((and (>= k 32) (< k 127))
     (self-insert (code-char k)) nil)

    (t nil)))
