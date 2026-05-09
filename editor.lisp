;;;; editor.lisp — Small terminal text editor.
;;;;              Uses ncurses directly through CFFI.  No cl-charms.
;;;;
;;;; Requirements:
;;;;   - SBCL
;;;;   - Quicklisp  (for CFFI)
;;;;   - libncursesw  (sudo apt install libncurses-dev)
;;;;
;;;; Load at the REPL on the Terminal, not from inside Emacs:
;;;;   (ql:quickload "cffi")
;;;;   (load "editor.lisp")
;;;;
;;;; Or from the Terminal:
;;;;   sbcl --load editor.lisp
;;;;   sbcl --load editor.lisp -- myfile.txt
;;;;
;;;; Keys:
;;;;   Arrows / Home / End / PgUp / PgDn        navigation
;;;;   Ctrl-A / Ctrl-E                          line start / end
;;;;   Ctrl-S   save          Ctrl-O   open file
;;;;   Ctrl-N   new buffer    Ctrl-Q   quit
;;;;   Enter    newline       Backspace / Del / Ctrl-D   delete

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cffi" :silent t))

(defpackage #:ted
  (:use #:cl #:cffi))

(in-package #:ted)

;;; ---------------------------------------------------------------
;;;  1.  Load libncurses
;;; ---------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((loaded nil))
    (dolist (name '("libncursesw.so.6" "libncurses.so.6"
                    "libncursesw.so.5" "libncurses.so.5"
                    "libncursesw.so"   "libncurses.so"))
      (unless loaded
        (handler-case
            (progn (load-foreign-library name) (setf loaded t))
          (load-foreign-library-error ()))))
    (unless loaded
      (error "Cannot find libncurses. Run: sudo apt install libncurses-dev"))))

;;; ---------------------------------------------------------------
;;;  2.  C function declarations
;;;      Each name is a real ncurses export; check with:
;;;        nm -D /usr/lib/x86_64-linux-gnu/libncursesw.so.6 | grep ' T '
;;; ---------------------------------------------------------------

(defcfun ("initscr"  %initscr)  :pointer)
(defcfun ("endwin"   %endwin)   :int)
(defcfun ("refresh"  %refresh)  :int)
(defcfun ("clear"    %clear)    :int)
(defcfun ("noecho"   %noecho)   :int)
(defcfun ("cbreak"   %cbreak)   :int)
(defcfun ("keypad"   %keypad)   :int (win :pointer) (bf :int))
(defcfun ("move"     %move)     :int (y :int) (x :int))
(defcfun ("addch"    %addch)    :int (ch :unsigned-int))
(defcfun ("addstr"   %addstr)   :int (s :string))
(defcfun ("clrtoeol" %clrtoeol) :int)
(defcfun ("getch"    %getch)    :int)
(defcfun ("getmaxy"  %getmaxy)  :int (win :pointer))
(defcfun ("getmaxx"  %getmaxx)  :int (win :pointer))
(defcfun ("attron"   %attron)   :int (a :unsigned-long))
(defcfun ("attroff"  %attroff)  :int (a :unsigned-long))

;;; ncurses key codes (octal, same in every ncurses version)
(defconstant +key-up+        #o403)
(defconstant +key-down+      #o402)
(defconstant +key-left+      #o404)
(defconstant +key-right+     #o405)
(defconstant +key-home+      #o406)
(defconstant +key-end+       #o550)
(defconstant +key-ppage+     #o523)
(defconstant +key-npage+     #o522)
(defconstant +key-dc+        #o512)   ; forward-delete key
(defconstant +key-backspace+  #o407)
(defconstant +a-reverse+     #x00040000)

(defconstant +ctrl-a+  1)
(defconstant +ctrl-d+  4)
(defconstant +ctrl-e+  5)
(defconstant +ctrl-n+ 14)
(defconstant +ctrl-o+ 15)
(defconstant +ctrl-q+ 17)
(defconstant +ctrl-s+ 19)

(defvar *stdscr* (null-pointer))
(defun rows () (%getmaxy *stdscr*))
(defun cols () (%getmaxx *stdscr*))

(defun addstr-fit (s max-cols)
  (%addstr (subseq s 0 (min (length s) max-cols))))

(defun addstr-window (s start width)
  "Draw a horizontal slice of S of length WIDTH starting at column START."
  (let* ((from (min start (length s)))
         (to   (min (length s) (+ from width))))
    (%addstr (subseq s from to))))

;;; ---------------------------------------------------------------
;;;  3.  Buffer
;;; ---------------------------------------------------------------

(defstruct buf
  (lines (list "") :type list)
  (row   0        :type fixnum)
  (col   0        :type fixnum)
  (top   0        :type fixnum)
  (left  0        :type fixnum)
  (filename nil)
  (dirty    nil))

(defvar *buf* (make-buf))

(defun cur-line     ()    (nth (buf-row *buf*) (buf-lines *buf*)))
(defun set-cur-line (s)   (setf (nth (buf-row *buf*) (buf-lines *buf*)) s))
(defun line-count   ()    (length (buf-lines *buf*)))
(defun clamp (v lo hi)    (max lo (min hi v)))
(defun clamp-col    ()
  (setf (buf-col *buf*) (clamp (buf-col *buf*) 0 (length (cur-line)))))

;;; ---------------------------------------------------------------
;;;  4.  File I/O
;;; ---------------------------------------------------------------

(defun load-file (path)
  (handler-case
      (with-open-file (s path)
        (let ((lines (loop for ln = (read-line s nil nil) while ln collect ln)))
          (setf (buf-lines    *buf*) (or lines (list ""))
                (buf-row      *buf*) 0
                (buf-col      *buf*) 0
                (buf-top      *buf*) 0
                (buf-left     *buf*) 0
                (buf-filename *buf*) path
                (buf-dirty    *buf*) nil))
        (values t nil))
    (error (e) (values nil (format nil "~a" e)))))

(defun save-file (path)
  (handler-case
      (with-open-file (s path :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (dolist (ln (buf-lines *buf*)) (write-line ln s))
        (setf (buf-filename *buf*) path
              (buf-dirty    *buf*) nil)
        (values t nil))
    (error (e) (values nil (format nil "~a" e)))))

;;; ---------------------------------------------------------------
;;;  5.  Editing
;;; ---------------------------------------------------------------

(defun insert-char (ch)
  (let ((ln (cur-line)) (col (buf-col *buf*)))
    (set-cur-line (concatenate 'string
                               (subseq ln 0 col) (string ch) (subseq ln col)))
    (incf (buf-col *buf*))
    (setf (buf-dirty *buf*) t)))

(defun insert-newline ()
  (let* ((ln (cur-line)) (col (buf-col *buf*)) (row (buf-row *buf*)))
    (set-cur-line (subseq ln 0 col))
    (setf (buf-lines *buf*)
          (append (subseq (buf-lines *buf*) 0 (1+ row))
                  (list (subseq ln col))
                  (subseq (buf-lines *buf*) (1+ row))))
    (incf (buf-row *buf*))
    (setf (buf-col *buf*) 0  (buf-dirty *buf*) t)))

(defun delete-backward ()
  (let ((col (buf-col *buf*)) (row (buf-row *buf*)))
    (cond
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

;;; ---------------------------------------------------------------
;;;  7.  Rendering
;;; ---------------------------------------------------------------

(defun render (flash)
  (let* ((nrows (rows)) (ncols (cols))
         (text-rows (- nrows 2))
         (brow (buf-row *buf*)) (bcol (buf-col *buf*)))

    ;; vertical scroll
    (when (< brow (buf-top *buf*))
      (setf (buf-top *buf*) brow))
    (when (>= brow (+ (buf-top *buf*) text-rows))
      (setf (buf-top *buf*) (- brow text-rows -1)))

    ;; horizontal scroll — keep the cursor on screen for long lines
    (when (< bcol (buf-left *buf*))
      (setf (buf-left *buf*) bcol))
    (when (>= bcol (+ (buf-left *buf*) ncols))
      (setf (buf-left *buf*) (- bcol ncols -1)))

    ;; top bar
    (%move 0 0)
    (%attron +a-reverse+)
    (let* ((lhs (format nil " ~a~a"
                        (or (buf-filename *buf*) "[no name]")
                        (if (buf-dirty *buf*) " [+]" "")))
           (rhs "^S Save  ^O Open  ^N New  ^Q Quit")
           (pad (max 1 (- ncols (length lhs) (length rhs)))))
      (addstr-fit (concatenate 'string lhs
                               (make-string pad  :initial-element #\Space)
                               rhs
                               (make-string ncols :initial-element #\Space))
                  ncols))
    (%attroff +a-reverse+)

    ;; text lines (sliced by horizontal offset)
    (loop for sr from 0 below text-rows
          for lr = (+ sr (buf-top *buf*))
          do (%move (1+ sr) 0)
             (%clrtoeol)
             (when (< lr (line-count))
               (addstr-window (nth lr (buf-lines *buf*))
                              (buf-left *buf*) ncols)))

    ;; status bar
    (%move (1- nrows) 0)
    (%attron +a-reverse+)
    (addstr-fit
     (concatenate 'string
                  (or flash (format nil " Ln ~a  Col ~a  (~a lines)"
                                    (1+ brow) (1+ bcol) (line-count)))
                  (make-string ncols :initial-element #\Space))
     ncols)
    (%attroff +a-reverse+)

    ;; cursor
    (%move (1+ (- brow (buf-top *buf*)))
           (- bcol (buf-left *buf*)))
    (%refresh)))

;;; ---------------------------------------------------------------
;;;  8.  Mini-prompt on status bar
;;; ---------------------------------------------------------------

(defun mini-prompt (prompt-str)
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
;;;  9.  Key dispatch
;;; ---------------------------------------------------------------

(defun handle-key (k)
  (cond
    ((= k +ctrl-q+)
     (if (buf-dirty *buf*)
         (let ((a (mini-prompt "Unsaved changes — quit? (y/n): ")))
           (if (and a (string-equal a "y")) :quit nil))
         :quit))

    ((= k +ctrl-s+)
     (let ((p (or (buf-filename *buf*) (mini-prompt "Save as: "))))
       (when p
         (multiple-value-bind (ok err) (save-file p)
           (if ok (format nil " Saved: ~a" p)
                  (format nil " ERROR: ~a" err))))))

    ((= k +ctrl-o+)
     (let ((p (mini-prompt "Open file: ")))
       (when p
         (multiple-value-bind (ok err) (load-file p)
           (declare (ignore ok))
           (format nil " ~a~@[ (~a)~]" p err)))))

    ((= k +ctrl-n+)
     (let ((a (if (buf-dirty *buf*)
                  (mini-prompt "Discard changes? (y/n): ") "y")))
       (when (and a (string-equal a "y"))
         (setf *buf* (make-buf)) " [new buffer]")))

    ((= k +key-up+)    (move-up)    nil)
    ((= k +key-down+)  (move-down)  nil)
    ((= k +key-left+)  (move-left)  nil)
    ((= k +key-right+) (move-right) nil)
    ((or (= k +key-home+) (= k +ctrl-a+))  (move-home) nil)
    ((or (= k +key-end+)  (= k +ctrl-e+))  (move-end)  nil)
    ((= k +key-ppage+) (page-up   (- (rows) 3)) nil)
    ((= k +key-npage+) (page-down (- (rows) 3)) nil)

    ((or (= k 10) (= k 13))               (insert-newline)   nil)
    ((or (= k 127) (= k 8)
         (= k +key-backspace+))            (delete-backward) nil)
    ((or (= k +key-dc+) (= k +ctrl-d+))   (delete-forward)  nil)

    ((and (>= k 32) (< k 127))
     (insert-char (code-char k)) nil)

    (t nil)))

;;; ---------------------------------------------------------------
;;;  10. Main loop
;;; ---------------------------------------------------------------

(defun run (&optional filename)
  (when filename
    (multiple-value-bind (ok err) (load-file filename)
      (unless ok (format t "Note: ~a~%" err))))

  (setf *stdscr* (%initscr))
  (when (null-pointer-p *stdscr*)
    (error "initscr() returned NULL — check that $TERM is set (e.g. xterm-256color)"))

  (%noecho)
  (%cbreak)
  (%keypad *stdscr* 1)

  (let ((flash nil) (flash-ttl 0))
    (unwind-protect
        (loop
          (render (when (plusp flash-ttl) flash))
          (when (plusp flash-ttl) (decf flash-ttl))
          (let* ((k      (%getch))
                 (result (handle-key k)))
            (cond
              ((eq result :quit) (return))
              ((stringp result)
               (setf flash result flash-ttl 4)))))
      (%endwin))))

(defun main ()
  ;; Filename comes after a `--' separator, per the docstring at top.
  ;; SBCL leaves its own switches (`--load FILE') in *posix-argv*, so the
  ;; explicit separator is the only reliable way to find the user's argv.
  (let* ((argv #+sbcl sb-ext:*posix-argv* #-sbcl nil)
         (sep  (position "--" argv :test #'string=))
         (file (when sep (nth (1+ sep) argv))))
    (run file)))

(main)
