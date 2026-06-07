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
(defcfun ("nodelay"  %nodelay)  :int  (win :pointer) (bf :int))
(defcfun ("timeout"  %timeout)  :void (delay :int))
(defcfun ("getmaxy"  %getmaxy)  :int (win :pointer))
(defcfun ("getmaxx"  %getmaxx)  :int (win :pointer))
(defcfun ("attron"   %attron)   :int (a :unsigned-long))
(defcfun ("attroff"  %attroff)  :int (a :unsigned-long))

;;; color (extension symbols are looked up lazily; calls are guarded)
(defcfun ("start_color"        %start-color)        :int)
(defcfun ("init_pair"          %init-pair)          :int (pair :short) (fg :short) (bg :short))
(defcfun ("has_colors"         %has-colors)         :int)
(defcfun ("use_default_colors" %use-default-colors) :int)

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

(defconstant +ctrl-space+      0)
(defconstant +ctrl-a+          1)
(defconstant +ctrl-d+          4)
(defconstant +ctrl-e+          5)
(defconstant +ctrl-g+         #o07)
(defconstant +ctrl-k+         #o13)
(defconstant +ctrl-n+         14)
(defconstant +ctrl-o+         15)
(defconstant +ctrl-q+         17)
(defconstant +ctrl-r+         #o22)
(defconstant +ctrl-s+         19)
(defconstant +ctrl-w+         #o27)
(defconstant +ctrl-y+         #o31)
(defconstant +ctrl-underscore+ #o37)

;;; ncurses base colors and the pair numbers we initialize in setup-colors
(defconstant +color-black+   0)
(defconstant +color-red+     1)
(defconstant +color-green+   2)
(defconstant +color-yellow+  3)
(defconstant +color-blue+    4)
(defconstant +color-magenta+ 5)
(defconstant +color-cyan+    6)
(defconstant +color-white+   7)

(defconstant +pair-comment+ 1)
(defconstant +pair-string+  2)
(defconstant +pair-keyword+ 3)
(defconstant +pair-number+  4)
(defconstant +pair-match+   5)

(defvar *stdscr* (null-pointer))
(defun rows () (%getmaxy *stdscr*))
(defun cols () (%getmaxx *stdscr*))

(defun addstr-fit (s max-cols)
  (%addstr (subseq s 0 (min (length s) max-cols))))

;;; ---------------------------------------------------------------
;;;  3.  Buffer
;;; ---------------------------------------------------------------

(defstruct buf
  (lines    (list "") :type list)
  (row      0         :type fixnum)
  (col      0         :type fixnum)
  (top      0         :type fixnum)
  (left     0         :type fixnum)
  (filename nil)
  (dirty    nil)
  (undo     nil       :type list)
  (redo     nil       :type list)
  (mark-row nil)
  (mark-col nil))

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

(defparameter *undo-limit* 200
  "Maximum number of snapshots retained on the undo stack.")

(defvar *last-cmd-was-insert* nil
  "T iff the previous handle-key call was a printable insert.  Used by
   insert-char to coalesce a run of single-char inserts into one undo
   step.  Set/cleared by the run loop based on the keycode.")

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

(defun insert-newline ()
  (record-undo)
  (clear-mark)
  (let* ((ln (cur-line)) (col (buf-col *buf*)) (row (buf-row *buf*)))
    (set-cur-line (subseq ln 0 col))
    (setf (buf-lines *buf*)
          (append (subseq (buf-lines *buf*) 0 (1+ row))
                  (list (subseq ln col))
                  (subseq (buf-lines *buf*) (1+ row))))
    (incf (buf-row *buf*))
    (setf (buf-col *buf*) 0  (buf-dirty *buf*) t)
    ;; Auto-indent: if the new line continues a multi-line string or
    ;; #|...|# block comment, leave it alone (auto-indent would corrupt
    ;; the literal).  Otherwise, if the cursor is inside a Lisp form,
    ;; prepend the right number of spaces.
    (multiple-value-bind (in-str in-blk) (state-at-line (buf-row *buf*))
      (unless (or in-str (plusp in-blk))
        (multiple-value-bind (orow ocol)
            (enclosing-open-paren (buf-row *buf*) 0)
          (when orow
            (let ((n (compute-newline-indent orow ocol)))
              (when (plusp n)
                (set-cur-line
                 (concatenate 'string
                              (make-string n :initial-element #\Space)
                              (cur-line)))
                (setf (buf-col *buf*) n)))))))))

(defun delete-backward ()
  (record-undo)
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
  (record-undo)
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
;;;  5a. Selection + kill ring
;;; ---------------------------------------------------------------

(defun set-mark ()
  "Anchor the selection at the current cursor."
  (setf (buf-mark-row *buf*) (buf-row *buf*)
        (buf-mark-col *buf*) (buf-col *buf*)))

(defun clear-mark ()
  "Discard any active selection."
  (setf (buf-mark-row *buf*) nil
        (buf-mark-col *buf*) nil))

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
;;;  7.  Syntax highlighting & paren matching
;;; ---------------------------------------------------------------

(defvar *suppress-main* nil
  "When non-nil, loading this file does NOT launch the UI.  Tests bind
   this to T before LOAD so they can drive the pure functions.")

(defvar *colors-enabled* nil)

(defun setup-colors ()
  "Initialize ncurses color pairs.  Silent no-op if the terminal lacks
   color support; missing use_default_colors is tolerated."
  (handler-case
      (when (= 1 (%has-colors))
        (%start-color)
        (handler-case (%use-default-colors) (error () nil))
        (%init-pair +pair-comment+ +color-cyan+    -1)
        (%init-pair +pair-string+  +color-yellow+  -1)
        (%init-pair +pair-keyword+ +color-magenta+ -1)
        (%init-pair +pair-number+  +color-green+   -1)
        (%init-pair +pair-match+   +color-white+   +color-red+)
        (setf *colors-enabled* t))
    (error () (setf *colors-enabled* nil))))

(defmacro with-color-pair (pair &body body)
  "Run BODY with the given color PAIR active.  PAIR may be NIL,
   in which case BODY runs with no extra attribute."
  (let ((p (gensym "P")))
    `(let ((,p ,pair))
       (when (and ,p *colors-enabled*) (%attron (ash ,p 8)))
       (unwind-protect (progn ,@body)
         (when (and ,p *colors-enabled*) (%attroff (ash ,p 8)))))))

(defun token-pair (kind)
  (case kind
    (:comment +pair-comment+)
    (:string  +pair-string+)
    (:keyword +pair-keyword+)
    (:number  +pair-number+)
    (otherwise nil)))

(defparameter *defun-likes*
  '("defun" "defmacro" "defvar" "defparameter" "defconstant"
    "defstruct" "defclass" "defmethod" "defgeneric" "defpackage"
    "let" "let*" "if" "when" "unless" "cond" "case" "ecase" "typecase"
    "progn" "prog1" "prog2" "dolist" "dotimes" "loop" "lambda"
    "handler-case" "handler-bind" "unwind-protect" "with-open-file"
    "multiple-value-bind" "multiple-value-call" "multiple-value-list"
    "destructuring-bind" "eval-when" "in-package" "labels" "flet"
    "block" "return-from" "return" "tagbody" "go" "throw" "catch"
    "setf" "setq" "psetq" "and" "or" "not" "function" "quote"))

(defun keyword-form-p (s)
  (member s *defun-likes* :test #'string-equal))

(defun symbol-char-p (c)
  (or (alphanumericp c)
      (find c "!?*+/<=>:-_$%&^~." :test #'char=)))

(defun tokenize-line (line in-string in-block)
  "Scan LINE with continuation state from preceding lines.
   Returns: tokens new-in-string new-in-block.
   Tokens are (start end kind), kind in {:comment :string :keyword :number}.
   IN-STRING is bool; IN-BLOCK is the #| ... |# nesting depth (0 if none)."
  (let ((tokens '())
        (i 0)
        (n (length line))
        (after-paren nil))
    (loop while (< i n) do
      (cond
        ;; inside an open #| ... |# block comment (CL block comments nest)
        ((> in-block 0)
         (let ((seg-start i))
           (loop while (and (< i n) (> in-block 0)) do
             (cond
               ((and (< (1+ i) n)
                     (char= (char line i) #\#)
                     (char= (char line (1+ i)) #\|))
                (incf in-block) (incf i 2))
               ((and (< (1+ i) n)
                     (char= (char line i) #\|)
                     (char= (char line (1+ i)) #\#))
                (decf in-block) (incf i 2))
               (t (incf i))))
           (push (list seg-start i :comment) tokens))
         (setf after-paren nil))

        ;; inside an unterminated "..." string
        (in-string
         (let ((seg-start i))
           (loop while (< i n) do
             (let ((c (char line i)))
               (cond
                 ((char= c #\\)
                  (incf i)
                  (when (< i n) (incf i)))
                 ((char= c #\")
                  (incf i)
                  (setf in-string nil)
                  (return))
                 (t (incf i)))))
           (push (list seg-start i :string) tokens))
         (setf after-paren nil))

        (t
         (let ((c (char line i)))
           (cond
             ;; line comment to end-of-line
             ((char= c #\;)
              (push (list i n :comment) tokens)
              (setf i n)
              (setf after-paren nil))

             ;; block comment start
             ((and (< (1+ i) n)
                   (char= c #\#)
                   (char= (char line (1+ i)) #\|))
              (let ((seg-start i))
                (incf i 2)
                (incf in-block)
                (loop while (and (< i n) (> in-block 0)) do
                  (cond
                    ((and (< (1+ i) n)
                          (char= (char line i) #\#)
                          (char= (char line (1+ i)) #\|))
                     (incf in-block) (incf i 2))
                    ((and (< (1+ i) n)
                          (char= (char line i) #\|)
                          (char= (char line (1+ i)) #\#))
                     (decf in-block) (incf i 2))
                    (t (incf i))))
                (push (list seg-start i :comment) tokens))
              (setf after-paren nil))

             ;; #\X character literal — emit a token so its paren/quote isn't matched
             ((and (< (+ i 2) n)
                   (char= c #\#)
                   (char= (char line (1+ i)) #\\))
              (let ((seg-start i))
                (incf i 3)
                (push (list seg-start i :char) tokens))
              (setf after-paren nil))

             ;; string
             ((char= c #\")
              (let ((seg-start i))
                (incf i)
                (setf in-string t)
                (loop while (< i n) do
                  (let ((c2 (char line i)))
                    (cond
                      ((char= c2 #\\)
                       (incf i)
                       (when (< i n) (incf i)))
                      ((char= c2 #\")
                       (incf i)
                       (setf in-string nil)
                       (return))
                      (t (incf i)))))
                (push (list seg-start i :string) tokens))
              (setf after-paren nil))

             ;; whitespace
             ((or (char= c #\Space) (char= c #\Tab))
              (incf i))

             ;; open paren — flag the next symbol as keyword candidate
             ((char= c #\()
              (incf i)
              (setf after-paren t))

             ;; close paren
             ((char= c #\))
              (incf i)
              (setf after-paren nil))

             ;; number
             ((or (digit-char-p c)
                  (and (or (char= c #\+) (char= c #\-))
                       (< (1+ i) n)
                       (digit-char-p (char line (1+ i)))))
              (let ((seg-start i))
                (incf i)
                (loop while (and (< i n)
                                 (let ((cc (char line i)))
                                   (or (digit-char-p cc)
                                       (char= cc #\.)
                                       (char= cc #\/)
                                       (char= cc #\e)
                                       (char= cc #\E))))
                      do (incf i))
                (push (list seg-start i :number) tokens))
              (setf after-paren nil))

             ;; symbol — emit as :keyword if it is the operator of a form
             ((symbol-char-p c)
              (let ((seg-start i))
                (loop while (and (< i n) (symbol-char-p (char line i)))
                      do (incf i))
                (when (and after-paren
                           (keyword-form-p (subseq line seg-start i)))
                  (push (list seg-start i :keyword) tokens)))
              (setf after-paren nil))

             ;; anything else — skip one char
             (t
              (incf i)
              (setf after-paren nil)))))))
    (values (nreverse tokens) in-string in-block)))

(defun state-at-line (line-index)
  "Return (in-string in-block) at the start of LINE-INDEX by folding
   tokenize-line over preceding lines."
  (let ((in-string nil) (in-block 0))
    (loop for i from 0 below line-index do
      (multiple-value-bind (toks new-str new-blk)
          (tokenize-line (nth i (buf-lines *buf*)) in-string in-block)
        (declare (ignore toks))
        (setf in-string new-str in-block new-blk)))
    (values in-string in-block)))

(defun render-line-with-tokens (line tokens left width)
  "Draw the slice [LEFT, LEFT+WIDTH) of LINE, applying TOKEN colors.
   Tokens must be sorted by start and non-overlapping."
  (let* ((len (length line))
         (end (min len (+ left width)))
         (toks tokens)
         (cursor left))
    ;; drop tokens entirely before the viewport
    (loop while (and toks (<= (second (first toks)) cursor))
          do (setf toks (rest toks)))
    (loop while (< cursor end) do
      (let ((tok (first toks)))
        (cond
          ;; no more tokens — emit the rest as default-colored
          ((null tok)
           (%addstr (subseq line cursor end))
           (setf cursor end))
          ;; we're inside (or at the start of) the next token
          ((<= (first tok) cursor)
           (let ((tok-end (min end (second tok))))
             (with-color-pair (token-pair (third tok))
               (%addstr (subseq line cursor tok-end)))
             (setf cursor tok-end)
             (when (>= cursor (second tok))
               (setf toks (rest toks)))))
          ;; gap of default-colored text before the next token
          (t
           (let ((gap-end (min end (first tok))))
             (%addstr (subseq line cursor gap-end))
             (setf cursor gap-end))))))))

;;; ----- Paren matching -------------------------------------------

(defun paren-skippable-p (col toks)
  "True if COL is inside a span that should not participate in paren
   matching: strings, comments, and character literals (#\\()."
  (some (lambda (tk)
          (and (<= (first tk) col)
               (< col (second tk))
               (or (eq (third tk) :string)
                   (eq (third tk) :comment)
                   (eq (third tk) :char))))
        toks))

(defun paren-info (row col)
  "Return :open, :close, or NIL for the char at (ROW, COL).
   A paren inside a string or comment returns NIL."
  (let ((line (nth row (buf-lines *buf*))))
    (when (and line (< col (length line)))
      (let ((c (char line col)))
        (when (or (char= c #\() (char= c #\)))
          (multiple-value-bind (st-str st-blk) (state-at-line row)
            (let ((toks (tokenize-line line st-str st-blk)))
              (unless (paren-skippable-p col toks)
                (if (char= c #\() :open :close)))))))))

(defun walk-paren-forward (start-row start-col)
  "From the open paren at (START-ROW, START-COL), find the matching close.
   Returns (values match-row match-col) or NIL if unbalanced."
  (let* ((lines (buf-lines *buf*))
         (nlines (length lines))
         (depth 1)
         (row start-row)
         (col (1+ start-col)))
    (multiple-value-bind (st-str st-blk) (state-at-line start-row)
      (loop while (< row nlines) do
        (let ((line (nth row lines)))
          (multiple-value-bind (toks new-str new-blk)
              (tokenize-line line st-str st-blk)
            (let ((n (length line)))
              (loop while (< col n) do
                (unless (paren-skippable-p col toks)
                  (let ((c (char line col)))
                    (cond
                      ((char= c #\() (incf depth))
                      ((char= c #\))
                       (decf depth)
                       (when (zerop depth)
                         (return-from walk-paren-forward
                           (values row col)))))))
                (incf col)))
            (setf st-str new-str st-blk new-blk)))
        (incf row)
        (setf col 0)))
    nil))

(defun walk-paren-backward (start-row start-col)
  "From the close paren at (START-ROW, START-COL), find the matching open.
   We sweep from the top of the buffer to just before (START-ROW, START-COL),
   collecting all real paren positions in reverse order, then walk them with
   a depth counter."
  (let ((reals '())
        (st-str nil)
        (st-blk 0))
    (loop for row from 0 to start-row do
      (let ((line (nth row (buf-lines *buf*))))
        (multiple-value-bind (toks new-str new-blk)
            (tokenize-line line st-str st-blk)
          (let ((bound (if (= row start-row) start-col (length line))))
            (dotimes (col bound)
              (let ((c (char line col)))
                (when (and (or (char= c #\() (char= c #\)))
                           (not (paren-skippable-p col toks)))
                  (push (list row col c) reals)))))
          (setf st-str new-str st-blk new-blk))))
    ;; REALS is most-recent-first (push order), so iterating it walks
    ;; backward from just before (START-ROW, START-COL).
    (let ((depth 1))
      (dolist (p reals)
        (let ((c (third p)))
          (cond
            ((char= c #\)) (incf depth))
            ((char= c #\()
             (decf depth)
             (when (zerop depth)
               (return-from walk-paren-backward
                 (values (first p) (second p)))))))))
    nil))

(defun enclosing-open-paren (row col)
  "Return (values OPEN-ROW OPEN-COL) of the innermost unclosed `(`
   containing position (ROW, COL), or NIL if none.  Parens inside
   strings, comments, and #\\X char literals are ignored, the same
   way the existing paren walkers handle them."
  (let ((stack '())
        (st-str nil)
        (st-blk 0))
    (loop for r from 0 to row do
      (let ((line (nth r (buf-lines *buf*))))
        (multiple-value-bind (toks new-str new-blk)
            (tokenize-line line st-str st-blk)
          (let ((bound (if (= r row) col (length line))))
            (dotimes (c bound)
              (let ((ch (char line c)))
                (when (and (or (char= ch #\() (char= ch #\)))
                           (not (paren-skippable-p c toks)))
                  (cond
                    ((char= ch #\() (push (list r c) stack))
                    ((char= ch #\)) (when stack (pop stack))))))))
          (setf st-str new-str st-blk new-blk))))
    (when stack
      (let ((top (first stack)))
        (values (first top) (second top))))))

(defun compute-newline-indent (open-row open-col)
  "Given the enclosing open-paren position, return the column the new
   line should be indented to.  Three rules, in order:
     - operator is in *defun-likes*  =>  open-col + 2
     - first arg is on the same line =>  column of that arg
     - operator at EOL or no operator =>  open-col + 1"
  (let* ((line (nth open-row (buf-lines *buf*)))
         (n    (length line))
         (i    (1+ open-col)))
    ;; tolerate stray whitespace right after `('
    (loop while (and (< i n)
                     (or (char= (char line i) #\Space)
                         (char= (char line i) #\Tab)))
          do (incf i))
    (cond
      ((>= i n) (1+ open-col))
      (t
       (let ((op-start i))
         (loop while (and (< i n) (symbol-char-p (char line i)))
               do (incf i))
         (let ((op-end i))
           (cond
             ((= op-end op-start) (1+ open-col))
             ((keyword-form-p (subseq line op-start op-end))
              (+ open-col 2))
             (t
              (loop while (and (< i n)
                               (or (char= (char line i) #\Space)
                                   (char= (char line i) #\Tab)))
                    do (incf i))
              (if (< i n) i (1+ open-col))))))))))

(defun find-paren-match (row col)
  "Return (values match-row match-col) for the paren at (ROW, COL),
   or NIL if there is no match (no paren, in string/comment, unbalanced)."
  (case (paren-info row col)
    (:open  (walk-paren-forward row col))
    (:close (walk-paren-backward row col))
    (otherwise nil)))

(defun overlay-paren-highlight (row col)
  "If (ROW, COL) is in the visible viewport, redraw its single char with
   +pair-match+ active.  Caller is responsible for restoring the cursor."
  (let* ((nrows (rows)) (ncols (cols))
         (text-rows (- nrows 2))
         (top (buf-top *buf*)) (left (buf-left *buf*))
         (sr (- row top)) (sc (- col left)))
    (when (and (>= sr 0) (< sr text-rows)
               (>= sc 0) (< sc ncols))
      (let ((line (nth row (buf-lines *buf*))))
        (when (and line (< col (length line)))
          (%move (1+ sr) sc)
          (with-color-pair +pair-match+
            (%addstr (string (char line col)))))))))

(defun overlay-selection ()
  "If a region is active, repaint each visible character inside it with
   +a-reverse+.  Mirrors overlay-paren-highlight's viewport math."
  (multiple-value-bind (sr sc er ec) (region-bounds)
    (when sr
      (let* ((nrows (rows)) (ncols (cols))
             (text-rows (- nrows 2))
             (top (buf-top *buf*)) (left (buf-left *buf*)))
        (loop for r from sr to er do
          (let* ((scr-row (- r top)))
            (when (and (>= scr-row 0) (< scr-row text-rows))
              (let* ((line  (nth r (buf-lines *buf*)))
                     (row-start (if (= r sr) sc 0))
                     (row-end   (if (= r er) ec (length line))))
                (loop for c from row-start below row-end do
                  (let ((scr-col (- c left)))
                    (when (and (>= scr-col 0) (< scr-col ncols)
                               (< c (length line)))
                      (%move (1+ scr-row) scr-col)
                      (%attron +a-reverse+)
                      (%addstr (string (char line c)))
                      (%attroff +a-reverse+))))))))))))

;;; ---------------------------------------------------------------
;;;  8.  Rendering
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

    ;; text lines, syntax-highlighted, sliced by horizontal offset
    (multiple-value-bind (st-str st-blk) (state-at-line (buf-top *buf*))
      (loop for sr from 0 below text-rows
            for lr = (+ sr (buf-top *buf*))
            do (%move (1+ sr) 0)
               (%clrtoeol)
               (when (< lr (line-count))
                 (let ((line (nth lr (buf-lines *buf*))))
                   (multiple-value-bind (toks new-str new-blk)
                       (tokenize-line line st-str st-blk)
                     (render-line-with-tokens line toks (buf-left *buf*) ncols)
                     (setf st-str new-str st-blk new-blk))))))

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

    ;; selection overlay — repaint chars in region-bounds with reverse
    ;; video.  Runs before paren-match so paren highlight wins visually
    ;; on overlap.
    (overlay-selection)

    ;; paren match overlay — repaint the cursor paren and its partner
    ;; with +pair-match+ so they stand out.  No-op when not on a paren.
    (multiple-value-bind (mr mc) (find-paren-match brow bcol)
      (when mr
        (overlay-paren-highlight brow bcol)
        (overlay-paren-highlight mr   mc)))

    ;; cursor
    (%move (1+ (- brow (buf-top *buf*)))
           (- bcol (buf-left *buf*)))
    (%refresh)))

;;; ---------------------------------------------------------------
;;;  9.  Mini-prompt on status bar
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

(defun handle-key (k)
  (cond
    ;; Meta-prefix: (cons 27 . k2)
    ((consp k)
     (cond
       ((eql (cdr k) (char-code #\w))
        (copy-region))
       (t nil)))

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

    ;; --- Stage 5 selection / kill / yank ---

    ((= k +ctrl-space+) (set-mark) " Mark set")
    ((= k +ctrl-w+)     (kill-region))
    ((= k +ctrl-y+)     (yank))
    ((= k +ctrl-k+)     (kill-line))
    ((= k +ctrl-g+)     (clear-mark) " Mark cleared")

    ((= k +key-up+)    (move-up)    nil)
    ((= k +key-down+)  (move-down)  nil)
    ((= k +key-left+)  (move-left)  nil)
    ((= k +key-right+) (move-right) nil)
    ((or (= k +key-home+) (= k +ctrl-a+))  (move-home) nil)
    ((or (= k +key-end+)  (= k +ctrl-e+))  (move-end)  nil)
    ((= k +key-ppage+) (page-up   (- (rows) 3)) nil)
    ((= k +key-npage+) (page-down (- (rows) 3)) nil)

    ((= k +ctrl-underscore+) (do-undo))
    ((= k +ctrl-r+)          (do-redo))

    ((or (= k 10) (= k 13))               (insert-newline)   nil)
    ((or (= k 127) (= k 8)
         (= k +key-backspace+))            (delete-backward) nil)
    ((or (= k +key-dc+) (= k +ctrl-d+))   (delete-forward)  nil)

    ((and (>= k 32) (< k 127))
     (self-insert (code-char k)) nil)

    (t nil)))

;;; ---------------------------------------------------------------
;;;  11. Main loop
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
  (setup-colors)

  (let ((flash nil) (flash-ttl 0))
    (unwind-protect
        (loop
          (render (when (plusp flash-ttl) flash))
          (when (plusp flash-ttl) (decf flash-ttl))
          (let* ((k      (read-key))
                 (result (handle-key k)))
            (setf *last-cmd-was-insert*
                  (and (integerp k) (>= k 32) (< k 127)))
            (setf *last-cmd-was-kill*
                  (or (and (integerp k) (or (= k +ctrl-k+) (= k +ctrl-w+)))
                      (and (consp k) (eql (cdr k) (char-code #\w)))))
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

(unless *suppress-main* (main))
