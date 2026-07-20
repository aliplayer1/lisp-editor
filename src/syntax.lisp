;;;; src/syntax.lisp - colors, tokenizer, syntax highlighting

(in-package #:ted)

;;; ---------------------------------------------------------------
;;;  7.  Syntax highlighting & paren matching
;;; ---------------------------------------------------------------

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

             ;; #\X character literal: emit a token so its paren/quote isn't matched
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

             ;; open paren: flag the next symbol as keyword candidate
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

             ;; symbol: emit as :keyword if it is the operator of a form
             ((symbol-char-p c)
              (let ((seg-start i))
                (loop while (and (< i n) (symbol-char-p (char line i)))
                      do (incf i))
                (when (and after-paren
                           (keyword-form-p (subseq line seg-start i)))
                  (push (list seg-start i :keyword) tokens)))
              (setf after-paren nil))

             ;; anything else: skip one char
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
          ;; no more tokens: emit the rest as default-colored
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

