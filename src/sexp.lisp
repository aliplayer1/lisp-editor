;;;; src/sexp.lisp - paren walkers, sexp bounds, slurp/barf, indent

(in-package #:ted)

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

(defun sexp-bounds-at (row col)
    "Bounds of the first s-expression starting at or after (ROW, COL):
     (values sr sc er ec), EC inclusive.  Whitespace and comments are
     skipped.  Returns NIL at a close paren (end of the enclosing form)
     or when only whitespace/comments remain to end of buffer."
    (let ((nlines (line-count)))
      (loop while (< row nlines) do
        (let* ((line (nth row (buf-lines *buf*)))
               (n    (length line)))
          (multiple-value-bind (st-str st-blk) (state-at-line row)
            (multiple-value-bind (toks line-end-str)
                (tokenize-line line st-str st-blk)
              (loop while (< col n) do
                (let ((c   (char line col))
                      (tok (find-if (lambda (tk) (and (<= (first tk) col)
                                                      (< col (second tk))))
                                    toks)))
                  (cond
                    ;; comments skip like whitespace
                    ((and tok (eq (third tok) :comment))
                     (setf col (second tok)))
                    ((or (char= c #\Space) (char= c #\Tab))
                     (incf col))
                    ;; a string or char literal is one sexp: its token extent.
                    ;; A string still open at end of line (it is then the
                    ;; line's last token) continues on later lines; walk to
                    ;; the line where it closes.
                    ((and tok (member (third tok) '(:string :char)))
                     (if (and (eq (third tok) :string)
                              (= (second tok) n)
                              line-end-str)
                         (let ((r (1+ row)) (in-str t) (in-blk 0))
                           (loop while (< r nlines) do
                             (multiple-value-bind (tks2 s2 b2)
                                 (tokenize-line (nth r (buf-lines *buf*))
                                                in-str in-blk)
                               (unless s2
                                 (return-from sexp-bounds-at
                                   (values row (first tok)
                                           r (1- (second (first tks2))))))
                               (setf in-str s2 in-blk b2))
                             (incf r))
                           (return-from sexp-bounds-at nil))
                         (return-from sexp-bounds-at
                           (values row (first tok) row (1- (second tok))))))
                    ((char= c #\))
                     (return-from sexp-bounds-at nil))
                    ((char= c #\()
                     (multiple-value-bind (mr mc) (walk-paren-forward row col)
                       (return-from sexp-bounds-at
                         (when mr (values row col mr mc)))))
                    ;; reader-macro prefixes attach to the sexp that follows
                    ((find c "'`,@#")
                     (let ((start col))
                       (incf col)
                       (loop while (and (< col n) (find (char line col) "'`,@#"))
                             do (incf col))
                       (multiple-value-bind (sr sc er ec) (sexp-bounds-at row col)
                         (declare (ignore sc))
                         (return-from sexp-bounds-at
                           (when (and sr (= sr row))
                             (values row start er ec))))))
                    ((symbol-char-p c)
                     (let ((start col))
                       (loop while (and (< col n) (symbol-char-p (char line col)))
                             do (incf col))
                       (return-from sexp-bounds-at
                         (values row start row (1- col)))))
                    (t (return-from sexp-bounds-at (values row col row col)))))))))
        (incf row)
        (setf col 0))
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

(defun last-two-sexps-before (row col)
  "The last two s-expressions ENDING strictly before (ROW, COL) inside
   the innermost form enclosing that position.  Returns (values LAST
   PREV), each a list (sr sc er ec) with inclusive EC, or NIL.  Both
   NIL when there is no enclosing form or it is empty."
  (multiple-value-bind (orow ocol) (enclosing-open-paren row col)
    (if (null orow)
        (values nil nil)
        (let ((r orow) (c (1+ ocol)) (last nil) (prev nil))
          (loop
            (multiple-value-bind (sr sc er ec) (sexp-bounds-at r c)
              (unless sr (return))
              ;; stop once a sexp ends at or past the target position
              (unless (or (< er row) (and (= er row) (< ec col)))
                (return))
              (setf prev last
                    last (list sr sc er ec)
                    r er
                    c (1+ ec))))
          (values last prev)))))

(defun splice-paren (from-row from-col to-row to-col)
  "Delete the close paren at (FROM-ROW, FROM-COL) and re-insert it at
   (TO-ROW, TO-COL).  TO-COL is interpreted in the line's coordinates
   BEFORE the deletion; same-line moves adjust for the removed char.
   Marks the buffer dirty.  Caller records undo."
  (if (= from-row to-row)
      (let* ((ln  (nth from-row (buf-lines *buf*)))
             (del (concatenate 'string (subseq ln 0 from-col)
                               (subseq ln (1+ from-col))))
             (ins (if (> to-col from-col) (1- to-col) to-col)))
        (setf (nth from-row (buf-lines *buf*))
              (concatenate 'string (subseq del 0 ins) ")" (subseq del ins))))
      (progn
        (let ((ln (nth from-row (buf-lines *buf*))))
          (setf (nth from-row (buf-lines *buf*))
                (concatenate 'string (subseq ln 0 from-col)
                             (subseq ln (1+ from-col)))))
        (let ((ln (nth to-row (buf-lines *buf*))))
          (setf (nth to-row (buf-lines *buf*))
                (concatenate 'string (subseq ln 0 to-col) ")"
                             (subseq ln to-col))))))
  (setf (buf-dirty *buf*) t))

(defun slurp-forward ()
  "Pull the s-expression after the enclosing form inside its closer:
   (a)| b  ->  (a b).  Returns a flash string on refusal, NIL on success."
  (if (in-literal-p (buf-row *buf*) (buf-col *buf*))
      " In a string or comment"
      (multiple-value-bind (orow ocol)
          (enclosing-open-paren (buf-row *buf*) (buf-col *buf*))
        (if (null orow)
            " No enclosing form"
            (multiple-value-bind (crow ccol) (walk-paren-forward orow ocol)
              (if (null crow)
                  " Unbalanced form"
                  (multiple-value-bind (sr sc er ec)
                      (sexp-bounds-at crow (1+ ccol))
                    (declare (ignore sc))
                    (if (null sr)
                        " Nothing to slurp"
                        (progn
                          (record-undo)
                          (splice-paren crow ccol er (1+ ec))
                          (clamp-col)
                          nil)))))))))

(defun barf-forward ()
  "Eject the enclosing form's last s-expression past its closer:
   (a b|)  ->  (a) b.  Refuses rather than empty the form.  Returns a
   flash string on refusal, NIL on success."
  (if (in-literal-p (buf-row *buf*) (buf-col *buf*))
      " In a string or comment"
      (multiple-value-bind (orow ocol)
          (enclosing-open-paren (buf-row *buf*) (buf-col *buf*))
        (if (null orow)
            " No enclosing form"
            (multiple-value-bind (crow ccol) (walk-paren-forward orow ocol)
              (if (null crow)
                  " Unbalanced form"
                  (multiple-value-bind (last prev)
                      (last-two-sexps-before crow ccol)
                    (declare (ignore last))
                    (if (null prev)
                        " Nothing to barf"
                        (progn
                          (record-undo)
                          (splice-paren crow ccol (third prev) (1+ (fourth prev)))
                          (clamp-col)
                          nil)))))))))

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

(defun reindent-line ()
  "Re-indent the current line to match its Lisp nesting, the same way Enter
   auto-indents a fresh line: the leading whitespace is replaced with the
   computed indent.  A line whose first non-whitespace character is a close
   paren dedents to align under its enclosing opener.  No-op (and no undo
   step) when the line is already correctly indented or sits inside a string
   or block comment.  Point keeps its place in the line's text.  Returns NIL."
  (let* ((row     (buf-row *buf*))
         (line    (cur-line))
         (trimmed (string-left-trim '(#\Space #\Tab) line))
         (old     (- (length line) (length trimmed))))
    (multiple-value-bind (st-str st-blk) (state-at-line row)
      (unless (or st-str (plusp st-blk))
        (multiple-value-bind (orow ocol) (enclosing-open-paren row 0)
          (let ((target
                  (cond
                    ((null orow) 0)
                    ((and (plusp (length trimmed))
                          (char= (char trimmed 0) #\))) ocol)
                    (t (compute-newline-indent orow ocol)))))
            (unless (= target old)
              (record-undo)
              (set-cur-line
               (concatenate 'string
                            (make-string target :initial-element #\Space)
                            trimmed))
              (setf (buf-col *buf*)
                    (clamp (max target (+ (buf-col *buf*) (- target old)))
                           0 (length (cur-line)))
                    (buf-dirty *buf*) t)))))))
  nil)

(defun find-paren-match (row col)
  "Return (values match-row match-col) for the paren at (ROW, COL),
   or NIL if there is no match (no paren, in string/comment, unbalanced)."
  (case (paren-info row col)
    (:open  (walk-paren-forward row col))
    (:close (walk-paren-backward row col))
    (otherwise nil)))

(defun sexp-before-point ()
  "Text to evaluate: the active region wrapped in (progn ...), else the
   expression ENDING at point (a closed form via its matching opener,
   including any reader-prefix chars, or the atom point sits after).
   NIL when there is nothing before point."
  (if (region-active-p)
      (format nil "(progn ~a)" (region-text))
      (let* ((row (buf-row *buf*))
             (col (buf-col *buf*))
             (ln  (cur-line)))
        (cond
          ((and (plusp col)
                (eq (paren-info row (1- col)) :close))
           (multiple-value-bind (orow ocol) (walk-paren-backward row (1- col))
             (when orow
               (let ((s ocol)
                     (oline (nth orow (buf-lines *buf*))))
                 ;; Reach left over 'x, `x, ,x, ,@x, #'x so the prefix
                 ;; travels with the form it belongs to.
                 (loop while (and (plusp s)
                                  (find (char oline (1- s)) "'`,@#"))
                       do (decf s))
                 (text-between orow s row col)))))
          ((and (plusp col) (symbol-char-p (char ln (1- col))))
           (let ((s (1- col)))
             (loop while (and (plusp s) (symbol-char-p (char ln (1- s))))
                   do (decf s))
             (subseq ln s col)))
          (t nil)))))

