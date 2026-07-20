;;;; src/render.lisp - match/selection overlays and screen drawing

(in-package #:ted)

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

    ;; horizontal scroll: keep the cursor on screen for long lines
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

    ;; selection overlay: repaint chars in region-bounds with reverse
    ;; video.  Runs before paren-match so paren highlight wins visually
    ;; on overlap.
    (overlay-selection)

    ;; paren match overlay: repaint the cursor paren and its partner
    ;; with +pair-match+ so they stand out.  No-op when not on a paren.
    (multiple-value-bind (mr mc) (find-paren-match brow bcol)
      (when mr
        (overlay-paren-highlight brow bcol)
        (overlay-paren-highlight mr   mc)))

    ;; cursor
    (%move (1+ (- brow (buf-top *buf*)))
           (- bcol (buf-left *buf*)))
    (%refresh)))

