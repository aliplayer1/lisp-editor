;;;; src/main.lisp - run loop, argv parsing, entry point

(in-package #:ted)

;;; ---------------------------------------------------------------
;;;  11. Main loop
;;; ---------------------------------------------------------------

(defun run (&optional filename)
  (when filename
    (multiple-value-bind (ok err) (load-file filename)
      (unless ok (format t "Note: ~a~%" err))))

  (setf *stdscr* (%initscr))
  (when (null-pointer-p *stdscr*)
    (error "initscr() returned NULL -> Check that $TERM is set (e.g. xterm-256color)"))

  (%noecho)
  ;; raw() rather than cbreak(): cbreak() leaves XON/XOFF flow control on,
  ;; so the tty driver eats Ctrl-S (XOFF) and Ctrl-Q (XON) before getch can
  ;; see them - Save and Quit never fire.  raw() disables flow control so
  ;; those keys reach handle-key.  It also stops Ctrl-C/Ctrl-Z/Ctrl-\ from
  ;; raising signals, which for a full-screen editor is desirable: a stray
  ;; Ctrl-C can no longer kill the session and drop unsaved work.
  (%raw)
  (%keypad *stdscr* 1)
  (setup-shift-keys)
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
                      (and (consp k)
                           (let ((c (cdr k)))
                             (or (eql c (char-code #\w))
                                 (eql c (char-code #\d))
                                 (eql c 127) (eql c 8)
                                 (eql c +key-backspace+))))))
            (setf *last-cmd-was-delete*
                  (and (integerp k)
                       (or (= k 127) (= k 8) (= k +key-backspace+)
                           (= k +key-dc+) (= k +ctrl-d+))))
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

