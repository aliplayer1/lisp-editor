;;;; editor.lisp - Small terminal text editor.
;;;;              Uses ncurses directly through CFFI.  No cl-charms.
;;;;
;;;; This file is a thin loader: the real source lives in src/, loaded
;;;; below in dependency order.  Run from a real terminal (never inside
;;;; Emacs/SLIME, ncurses takes over the tty):
;;;;
;;;;   sbcl --load editor.lisp                  # empty buffer
;;;;   sbcl --load editor.lisp -- myfile.txt    # open a file
;;;;
;;;; Keybindings and features: see README.md.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cffi" :silent t))

(let ((here (or *load-truename* *default-pathname-defaults*)))
  (dolist (file '("package" "curses" "buffer" "editing" "region"
                  "movement" "syntax" "sexp" "repl" "render" "keys" "main"))
    (load (merge-pathnames (format nil "src/~a.lisp" file) here))))

(in-package #:ted)

(unless *suppress-main*
  (main)
  ;; Leave the process once the editor quits: without this, `sbcl --load
  ;; editor.lisp` drops into the bare SBCL REPL prompt after (main) returns.
  #+sbcl (sb-ext:exit))
