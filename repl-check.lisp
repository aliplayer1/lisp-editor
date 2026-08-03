;;;; Run using:
;;;; sbcl --script repl-check.lisp
;;;;
;;;; Needs SBCL, Quicklisp (for CFFI), libncursesw, and sbcl on PATH.

;; sbcl --script does not read ~/.sbclrc, so we put Quicklisp on the table.
(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file setup) (load setup)))
(funcall (read-from-string "ql:quickload") "cffi" :silent t)

(defpackage #:ted (:use #:cl #:cffi))
(in-package #:ted)
(defvar *suppress-main* t)
(load (merge-pathnames "editor.lisp"
                       (or *load-pathname* *default-pathname-defaults*)))

(defun wait-for (pred)
  (dotimes (i 200)
    (repl-drain)
    (when (funcall pred) (return t))
    (sleep 0.05)))

(defun evaluate (text)
  (format t "~&~a> ~a~%" (repl-package *repl*) text)
  (setf (repl-pending *repl*) t)
  (repl-send text)
  (wait-for (lambda () (not (repl-pending *repl*))))
  (format t "~a~%" (first (repl-transcript *repl*))))

(unless (repl-start)
  (format t "~&Could not start a child sbcl.  Is it on PATH?~%")
  (sb-ext:exit :code 1))

(evaluate "(defun square (n) (* n n))")
(evaluate "(square 7)")

(repl-kill)
(format t "~&Both forms went to the same child, so the definition survived.~%")
