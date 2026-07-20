;;;; src/buffer.lisp - buffer struct, language profiles, file I/O

(in-package #:ted)

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
  (mark-col nil)
  (mark-shift nil)
  (lang      nil))

(defvar *buf* (make-buf))

(defun cur-line     ()    (nth (buf-row *buf*) (buf-lines *buf*)))
(defun set-cur-line (s)   (setf (nth (buf-row *buf*) (buf-lines *buf*)) s))
(defun line-count   ()    (length (buf-lines *buf*)))
(defun clamp (v lo hi)    (max lo (min hi v)))
(defun clamp-col    ()
  (setf (buf-col *buf*) (clamp (buf-col *buf*) 0 (length (cur-line)))))

;;; ---------------------------------------------------------------
;;;  3a. Language profiles
;;; ---------------------------------------------------------------

(defstruct lang
  name           ; keyword shown in the status bar: :lisp :c :python
  pairs          ; alist of (opener . closer) chars that auto-pair
  newline-style  ; :lisp-form | :c-brace | :python-indent
  indent-width   ; step used by :c-brace / :python-indent
  extensions)    ; lowercase extension strings, no leading dot

(defparameter *lisp-lang*
  ;; ' is intentionally excluded: 'foo quoting is pervasive in Lisp.
  (make-lang :name :lisp
             :pairs '((#\( . #\)) (#\" . #\") (#\[ . #\]))
             :newline-style :lisp-form
             :indent-width 2
             :extensions '("lisp" "cl" "lsp" "el" "scm" "clj" "cljs" "ss" "rkt")))

(defparameter *c-lang*
  (make-lang :name :c
             :pairs '((#\( . #\)) (#\{ . #\}) (#\[ . #\])
                      (#\" . #\") (#\' . #\'))
             :newline-style :c-brace
             :indent-width 4
             :extensions '("c" "h" "cpp" "cc" "hpp" "hh" "cxx"
                           "js" "ts" "jsx" "tsx" "java" "cs" "go" "rs")))

(defparameter *python-lang*
  (make-lang :name :python
             :pairs '((#\( . #\)) (#\[ . #\]) (#\{ . #\})
                      (#\" . #\") (#\' . #\'))
             :newline-style :python-indent
             :indent-width 4
             :extensions '("py" "pyw" "pyi")))

(defparameter *languages* (list *lisp-lang* *c-lang* *python-lang*))
(defparameter *default-lang* *lisp-lang*)

(defun current-lang ()
  "Language profile of the current buffer, defaulting to *default-lang*."
  (or (buf-lang *buf*) *default-lang*))

(defun file-extension (path)
  "Lowercased extension of PATH without the dot, or NIL if there is none."
  (let* ((name (file-namestring path))
         (dot  (position #\. name :from-end t)))
    (when (and dot (< (1+ dot) (length name)))
      (string-downcase (subseq name (1+ dot))))))

(defun detect-language (path)
  "Language profile whose extensions include PATH's extension, else default."
  (let ((ext (file-extension path)))
    (or (and ext
             (find-if (lambda (l)
                        (member ext (lang-extensions l) :test #'string=))
                      *languages*))
        *default-lang*)))

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
                (buf-lang     *buf*) (detect-language path)
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

