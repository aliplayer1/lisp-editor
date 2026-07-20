;;;; src/package.lisp - package definition, forward declarations, flags.

(defpackage #:ted
  (:use #:cl #:cffi))

(in-package #:ted)

;; Forward declarations: these are referenced before their defuns appear
;; (the source is grouped by topic, not definition order), so declare them
;; up front to keep SBCL from emitting undefined-function STYLE-WARNINGs
;; at load time.
(declaim (ftype function
                clear-mark pair-closer symbol-char-p mini-prompt
                tokenize-line state-at-line paren-skippable-p
                enclosing-open-paren compute-newline-indent
                sexp-bounds-at region-bounds))

(defvar *suppress-main* nil
  "When non-nil, loading this file does NOT launch the UI.  Tests bind
   this to T before LOAD so they can drive the pure functions.")
