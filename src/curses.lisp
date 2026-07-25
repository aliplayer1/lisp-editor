;;;; src/curses.lisp - ncurses library loading, CFFI bindings, key codes

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
(defcfun ("raw"      %raw)      :int)
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

;; terminfo lookups (live in libtinfo, resolved at runtime): used to discover
;; the shifted-movement keycodes without hardcoding them.
(defcfun ("tigetstr"    %tigetstr)    :pointer (capname :string))
(defcfun ("key_defined" %key-defined) :int     (definition :string))

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
(defconstant +ctrl-x+         24)
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
