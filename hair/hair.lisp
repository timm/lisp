; vim: set ts=2 sw=2 sts=2 et lisp cc=75 lispwords+=loop,defm,aif :
;;; hair: where the system tricks live. Start the package, tell the
;;; debugger to chill, set the config, load the plain-lisp rest, run.
;;; Usage: sbcl --script hair.lisp [--key=val ...] [NAME ...]
;;; (c) 2026, Tim Menzies <timm@ieee.org>, MIT license
(defpackage :hair (:use :cl))
(in-package :hair)

#+sbcl (declaim (sb-ext:muffle-conditions style-warning))
#+sbcl (setf sb-ext:*invoke-debugger-hook*  ; one line per crash, then exit
         (lambda (c h) (declare (ignore h))
           (format *error-output* "~&!! ~a~%" c)
           (sb-ext:exit :code 1 :abort t)))

(defun args () "Command-line words after the script name."
  #+sbcl  (cdr sb-ext:*posix-argv*)
  #+clisp ext:*args*
  #+ecl   (cdddr ext:*command-args*))

(defun halt (&optional (n 0)) "Exit with status n."
  #+sbcl (sb-ext:exit :code n :abort t) #+(or clisp ecl) (ext:quit n))

(load "min")   ; general lisp tricks

;;;; about -------------------------------------------------------------
(defstruct about
  (file "~/gits/moot/optimize/misc/auto93.csv")
  (seed 1234567891) (pow 2) (leaf 3) (maxd 8) (eps 0.35))

(defvar *k* (make-about))

(load "ai")    ; the AI
(mapc #'cli (args))
