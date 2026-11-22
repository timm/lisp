; vim: set ts=2 sw=2 sts=2 et lisp cc=75 lispwords+=loop :
;;; hair: the command-line script; all the system tricks live here.
;;; Tell the debugger to chill, load the system, run the args.
;;; Usage: sbcl --script hair.lisp [--key=val ...] [NAME ...]
;;;   e.g. sbcl --script hair.lisp --leaf=20 tree
;;; Script only: don't (load) this from a REPL; it exits on any error.
;;; As a library: (ql:quickload "hair") or (asdf:load-system "hair");
;;; add the egs and cli with "hair/eg". None of that runs anything.
;;; (c) 2026, Tim Menzies <timm@ieee.org>, MIT license
#+sbcl (declaim (sb-ext:muffle-conditions style-warning))
#+sbcl (setf sb-ext:*invoke-debugger-hook*  ; one line per crash, then exit
         (lambda (c h) (declare (ignore h))
           (format *error-output* "~&!! ~a~%" c)
           (sb-ext:exit :code 1 :abort t)))

(defun args () "Command-line words after the script name."
  #+sbcl  (cdr sb-ext:*posix-argv*)
  #+clisp ext:*args*
  #+ecl   (cdddr ext:*command-args*))

(dolist (f '("lithp" "ai" "eg"))      ; same order as hair.asd
  (load (merge-pathnames f *load-truename*)))  ; works from any dir
(let ((*package* (find-package :hair)))  ; print symbols as before
  (mapc #'hair:cli (args)))
