; TIYIN (c) 2022, Tim Menzies 
; Multi-objective semi-supervised XAI.

(defpackage :tiyin (:use :cl))
(in-package :tiyin)
(defvar *opt* 
  `((file  "-f"  "help file"  "../../data/auto93.lisp")
    (help  "-h"  "show help"                 nil)
    (keep  "-K"  "items to keep"             256)
    (k     "-k"  "nb low attributes classes" 1)
    (m     "-n"  "nb low frequency classes"  2)
    (seed  "-s"  "random number seed"        10019)
    (go    "-g"  "start up action"           ls)))

(defmethod it (x) x)
(defmethod it ((x string))
  (let ((y (string-trim '(#\Space #\Tab #\Newline) x)))
    (if (string= y "?") y
      (let ((z (ignore-errors (read-from-string y))))
        (if (numberp z) z y)))))

(defun cli (flag help default)
  (let* ((args #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*)
         (it (member flag args :test 'equal)))
    (cond ((not it) default)
          ((equal default t)   nil)
          ((equal default nil) t)
          (t (it (second it))))))

(setf *opt* (mapcar (lambda (x) (cons (car x) (apply 'cli (cdr x))))  *opt*))

(defstruct (cols (:constructor %make-cols)) all x y names)

(defun make-cols (lst &aux (self (%make-cols :names lst)))
   (setf (cols-all self) (mapcar (lambda (x) (make-col x (incf at)))))



