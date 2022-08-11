; TIYIN (c) 2022, Tim Menzies 
; Multi-objective semi-supervised XAI.
(defpackage :tiyin (:use :cl))
(in-package :tiyin)
(load "lib")

(defvar *opt* 
  (mapcar 'cli '(
    (file  "-f"  "help file"  "../../data/auto93.lisp")
    (help  "-h"  "show help"                 nil)
    (keep  "-K"  "items to keep"             256)
    (k     "-k"  "nb low attributes classes" 1)
    (m     "-n"  "nb low frequency classes"  2)
    (seed  "-s"  "random number seed"        10019)
    (go    "-g"  "start up action"           ls))))

(defmacro ! (key) `(cdr (assoc ',key *opt*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def sym  (txt "") (at 0) kept)
(def  num  (txt "") (at 0) kept ok (w 1))
(def  cols names all x y klass)
(def  data rows about)
(def  row  cells _about)

(defun make-sym (s n) (%make-sym :txt s :at n))
(defun make-num (s n) (%make-num :txt s :at n :w (if (equal #\- (charn s)) -1 1)))
(defun make-row (about l)   (%make-row :cells l :_about about))

(defun make-cols (lst)
  (let (all x y kl (pos -1))
    (dolist (s lst (%make-cols :names lst :all (reverse all) :x x :y y :klass kl))
      (let* ((what (if (eq #\$ (char0 s)) 'make-num 'make-sym))
             (col  (funcall what s (incf pos))))
        (push col all)
        (unless (eq #\~ (charn s))
          (if (member (charn s) '(#\! #\- #\+)) (push  col y) (push  col x))
          (if (eq #\! (charn s)) (setf kl col)))))))

(defun make-data (names &optional src (i (%make-data :about (make-cols names))))
  (if (stringp src)
    (reads src (lambda (row) (add i row)))
    (dolist (row src) (add i row)))
 i)

(print (make-row 12 '(1 2 3 4)))
(print (make-data '($aa bb!~ cc+)))
(print *opt*)
; ; (defmethod clone ((d data) &optional src) (make-data (? d about names) src))
; ;(reads "../../data/auto93.lisp" 'print)
