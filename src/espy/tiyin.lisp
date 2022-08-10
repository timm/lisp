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
  
(defun chars (x) (if (symbolp x) (symbol-name x) x))
(defun char0 (x) (char (chars x) 0))
(defun charn (x) (let ((y (chars x))) (char y (1- (length y)))))

(defun reads (file fun)
  (print 1)
  (with-open-file (s file)
    (loop (funcall fun (or (read s nil) (return-from reads))))))

(defmacro ? (s x &rest xs)
 (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))

(defun slots (obj slots)
  (mapcar (lambda (y) (format nil ":~(~a~) ~a" y (slot-value obj y))) slots))
                            
(defstruct pretty)
(defmacro defthing (x &body body) 
  (let ((show (remove-if (lambda (x) (eq #\_ (char (symbol-name x) 0))) 
                         (mapcar (lambda (x) (if (consp x) (car x) x)) body))))
    `(progn
       (defstruct (,x (:include pretty) 
                      (:constructor ,(intern (format nil "%MAKE-~a" x)))) ,@body)
       (defmethod print-object ((self ,x) str)
         (format str "~a" (cons ',x (slots self ',show)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defthing sym  (txt "") (at 0) kept)
(defthing num  (txt "") (at 0) kept ok (w 1))
(defthing cols names all x y klass)
(defthing data rows about)
(defthing row  cells _about)

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
;
; (defmethod clone ((d data) &optional src) (make-data (? d about names) src))
;(reads "../../data/auto93.lisp" 'print)
