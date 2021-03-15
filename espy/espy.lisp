; vim: noai:ts=2:sw=2:et: 
(defpackage :espy
  (:use :common-lisp)
  (:export #:espy))

(in-package :espy)

(defstruct about
  (who  "Tim Menzies")
  (why  "es.py /əˈspī/ verb LITERARY to glimpse what is hidden, or obscure")
  (what "data mining used for optimizing")
  (version 0.21)
  (copyleft "(c) 2021 Tim Menzies, MIT License"))

(defstruct options 
  (about (make-about))
  (dir  "data")
  (data "auto93.csv")
  (seed 10013)
  (keep 1024)
  (no   "?")
  (sep  ","))

;-------- --------- --------- --------- --------- --------- --------- ----------
; macros and constants
(defconstant +lo+ most-negative-fixnum)
(defconstant +hi+ most-positive-fixnum)

(defmacro memo (x &rest body)
  `(or ,x (setf ,x (progn ,@body))))

(defmacro assoc! (x lst &key new (test #'equal))
  `(or (assoc ,x ,lst :test ,test)
       (car (setf ,lst (cons (cons ,x ,new) ,lst)))))

(defmacro ? (obj f &rest fs)
  (if fs `(? (slot-value ,obj ',f) ,@fs) `(slot-value ,obj ',f)))

;-------- --------- --------- --------- --------- --------- --------- ----------
(defstruct span (lo +lo+) (hi +hi+) also)

(defmethod has ((s span) x)
  (with-slots (lo hi) s
    (if (equal lo hi) (equal x lo) (<= lo x hi))))

;-------- --------- --------- --------- --------- --------- --------- ----------
(defstruct col (n 0) (pos 0) (txt "") w bins all)

(defmethod add ((c col) (lst cons)) 
  (dolist (x lst c) (add c x)))

(defmethod add ((c col) x) 
  (unless (equal x "?") (incf (? c n)) (add1 c x))
  x)

(defmethod bins ((c col) tab)
  (memo (? c bins) (bins1 c tab)))

(defmethod w ((c col) &aux (s (? c txt)))
  (memo (? c w) (if (eql #\- (char s (1- (length s)))) -1 1)))

(defmethod bins1 ((c col) x) x)

;-------- --------- --------- --------- --------- --------- --------- ----------
(defstruct (num (:include col)) ok tmp)

(defmethod add1 ((n num) x)
  (push x (? n tmp))
  (setf (? n ok) nil))

(defmethod all ((n num))
  (with-slots (ok all tmp) n
    (unless ok 
      (setf all (coerce (sort tmp #'<) 'vector)))
    (setf ok t)
    all))

(defmethod norm ((n num) x) 
  (cond ((equal x "?") x)
        (t (let ((lst (all n)))
             (/ (- x (nth 0 lst)) (- (cdr (last lst)) (nth 0 lst)))))))

(defmethod per ((n num) p) (aref (all n) (1- (floor (* p (length (all n)))))))
(defmethod mid ((n num))   (per n .5))
(defmethod sd  ((n num))   (/ (- (per n .9) (per n .1)) 2.56))
;-------- --------- --------- --------- --------- --------- --------- ----------
;;;; lib
(let* ((seed 10013))
  (labels ((park-miller-randomizer ()
             (setf seed (mod (* 16807.0d0 seed) 2147483647.0d0))
             (/ seed 2147483647.0d0))) 
    (defun srand (o)  (setf seed (? o seed)))
    (defun randf (&optional (n 1)) (* n (- 1.0d0 (park-miller-randomizer))))
    (defun randi (n) (floor (* n (/ (randf 1000.0) 1000))))))

(defun it (x &aux (y (read-from-string x))) 
  (if (typep y 'number) y x))

(defun has (needle haystack &key (test 'char=))
  (not (null (search (string needle) (string haystack) :test test))))
  
;-------- --------- --------- --------- --------- --------- --------- ----------
(defun espy (&optional (o (make-options))) 
  (print o))
