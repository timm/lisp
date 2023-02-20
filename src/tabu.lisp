; vi: set ts=2 sw=2 sts=2 et :
(defpackage :tabu (:use :cl))
(in-package :tabu)
(load "tabu-lib")
(defvar *help* "

tabu.lisp

OPTIONS:
  -b   bins   max bin numbers    = 16
  -f   file   csv file           = ../data/auto93.csv
  -m   max    max num cace       = 512
  -p   p      dist coeffecient   = 2
  -s   seed   random numbe seed  = 10013")

(defvar *settings* (settings *help* (args)))
;-------------------------------------------------------------------------------
;## Structs
(defstruct data 
  "stores `rows`, sumamrized sin `cols`"
  rows cols)

(defstruct cols 
  "stores everything in `all`, independent/dependent things in `x`,`y`"
  all x y names)

(defstruct num 
  "sumamrizes a stream of numbers"
  (at 0) (txt "") (n 0) (w 1) ; w=1,-1 means "maximize", "minimize"
  (hi most-negative-fixnum) 
  (lo most-positive-fixnum)
  (mu 0) (m2 0))

(defstruct sym 
  "sumamrizes a stream of symbols"
  (at 0) (txt "") (n 0) (w 1) has (most 0) mode)
;-------------------------------------------------------------------------------
;## Create
(defun lst->cols (lst &aux (self (make-cols :names lst)))
  "column names to cols, then added to `all` and (maybe) `x`, `y`, and `klass`"
  (with-slots (all x y klass) self
    (labels ((isKlass   (s) (got s #\! -1))
             (isLess    (s) (got s #\- -1))
             (isMore    (s) (got s #\+ -1))
             (isIgnored (s) (got s #\X -1))
             (isGoal    (s) (or (isKlass s) (isLess s) (isMore s)))
             (isNum     (s) (and (> (length s) 0) (upper-case-p (char s 0)))))
      (loop :for at :from 0 :and txt :in lst do
        (let* ((maker (if (isNum txt) #'make-num #'make-sym))
               (col   (funcall maker :at at :txt txt :w (if (isLess txt) -1 1))))
          (push col all)
          (unless (isIgnored txt)
            (if (isKlass txt) (setf klass col))
            (if (isGoal txt) (push col y) (push col x)))))))
  self)

(defun src->data (src &optional rows &aux (self (make-data)))
  "from file if (stringp src); from list if (consp src); mimic structure if (data-p src)"
  (labels ((row (x) (add self x)))
    (cond ((stringp  src) (with-file src #'row))
          ((consp src)    (mapc #'row src))
          ((data-p src)   (row  (cols-names (data-cols src)))))
    (mapc #'row rows)
    self))
;-------------------------------------------------------------------------------
;## Add
(defmethod add ((self data) lst)
  "updates `rows` and `cols`"
  (aif (data-cols self)
    (push (add it lst)  (data-rows self))
    (setf (data-cols self) (lst->cols lst))))

(defmethod add ((self cols) lst)
  "update nums and syms"
  (dolist (tmp `(,(cols-x self) ,(cols-y self)) lst)
    (dolist (col tmp)
      (add col (elt lst (slot-value col 'at))))))

(defmethod add ((self sym) x)
  "update frequency counts (in `has`) and `most` and `mode`"
  (with-slots (has n mode most) self
   (unless (eql x #\?)
     (incf n)
     (incf (freq x has))
     (if (> (freq x has) most) (setf most (freq x has) mode x)))))

(defmethod add ((self num) x) ;;; Add one thing, updating 'lo,hi'
  "updates `lo`, `hi`, `mu`, `sd`"
  (with-slots (n lo hi mu m2) self
    (unless (eq x #\?)
      (incf n)
      (let ((d (- x mu)))
        (incf mu (/ d n))
        (incf m2 (* d (- x mu)))
        (setf lo (min x lo)
              hi (max x hi))))))
;-------------------------------------------------------------------------------
;## Queries
(defmethod mid ((self sym)) (sym-mode self))
(defmethod mid ((self num)) (num-mu   self))

(defmethod div ((self sym))
  "Diversity (entropy)."
  (with-slots (has n) self 
    (labels ((fun (p) (* -1 (* p (log p 2)))))
      (loop for (_ . n1) in has sum (fun (/ n1 n))))))

(defmethod div ((self num))
  "return standard deviation"
  (with-slots (n m2) self (if (<= n 1) 0 (sqrt (/ m2 (- n 1))))))

(defmethod like ((self num) x)
  (with-slots (mu n) self
    (let ((sd (div self))
          (ε  1E-32))
      (cond ((< x (- mu (* 4 sd))) 0)
            ((> x (+ mu (* 4 sd))) 0)
            (t (let ((denom (sqrt (* 2 pi sd sd)))
                     (nom   (exp (/ (* -1 (expt (- x mu) 2)) 
                                    (+ ε (* 2 sd sd))))))
                 (/ nom (+ denom ε ))))))))

(defun tests ()
  (labels
    ((rand! (&aux (n (make-num)))
            (dotimes (i 1000) (add n (rand)))
            (assert (<= .48 (mid n) .52) () "_rand"))
     (num!  (&aux (n (make-num)))
            (dotimes (i 1000) (add n i))
            (assert  (<= 498 (mid n) 502)))
     (sym!  (&aux (s (make-sym)))
            (dolist (x '(a a a a b b c)) (add s x))
            (assert (<= 1.37 (div s) 1.38) () "sym"))
     (data! (&aux (d (src->data (? file))))
       (print (length (data-rows d)))
       (print (cols-x (data-cols d)))
       )
     )
    (rand!) (num!) (sym!) (data!)))

(tests)
