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
  bins)

(defstruct sample
  ok   ; true if `has` is currently sorted.
  (has ; holds ?max items. if full, new items replace any old
     (make-array 5  :fill-pointer 0 :adjustable t)))

(defstruct sym 
  "sumamrizes a stream of symbols"
  (at 0) (txt "") (n 0) (w 1) has (most 0) mode)
;-------------------------------------------------------------------------------
;## Column Header Language
(defun isNum   (s) (and (> (length s) 0) (upper-case-p (char s 0))))
(defun isGoal  (s) (or (isKlass s) (isLess s) (isMore s)))
(defun isKlass (s) (got s #\! -1))
(defun isLess  (s) (got s #\- -1))
(defun isMore  (s) (got s #\+ -1))
(defun isIgnored (s) (got s #\X -1))
;-------------------------------------------------------------------------------
;## Create
(defun lst->cols (lst &aux (self (make-cols :names lst)))
  "column names to cols, then added to `all` and (maybe) `x`, `y`, and `klass`"
  (with-slots (all x y klass) self
    (loop :for at :from 0 :and txt :in lst do
      (let* ((maker (if (isNum txt) #'make-num #'make-sym))
             (col   (funcall maker :at at :txt txt :w (if (isLess txt) -1 1))))
        (push col all)
        (unless (isIgnored txt)
          (if (isKlass txt) (setf klass col))
          (if (isGoal txt) (push col y) (push col x)))))
    self))

(defun make-row (x) x)
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

(defmethod add ((self num) x)
  "updates `lo`, `hi`"
  (with-slots (has ok n lo hi) self
    (unless (eql x #\?)
      (incf n)
      (setf lo (min lo x)
            hi (max hi x)))))

;
; (defmethod bin ((self sym) x) x)
; (defmethod bin ((self num) x) 
;   (with-slots (lo hi) self
;     (if (= x hi) 
;         (1- (? bins)) 
;         (floor (- x lo) / (/ (- hi lo) (? bins))))))
;
;-------------------------------------------------------------------------------
;## Query
(defun holds (num)
  "returns `has`, sorted"
  ;(unless (num-ok num) (sort (num-has num) #'<))
  ;(setf (num-ok num) t)
  (num-has num))

(print     (data-cols (src->data (? file))))
