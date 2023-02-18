; vi: set ts=2 sw=2 sts=2 et :
(load "tabu-lib")
(defvar *help* "

tabu.lisp

OPTIONS:
  -f   file   csv file           = ../data/auto93.csv
  -m   max    max num cace       = 512
  -p   p      dist coeffecient   = 2
  -s   seed   random numbe seed  = 10013")

(defvar *settings* (settings *help* (args)))
(print *settings*)
;----------------------------------------------------------

(defun isNum   (s) (and (> (length s) 0) (upper-case-p (char s 0))))
(defun isGoal  (s) (or (isKlass s) (isLess s) (isMore s)))
(defun isKlass (s) (got s #\! -1))
(defun isLess  (s) (got s #\- -1))
(defun isMore  (s) (got s #\+ -1))
(defun isIgnored (s) (got s #\? -1))

(defstruct data rows cols)

(defstruct cols all x y names)

(defstruct sym 
  (at 0) (txt "") (n 0) (w 1) has most mode)

(defstruct num 
  (at 0) (txt "") (n 0) (w 1) ok 
  (has (make-array 5 :fill-pointer 0 :adjustable t)))

(defmethod add ((self sym) x)
  (with-slots (has n mode most) self
   (unless (eql x #\?)
     (incf n)
     (incf (freq x has))
     (if (> (freq x has) most) (setf most (freq x has) mode x)))))

(defmethod add ((self num) x)
  (with-slots (has ok n) self
    (unless (eql x #\?)
      (incf n)
      (cond ((< (length has) (? max)) 
             (setf ok nil) 
             (vector-push-extend x has))
            ((<= (rand) (/ (? max) n)) 
             (setf ok nil) 
             (setf (aref has (rint (length has))) x))))))

(defun holds (num)
  (unless (num-ok num) (sort (num-has num) #'<))
  (setf (num-ok num) t)
  (num-has num))

(defun make-col (&key (txt "") (at 0))
  (funcall  (if (isNum txt) #'make-num #'make-sym) 
            :at at :txt txt
            :w   (if (isLess txt) -1 1)))

(defun defcols (lst &aux (self (make-cols :names lst)) (n -1))
  (with-slots (all x y klass) self
    (labels ((defcol (txt &aux (col (make-col :txt txt :at (incf n)))) 
                     (push col all)
                     (unless (isIgnored txt)
                       (if (isKlass txt)  (setf klass col))
                       (if (isGoal txt) (push col y) (push col x)))))
      (mapcar #'defcol lst)
      self)))

(defun defdata (src &optional more &aux (self (make-data)))
  (labels ((row (lst) (add self lst)))
    (cond 
      ((stringp  src) (with-file src #'row)  self)
      ((consp src)    (mapcar #'row src)     self)
      ((data-p src)   (defdata (make-data) (cons (list (cols-names (data-cols src))) more))))))

(defmethod add ((self data) lst)
  (aif (data-cols self)
    (push (add it lst)  (data-rows self))
    (setf (data-cols self) (defcols lst))))

(defmethod add ((self cols) lst)
  (dolist (tmp (list (cols-x self) (cols-y self)) lst)
    (dolist (col tmp)
      (add col (elt lst (slot-value col 'at))))))

(print (defdata (? file)))
