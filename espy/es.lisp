; vim: noai:ts=2:sw=2:et: 
(defpackage :espy (:use :common-lisp) (:export #:espy))
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

;;;; structs ------------------------------------------------------------------
(defstruct col (n 0) (pos 0) (txt "") w bins all)
(defstruct (sym (:include col)) tmp (most 0) mode)
(defstruct (num (:include col)) ok tmp)

((defstruct tab rows cols names xs ys)
defstruct bin (also (make-sym)) (lo most-positive-fixnum) (hi most-negative-fixnum))

;;;; macros -------------------------------------------------------------------
(defmacro has (x &rest body) `(or ,x (setf ,x (progn ,@body))))

(defmacro hassoc (x lst &key new (test #'equal))
  `(or (assoc ,x ,lst :test ,test)
       (car (setf ,lst (cons (cons ,x ,new) ,lst)))))

(defmacro ? (obj f &rest fs)
  (if fs `(? (slot-value ,obj ',f) ,@fs) `(slot-value ,obj ',f)))

;;;; methods -----------------------------------------------------------------
;;; bin
(defmethod holds ((b bin) x)
  (with-slots (lo hi) b
    (if (equal lo hi) (equal x lo) (<= lo x hi))))

;;; col
(defmethod add ((c col) (lst cons)) (dolist (x lst c) (add c x)))

(defmethod add ((c col) x) 
  (unless (equal x "?") (incf (? c n)) (add1 c x))
  x)

(defmethod bins ((c col) tab) (has (? c bins) (bins1 c tab)))

(defmethod  w ((c col)  &aux (s (? c txt)))
  (has (? c w) (if (eql #\- (char s (1- (length s)))) -1 1)))

(defmethod bins1 ((c col) x) x)

;;; num
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

(defmethod mid ((n num))   (per n .5))
(defmethod per ((n num) p) (aref (all n) (1- (floor (* p (length (all n)))))))
(defmethod sd  ((n num))   (/ (- (per n .9) (per n .1)) 2.56))

;;; tables
(defmethod better ((tb tab) r1 r2 &aux (s1 0) (s2 0) (n (length (? tb rows))))
  (dolist (col (? tb ys) 
               (< (/ s1 n)  (/ s2 n)))
    (let ((a (norm col (nth r1 (? col pos))))
          (b (norm col (nth r2 (? col pos)))))
      (decf s1 (exp (* (? col w) (/ (- a b) n))))
      (decf s2 (exp (* (? col w) (/ (- b a) n)))))))

(defmethod add ((tb tab) lst)
  (let ((n 0))
    (labels 
      ((goalp (x) (member (char x (1- (length x))) (list #\! #\+ #\-)))
       (prep  (x) (let ((tmp (if (upper-case-p (char x 0))
                               (make-num :pos (incf n) :txt x)
                               (make-sym :pos (incf n) :txt x))))
                    (push (cons x n) (? tb names))
                    (if (goalp x) (push tmp (? tb y)) (push tmp (? tb x)))
                    tmp)))
      (dolist (one lst)
        (if (? tb cols)
          (push (mapcar #'add (? tb cols)) (? tb rows))
          (setf (? tb cols) (mapcar #'prep one))))
      (setf (? tb  rows) (sort (? tb rows) #'better))
      tb)))
     
;;;; lib -----------------------------------------------------------------------
;;; random numbers
(let* ((seed 10013))
  (labels ((park-miller-randomizer ()
             (setf seed (mod (* 16807.0d0 seed) 2147483647.0d0))
             (/ seed 2147483647.0d0))) 
    (defun srand (o)  (setf seed (? o seed)))
    (defun randf (&optional (n 1)) (* n (- 1.0d0 (park-miller-randomizer))))
    (defun randi (n) (floor (* n (/ (randf 1000.0) 1000))))))

;;; strings
(defun it (x &aux (y (read-from-string x))) 
  (if (typep y 'number) y x))

(defun in (needle haystack &key (test 'char=))
  (not (null (search (string needle) (string haystack) :test test))))

;-------- --------- --------- --------- --------- --------- --------- ----------
;;;; main
(defun espy (&optional (my (make-options))) my)
