; vim: ts=2 sw=2 et:

(defstruct col (n 0) txt (w -1) (pos 0))
(defstruct (num (:include col))
  (_all (make-array 32 :fill-pointer 0 :adjustable t))
  sorted)
(defstruct (sym (:include col))
  seen mode (most 0))

(def 
(defmethod dist (c x y)
  (if (and (eq x #\?) (eq y #\?)) 1 (dist1 x y)))

(defmethod dist1 ((c sym) x y) (if (eql x y) 1 0))

(defmethod add ((x col) (y cons)) (dolist (z y) (add x z)))  
(defmethod add ((x col) y)
  (unless (eq #\? y)
    (incf (o x n))
    (add1 x y))
  y)

(defmethod add1 ((x num) (y string))
  (add1 x (read-from-string y)))
(defmethod add1 ((x num) (y number))
  (push y (o x all))
  (setf (o x sorted) nil)
  y)

(defmethod all ((x num))
  (unless (o sorted)
    (setf (o _all)   (sort (o _all) #'<)
          (o sorted) t))
   (o _all))

(defmethod mid ((x num)) (per x))
(defmethod sd  ((x num)) (/ (- (per x .9) (per x .1)) 2.56))
(defmethod per ((x num) &optional (p .5))
  (let* ((v (all x))
         (n (length v)))
    (svref v (floor (* p n)))))

(defmethod dist ((x num) a b)
