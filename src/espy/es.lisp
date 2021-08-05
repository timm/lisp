; vim: ts=2 sw=2 et:

; -----------------------------------------------
; ## Columns
; ### Col
(defstruct col (n 0) txt (w -1) (pos 0))
(defmethod add ((x col) (y cons)) (dolist (z y) (add x z)))

(defmethod add ((x col) y)
  (unless (eq #\? y)
    (incf (o x n))
    (add1 x y))
  y)

(defmethod dist ((c col) x y)
  (if (and (eq x #\?) (eq y #\?)) 1 (dist1 x y)))

; ### Skip
(defstruct (skip (:include col)))
(defmethod add1 ((x skip) y &optional (n 1)) y)

; ### Sym
(defstruct (sym (:include col))  seen mode (most 0))
(defmethod add1 ((s sym) y &optional (n 1))
  (let ((new (inca y (o s seen) n)))
    (when (> new (o s most))
      (setf most new
            mode y)))
  y)

(defmethod mid ((s sym)) (o s mode))
(defmethod var ((s sym)) (entropy s))

(defmethod entropy ((s sym) &aux (e 0))
  (dolist (x (o s seen) e)
    (let ((p (/ (cdr x) (o s n))))
      (decf e (* p (log p 2))))))
                     
(defmethod dist1 ((c sym) x y) (if (eql x y) 1 0))

; ### Num
(defstruct (num (:include col))
  (_all (make-array 32 :fill-pointer 0 :adjustable t))
  sorted)

(defmethod add1 ((n num) (x string) &optional (r 1))
  (add1 n (read-from-string x) r))

(defmethod add1 ((n num) (x number) &optional (r 1))
  (loop do repeat r do (push-vector-extend x (o n all))
  (setf (o n sorted) nil)
  y)

(defmethod all ((n num))
  (unless (o n sorted)
    (setf (o n _all)   (sort (o n _all) #'<)
          (o n sorted) t))
   (o n _all))

(defmethod mid ((n num)) (per n .5))
(defmethod var ((n num))  (sd n))
(defmethod sd  ((n num)) (/ (- (per n .9) (per n .1)) 2.56))
(defmethod per ((n num) &optional (p .5))
  (let* ((v (all n))
         (s (length v)))
    (svref v (floor (* p s)))))

(defmethod lo ((n  num)) (svref (all n) 0))
(defmethod hi ((n  num) &aux (a (all  n))) (svref a (1- (length a)))))

(defmethod dist1 ((n num) a b)
  (cond ((eq a #\?) (setf b (norm n b)
                          a (if (> b 0.5) 1 0)))
        ((eq b #\?) (setf a (norm n  a)
                          b (if (> a 0.5) 1 0)))
        (t          (setf a (norm n a)
                          b (norm n b))))
  (abs (- a b)))

(defmethod norm ((n num) x)
  (if (eq x #\?)
      x
    (let ((n1 (lo n)) (n2 (hi n)))
      (if (eql n1 n2)
          0
        (max 0 (min 1 (/ (- x n1) (- n2 n1 1E-32))))))))
  
