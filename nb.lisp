(defstruct num (at 0) (txt "") (n 0) (mu 0) (m2 0))
(defstruct sym (at 0) (txt "") (n 0) has )
(defstruct cols all x y)
(defstruct sample rows (cols (make-cols)))


(defmacro ? (s x &rest xs)
  (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))

(defmethod add ((s sample) (lst cons)) (add s (coerce lst 'vector)))
(defmethod add ((s sample) (v vector)) 
  (loop for col being the elements of (? s cols x) do
    (aref v (? col at))

:w
(defmacro inca (x a &optional (n  1))
  `(incf 
     (cdr (or (assoc ,x ,a :test #'equal) 
              (car (setf ,a (cons (cons ,x 0) ,a))))) 
     ,n))

(loop for x being the elements of (? s cols all) sample#(a b c d e) to 3
         do (print x))
