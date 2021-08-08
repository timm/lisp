; vim: ts=2 sw=2 et:
(defmethod add1 ((s sym) x)
  (let ((n (inca x (? s seen))))
    (when (> n (? s most))
      (setf (? s most) n
            (? s mode) x)))
  x)

(defmethod mid   ((s sym))     (? s mode))
(defmethod var   ((s sym))     (entropy (? s seen)))
(defmethod dist1 ((s sym) x y) (if (eql x y) 0 1))
