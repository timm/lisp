; vim: ts=2 sw=2 et:

; Add a symbo, update symbol counts,  update mode
(defmethod add1 ((s sym) x)
  (let ((n (inca x (? s seen))))
    (when (> n (? s most))
      (setf (? s most) n
            (? s mode) x)))
  x)

; Central tendency.
(defmethod mid ((s sym)) (? s mode))

; Variable around centrality.
(defmethod var ((s sym)) (entropy (? s seen)))

; Seperation of two items.
(defmethod dist1 ((s sym) x y) (if (eql x y) 0 1))
