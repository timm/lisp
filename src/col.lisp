; vim: ts=2 sw=2 et:
(defmethod init ((c col)) c)

(defmethod add ((c col) (x cons)) (dolist (y x) (add c y)))
(defmethod add ((c col) (x row))  (add c (row-cells x)))
(defmethod add ((c col) x)
  (unless (eq #\? x) (incf (? c n)) (add1 c x))
  x)

(defmethod dist ((c col) x y)
  (if (and (eq x #\?) (eq y #\?)) 1 (dist1 x y)))


