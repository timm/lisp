; vim: ts=2 sw=2 et:
(defmethod init ((c col)) c)

(defmethod add ((c col) (x cons)) (dolist (y x) (add c y)))
(defmethod add ((c col) (x row))  (add  c (row-cells c)))
(defmethod add ((c col) x)
  (unless (eq #\? x)
    (incf (? c n))
    (add1 c x))
  y)

(defmethod dist ((c col) x y)
  (if (and (eq x #\?) (eq y #\?)) 1 (dist1 x y)))
