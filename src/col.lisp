; vim: ts=2 sw=2 et:

; Methods
; -------
; If `x` is a list, add everything in it.
(defmethod add ((c col) (x cons)) (dolist (y x) (add c y)))
; Unless we are skipping  stuff, increment `n`.
(defmethod add ((c col) x)
  (unless (eq #\? x) 
    (incf (? c n)) 
    (add1 c x)))

; Functions
; ---------
(defmethod dist (c x y)
  (if (and (eq x #\?) (eq y #\?)) 1 (dist1 x y)))
