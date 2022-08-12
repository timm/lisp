(defstruct+ num (txt "") (at 0) (n 0) kept ok (w 1))

(defun make-num (s n) (%make-num :txt s :at n :w (if (eq #\- (charn s)) -1 1)))


(defmethod add ((i num) (lst cons))
  (dolist (x lst i) (add i x)))

(defmethod add ((i num) x)
  (unless (eq x #\?)
    (incf (? i n))
    (add (? i kept) x)))

