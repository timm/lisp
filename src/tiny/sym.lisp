(defstruct+ sym  (txt "")  ; column name
                 (at 0)    ; column position
                 (n 0)     ; #items seen
                 kept)     ; symbol counts of the items

(defun make-sym (&optional s n) (%make-sym :txt s :at n))

(defmethod add ((i sym) (lst cons))
  (dolist (x lst i) (add i x)))

(defmethod add ((i sym) x)
  (unless (eq x #\?)
    (incf (? i n))
    (incf (geta x (? i kept)))))

(defmethod adds ((i sym) x inc)
  (incf (? i n) inc)
  (incf (geta x (? i kept)) inc))

(defmethod div ((i sym))
  (let ((out 0))
    (dolist (two (? i kept) out)
      (let ((p (/ (cdr two) (? i n)))) 
        (decf out (* p (log p 2)))))))
