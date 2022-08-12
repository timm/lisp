; Summarize symbolic columns
(defstruct+ sym  (txt "")  ; column name
                 (at 0)    ; column position
                 (n 0)     ; #items seen
                 kept)     ; symbol counts of the items

(defun make-sym (&optional s n) (%make-sym :txt s :at n))

(defmethod add ((i sym) (lst cons)) (dolist (x lst i) (add i x)))
(defmethod add ((i sym) x)
  (unless (eq x #\?)
    (incf (? i n))
    (incf (geta x (? i kept)))))

(defmethod adds ((i sym) x inc)
  (incf (? i n) inc)
  (incf (geta x (? i kept)) inc))

(defmethod div ((i sym))
	(labels ((fun (p) (* -1 (* p (log p 2)))))
		(loop for (_ . n) in (? i kept) sum (fun (/ n (? i n))))))

(defmethod mid ((i sym))
  (loop for (key . n) in (? i kept) maximizing n return key))
