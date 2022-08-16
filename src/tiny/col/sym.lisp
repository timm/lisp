(defstruct+ sym  
  "Summarize symbolic columns"
  (txt "")  ; column name
  (at 0)    ; column position
  (n 0)     ; #items seen
  kept)     ; symbol counts of the items

(defun make-sym (&optional s n) 
  "Create."
  (%make-sym :txt s :at n))

(defmethod add ((i sym) (lst cons)) 
  "Add a list of items."
  (dolist (x lst i) (add i x)))

(defmethod add ((i sym) x)
  "Add one items, skipping 'dont know', update frequency counts."
  (unless (eq x #\?)
    (incf (? i n))
    (incf (geta x (? i kept)))))

(defmethod adds ((i sym) x inc)
  "Bulk add of a symbol 'x', 'inc' times."
  (incf (? i n) inc)
  (incf (geta x (? i kept)) inc))

(defmethod mid ((i sym))
  "Middle"
  (loop for (key . n) in (? i kept) maximizing n return key))

(defmethod div ((i sym))
  "Diversity (entropy)."
  (labels ((fun (p) (* -1 (* p (log p 2)))))
    (loop for (_ . n) in (? i kept) sum (fun (/ n (? i n))))))

(defmethod dist ((i sym) x y)
  "Gap between 2 items; if unknown, assume max. distance."
  (cond ((and (eq #\? x) (eq #\? y)) 1)
        ((equal x y)                 0)
        (t                           1)))
