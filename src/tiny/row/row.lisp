(defstruct+ row 
   "Hold one record"
   cells    ; cells 
   _parent  ; pointer to someone who can say what are (e.g.) lo,hi
   evaled)  ; have we used the y values

(defun make-row (rows lst) 
  "Create."
  (%make-row :_parent rows :cells lst))

(defmethod better ((row1 row) (row2 row))
  "Row1 better than row2 if jumping away is better jumping to."
  (let* ((s1 0) (s2 0) 
                (cols (? row1 _parent cols y)) 
                (n (length cols)))
    (setf (? row1 evaled) t
          (? row2 evaled) t)
    (dolist (col cols (< (/ s1 n) (/ s2 n)))
      (with-slots (at w) col
        (let ((x (norm col (elt (? row1 cells) at)))
              (y (norm col (elt (? row2 cells) at))))
          (decf s1 (exp (* w (/ (- x y) n))))
          (decf s2 (exp (* w (/ (- y x) n))))))))) 

(defmethod around ((row1 row) allrows)
  "Sort `allrows` by distance to `row1`."
  (labels ((two (row2) (cons (dist (? row1 _parent cols)  row1 row2) row2)))
    (sort (mapcar 'two allrows) 'car<)))

(defmethod far ((i row) allrows)
  "Return something far away from `i`. Avoid outliers by only going so `far`."
  (cdr (elt (around i allrows) 
            (floor (* (length allrows) (? my far))))))
