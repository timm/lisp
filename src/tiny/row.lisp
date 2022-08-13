; Hold one record.
(defstruct+ row cells    ; cells 
                _about   ; pointer to someone who can say what are (e.g.) lo,hi
                evaled)  ; have we used the y values

(defun make-row (about l) (%make-row :cells l :_about about))

(defmethod lt ((row1 row) (row2 row))
  (let* ((s1 0) (s2 0) (d 0) (n 0) 
                (cols (? row1 _about y)) 
                (n (length cols)))
    (setf (? row1 evaled) t
          (? row2 evaled) t)
    (dolist (col cols (< (/ s1 n) (/ s2 n)))
      (with-slots (at w) col
        (let ((x (norm col (elt (? row1 cells) at)))
              (y (norm col (elt (? row2 cells) at))))
          (decf s1 (exp (* w (/ (- x y) n))))
          (decf s2 (exp (* w (/ (- y x) n)))))))))

(defmethod around ((row1 row) rows)
  (labels ((two (row2) (cons row2 (dist row1 row2))))
    (sort (mapcar 'two rows) 'car<)))

(defmethod dist ((row1 row) (row2 row))
  (let ((d 0) (n 0))
    (dolist (col (? row1 _about x) (expt (/ d n) (? my p)))
      (incf n)
      (incf d (dist col (elt (? row1 cells) (? col at)) 
                        (elt (? row2 cells) (? col at)))))))
