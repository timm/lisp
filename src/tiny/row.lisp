; Hold one record.
(defstruct+ row cells    ; cells 
                _about   ; pointer to someone who can say what are (e.g.) lo,hi
                evaled)  ; have we used the y values

(defun make-row (about l) (%make-row :cells l :_about about))

(defmethod lt ((row1 row) (row2 row))
  (setf (? row1 evaled) t
        (? row2 evaled) t)
  (let* ((s1 0) (s2 0) (d 0) (n 0) (cols (? row1 _about y)) (n (length cols)))
    (dolist (col cols)
       (let ((x (norm col (elt (? row1 cells) (? col at))))
             (y (norm col (elt (? row2 cells) (? col at)))))
        (decf s1 (exp (* (? col w) (/ (- x y) n))))
        (decf s1 (exp (* (? col w) (/ (- y x) n))))))
    (< (/ s1 n) (/ s2 n))))
