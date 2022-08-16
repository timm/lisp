; Place to hold rows, and their sumamries.
(defstruct+ rows rows   ; all the rows
                 cols)  ; summaries of all the columns

(defun make-rows (&optional src (i (%make-rows)))
  (labels ((top.row.is.special  (x) (if (? i cols) 
                                     (push (add i x) (? i rows)) 
                                     (setf (? i cols) (make-cols x)))))
    (if (stringp src)
      (with-lines src (lambda (line) (top.row.is.special (cells line))))
      (mapcar #'top.row.is.special src))
    i))

(defmethod clone ((i rows) &optional src) 
  (make-rows (cons (? i cols names) src)))

(defmethod add ((i rows) (lst cons)) (add i (make-row i lst)))
(defmethod add ((i rows) (row1 row))
  (dolist (cols `(,(? i cols x) ,(? i cols y)) row1)
    (dolist (col cols)
      (add col (elt (? row1 cells) (? col at))))))

(defmethod dist ((i rows) (row1 row) (row2 row))
  (let ((d 0) (n 0) (p (! my p)))
    (dolist (col (? i cols x))
      (incf n)
      (incf d (expt (dist col (elt (? row1 cells) (? col at)) 
                              (elt (? row2 cells) (? col at))) 
                    p)))
    (expt (/ d n) (/ 1 p))))

(defmethod half ((i rows) &optional all above)
  (or all (? i rows))
  (print 1)
  (let* 
    ((all   (or   all (? i rows)))
     (some  (many all (? my far)))
     (left  (or   above (far (any some) some)))
     (right (far  left some))
     (c     (dist (? i _parent) left right))
     (tmp   (mapcar (lambda (row) 
                      (print 2)
                      (let ((a (dist (? row _parent) row left))
                            (b (dist (? row _parent) row right)))
                        (cons (/ (+ (* a a) (* c c) (- (* b b))) (* 2 c)) row)))
                    all)))
    (print 1)
    (let ((n 0) lefts rights)
      (dolist (one (sort tmp #'car<))
        (if (< (incf n) (/ (length tmp) 2))
          (push (cdr one) lefts)
          (push (cdr one) rights)))
      (values left right lefts rights c))))
