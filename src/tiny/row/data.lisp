(defstruct+ data 
            "Stores multiple rows, and their summaries."
            rows   ; all the rows
            cols)  ; summaries of all the columns

(defun make-data (&optional src (i (%make-data)))
  "Eat first row for the column header,  add the rest"
  (labels ((top.row.is.special  (x) (if (? i cols) 
                                     (push (add i x) (? i rows)) 
                                     (setf (? i cols) (make-cols x)))))
    (if (stringp src)
      (with-lines src (lambda (line) (top.row.is.special (cells line))))
      (mapcar #'top.row.is.special src))
    i))

(defmethod clone ((i data) &optional src) 
  "Create a new table with same structure as `i`."
  (make-rows (cons (? i cols names) src)))

(defmethod add ((i data) (lst cons)) 
  "Row creation. Called in we try to add a simple list."
  (add i (make-row i lst)))

(defmethod add ((i data) (row1 row))
  "For all the unskipped columns, update from `row1`."
  (dolist (cols `(,(? i cols x) ,(? i cols y)) row1)
    (dolist (col cols)
      (add col (elt (? row1 cells) (? col at))))))

(defmethod dist ((i data) (row1 row) (row2 row))
  "Gap between `row1`, `row2`. At `p`=2, this is Euclidean distance."
  (let ((d 0) (n 0) (p (! my p)))
    (dolist (col (? i cols x))
      (incf n)
      (incf d (expt (dist col (elt (? row1 cells) (? col at)) 
                              (elt (? row2 cells) (? col at))) p)))
    (expt (/ d n) (/ 1 p))))

(defmethod half ((i data) &optional all above)
  "Split rows in two by their distance to two remove points."
  (or all (? i rows))
  (print 1)
  (let (all some left right c tmp) 
    (setf all  (or   all (? i rows)))
    (setf some  (many all (! my some)))
    (print  (any some))
     (setf left  (or   above (far (any some) some)))
    (return-from half  (print (length some)))
     (setf right (far  left some))
     (setf c     (dist (? i _parent) left right))
     (setf tmp   (mapcar (lambda (row) 
                      (print 2)
                      (let ((a (dist (? row _parent) row left))
                            (b (dist (? row _parent) row right)))
                        (cons (/ (+ (* a a) (* c c) (- (* b b))) (* 2 c)) row)))
                    all))
    (print 1)
    (let ((n 0) lefts rights)
      (dolist (one (sort tmp #'car<))
        (if (< (incf n) (/ (length tmp) 2))
          (push (cdr one) lefts)
          (push (cdr one) rights)))
      (values left right lefts rights c))))
