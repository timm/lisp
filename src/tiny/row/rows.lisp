(defstruct+ rows 
  "Stores multiple rows, and their summaries."
  _has   ; all the rows
  cols)  ; summaries of all the columns

(defun make-rows (&optional src (i (%make-rows)))
  "Eat first row for the column header,  add the rest"
  (labels ((top.row.is.special  (x) (if (? i cols) 
                                     (push (add i x) (? i _has)) 
                                     (setf (? i cols) (make-cols x)))))
    (if (stringp src)
      (with-lines src (lambda (line) (top.row.is.special (cells line))))
      (mapcar #'top.row.is.special src))
    i))

(defmethod clone ((i rows) &optional src) 
  "Create a new table with same structure as `i`."
  (make-rows (cons (? i cols names) src)))

(defmethod add ((i rows) (lst cons)) 
  "Row creation. Called in we try to add a simple list."
  (add i (make-row i lst)))

(defmethod add ((i rows) (row1 row))
  "For all the unskipped columns, update from `row1`."
  (dolist (cols `(,(? i cols x) ,(? i cols y)) row1)
    (dolist (col cols)
      (add col (elt (? row1 cells) (? col at))))))

(defmethod unsupervised-discretization ((i rows))
  (dolist (col (? i cols x))
    (dolist (row rows)
      (setf (elt (? row cooked) (? col at))
            (discretize col (elt (? row cooked) (? col at)) (? my bins))))))

   (dolist (row (? i _has))
(defmethod half ((i rows) &optional all above)
  "Split rows in two by their distance to two remove points."
  (let* ((all   (or    all (? i _has)))
         (some  (many  all (! my some)))
         (left  (or    above (far (any some) some)))
         (right (far   left some))
         (c     (dists left right))
         (n 0)  lefts rights)
    (labels ((project (row)
                (let ((a (dists row left))
                      (b (dists row right)))
                  (cons (/ (+ (* a a) (* c c) (- (* b b))) (* 2 c)) row))))
      (dolist (one (sort (mapcar #'project all) #'car<))
        (if (<= (incf n) (/ (length all) 2))
          (push (cdr one) lefts)
          (push (cdr one) rights)))
      (values left right lefts rights c))))

