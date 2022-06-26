(defstruct row cells used)
(defstruct (cols (%constructor %make-cols))  all x y name)
(defstruct rows kept cols)

(defmacro λ (args &rest rest) 
  "shorthand macro for lambda"
  `(lambda ,args ,@rest))

(defmacro ? (s x &rest xs)
  "Recursive struct accessors; e.g. `(? s address street number)`."
  (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))

(defun reads (file f)
  "iterate `f` over all items in `file`"
  (with-open-file (s file) 
    (labels ((reads1 (x) (when x (funcall f x) (reads1 (read s nil)))))
      (reads1 (read s nil)))))

(reads "../../data/auto93.lisp" (λ (x) (print x)))

(defun %make-cols (row &aux (i (%make-cols)))
  (labels ((col (word)
     
    (setf (? i all) (mapcar #'col row)))
(defmethod add ((c cols) (r row))
  (dolist (slot '(x y) r)
    (dolist (col (slot-vaue c slot)) (add col (elt row (? col at))))))

(defmethod add ((i rows) (r cons))
  (add i (make row :cells r)))

(defmethod add ((i rows) (r row))
  (if (? i cols)
      (push (mapcar #'add (? i cols) r) (? i kept))
      (setf (? i cols) (make-cols row))))
