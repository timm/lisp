; Place to hold rows, and their sumamries.
(defstruct+ rows rows   ; all the rows
                 cols)  ; summaries of all the columns

(defun make-rows (&optional src (i (%make-rows)))
  (if (stringp src)
    (with-lines src (lambda (line) (add i (cells line))))
    (dolist (row src) (add i row)))
  i)

(defmethod clone ((i rows) &optional src) 
  (make-rows (? i cols names) src))

(defmethod add ((i rows) x) 
  (if (? i cols)
    (push (add (? i cols) x) (? i rows))
    (setf (? i cols) (make-cols x))))
