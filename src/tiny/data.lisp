; Place to hold rows, and their sumamries.
(defstruct+ data rows    ; all the rows
                 about)  ; summaries of all the columns

(defun make-data (names &optional src (i (%make-data :about (make-about names))))
  (if (stringp src)
    (with-lines src (lambda (line) (add i (cells line))))
    (dolist (row src) (add i row)))
  i)

(defmethod clone ((d data) &optional src) (make-data (? d about names) src))
