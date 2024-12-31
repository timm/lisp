(defmacro ? (s x &rest xs)    
  (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))

(defmacro inca (x a &optional (n  1))
  `(incf 
     (cdr (or (assoc ,x ,a :test #'equal) 
              (car (setf ,a (cons (cons ,x 0) ,a))))) ,n))

(defstruct num (at 0) (txt "") (n 0) (mu 0) (m2 0))
(defstruct sym (at 0) (txt "") (n 0) has )
(defstruct cols all x y)
(defstruct sample rows (cols (make-cols)))

(defmethod add ((s sample) lst &aux (n -1)) 
  (if cols
    (mapc #'(lambda (col x) (datum s col x (incf n)  )) lst (? s col) lst)
    (mapc #'(lambda (x)     (cell s        (incf n) x)) lst)))

(defun datum (sample col val at)
  (loop for i from 0 and x across lst
      do (format t "~a = ~a~%" i x))
  (loop for col across (? s cols) do
        (add col (aref v (? col at))))
  (if (? s keep)
    (push (?  s rows) (coerce v 'vector))))

(defun add-columns (sample lst)
  (dolist (n (length v))
     (let ((name (aref (add-column s n (aref v s))))))))

