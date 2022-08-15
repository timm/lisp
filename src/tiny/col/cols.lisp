; Factory for making nums or syms. Also controls updating those nums+syms.
(defstruct+ cols names  ; list of column names
                  all    ; all the generated columns
                  x      ; just the independet columns
                  y      ; just the dependent columns
                  klass) ; just the klass col (if it exists)

(defun make-cols (lst)
  (print `(cols ,lst))
  (let (all x y kl (at -1))
    (dolist (str lst (%make-cols :names lst :x x :y y :klass kl
                                  :all (reverse all)))
      (incf at)
      (let* ((what (if (upper-case-p (char str 0)) #'make-num #'make-sym))
             (col  (funcall what str at)))
        (push col all)
        (unless (eq #\~ (charn str))
          (if (member (charn str) '(#\! #\- #\+)) (push col y) (push col x))
          (if (eq #\! (charn str)) (setf kl col)))))))

(defmethod add ((i cols) (lst cons)) (add i (make-row i lst)))
(defmethod add ((i cols) (row1 row))
  (dolist (cols `(,(? i x) ,(? i y)) row1)
    (dolist (col cols)
      (add col (elt (? row1 cells) (? col at))))))

(defmethod dist ((self cols) (row1 row) (row2 row))
  (let ((d 0) (n 0))
    (dolist (col (? self x) (expt (/ d n) (! my p)))
      (incf n)
      (incf d (dist col (elt (? row1 cells) (? col at)) 
                        (elt (? row2 cells) (? col at)))))))
