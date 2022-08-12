; Factory for making nums or syms. Also controls updating those nums+syms.
(defstruct+ about names  ; list of column names
                  all    ; all the generated columns
                  x      ; just the independet columns
                  y      ; just the dependent columns
                  klass) ; just the klass col (if it exists)

(defun make-about (lst)
  (let (all x y kl (at -1))
    (dolist (str lst (%make-about :names lst :x x :y y :klass kl
                                  :all (reverse all)))
      (incf at)
      (let ((col (if (eq #\$ (char str 0)) (make-num str at) (make-sym str at))))
        (push col all)
        (unless (eq #\~ (charn str))
          (if (member (charn str) '(#\! #\- #\+)) (push col y) (push col x))
          (if (eq #\! (charn str)) (setf kl col)))))))

(defmethod add ((i about) (lst cons)) (add i (make-row i lst)))
(defmethod add ((i about) (row1 row))
  (dolist (cols `(,(? i x) ,(? i y)) row1)
    (dolist (col cols)
      (add col (elt (? row1 cells) (? col at))))))
