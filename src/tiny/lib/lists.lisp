; sort predicates
(defun lt (x) (lambda (a b) (< (slot-value a x) (slot-value b x))))
(defun gt (x) (lambda (a b) (> (slot-value a x) (slot-value b x))))

(defun car< (x) (lambda (a b) (< (car a) (car b))))
(defun car> (x) (lambda (a b) (> (car a) (car b))))

(defmethod any ((i cons))   (any (coerce 'vector i)))
(defmethod any ((i vector)) (elt i (random (length i))))

(defmethod many ((i cons) &optional (n 10)) (many (coerce 'vector i) n))
(defmethod many ((i vector) &optional (n 10)) 
  (loop repeat n collect (any i)))
;; (5 1 3 5 4 0 7 4 9 1)

(dotimes (i 20) (print (any #(10 20 30))))
(print (sort (many #(10 20 30 40 50 60 70 80 90 100
                     110 120 130 140 150) 5) #'<))
