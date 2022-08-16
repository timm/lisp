; sort predicates
(defun lt (x) (lambda (a b) (< (slot-value a x) (slot-value b x))))
(defun gt (x) (lambda (a b) (> (slot-value a x) (slot-value b x))))

(defun car< (x) (lambda (a b) (< (car a) (car b))))
(defun car> (x) (lambda (a b) (> (car a) (car b))))

(defmethod any ((i cons))   (any (coerce 'vector i)))
(defmethod any ((i vector)) (elt i (random (length i))))

(defmethod many ((i cons)   &optional (n 10)) (many (coerce 'vector i) n))
(defmethod many ((i vector) &optional (n 10)) (loop repeat n collect (any i)))
