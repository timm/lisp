; sort predicates
(defun lt (x)     (lambda (a b) (< (slot-value a x) (slot-value b x))))
(defun gt (x)     (lambda (a b) (> (slot-value a x) (slot-value b x))))
(defun car< (a b) (< (car a) (car b)))
(defun car> (a b) (> (car a) (car b)))

; random sampling (with replacement).
(defmethod any ((i cons))   (any (coerce i 'vector)))
(defmethod any ((i vector)) (elt i (randi (length i))))

(defmethod many ((i cons)   &optional (n 10)) (many (coerce i 'vector) n))
(defmethod many ((i vector) &optional (n 10)) (loop repeat n collect (any i)))
