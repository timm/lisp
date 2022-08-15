; sort predicates
(defun lt (x) (lambda (a b) (< (slot-value a x) (slot-value b x))))
(defun gt (x) (lambda (a b) (> (slot-value a x) (slot-value b x))))

(defun car< (x) (lambda (a b) (< (car a) (car b))))
(defun car> (x) (lambda (a b) (> (car a) (car b))))
