; Random number control (since reseeding in LISP is... strange).
(defvar *seed* 10013)
(defun randf (&optional (n 1.0)) 
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))
(defun randi (&optional (n 1)) (floor (* n (/ (randf 1000000000.0) 1000000000))))
