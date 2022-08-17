(defun rnd (number &optional (digits 3))
  "Round to `digits` decimal places."
  (let* ((div (expt 10 digits))
         (tmp (/ (round (* number div)) div)))
    (if (zerop digits) (floor tmp) (float tmp))))

(defvar *seed* 10013)
(defun randf (&optional (n 1.0)) 
  "Random float 0.. n"
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun randi (&optional (n 1)) 
  "Random int 0..n"
  (floor (* n (/ (randf 1000000000.0) 1000000000))))
