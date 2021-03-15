(defun timings (function &optional (repeats 10))
  (let ((real-base (get-internal-real-time))
        (run-base (get-internal-run-time)))
    (dotimes (_ repeats) (funcall function))
    (values (/ (- (get-internal-real-time) real-base) internal-time-units-per-second) 
            (/ (- (get-internal-run-time) run-base) internal-time-units-per-second)) ))


(defstruct s1 b c d)
(defstruct (s2 (:include s1)) e f g)

(defun t0 ()
  (dotimes (i 1000) 
   (let ((x t))
      (eq x t))))

(defun t1 ()
  (dotimes (i 1000) 
   (let ((x (make-s2)))
     (setf (slot-value x 'e) 10))))


(defun  s11 () `(:a nil :b nil :c nil :d nil))
(defun  s21 ()  `(:e nil :f nil :g nil :h nil ,@(s11)))

(defun t2 ()
  (dotimes (i 1000) 
    (let ((x (s21)))
			(setf (getf  x :d) 10))))

(format t "t0 ~5,3f~%" (timings #'t0 100000))
(format t "t1 ~5,3f~%" (timings #'t1 100000))

(format t "t2 ~5,3f~%" (timings #'t2 100000))

(print (loop for x being the external-symbol of "CL" 
        when (fboundp x) collect (cons x (string x))))
