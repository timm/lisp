
(defstruct tests  all)
(defvar *tests* (make-tests))

(defmethod run ((te tests))
  (let (fail (n 0) (y 0))
    (dolist (one (tests-all te))
      (cond ((eval  one) (incf y))
            (t           (incf n)
	                 (push one fail))))
    (when fail
	(mapc #'print fail)
	(format t "~%PASS = ~s FAIL = ~s"  y n))))

(defmethod build ((te tests) x)
  (pushnew x (tests-all te) :test #'equalp))


(defmacro ok (&body body) `(build *tests* ',@body))

(ok (equal 1 1))
(ok (equal t 1))

(run *tests*)
