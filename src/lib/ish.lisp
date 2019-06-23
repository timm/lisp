(pushnew :ish *FEATURES*)

(format nil "# ISH")
(let (seen)
  (defun has (&rest lst)
    (dolist (f lst)
      (let ((f (format nil "../~a.lisp" f)))
	(unless  (member f seen :test #'equalp)
	  (format t "; ~a~%" f)
	  (push f seen)  
	  #-sbcl
	  (load f) 
	  #+sbcl
	  (handler-bind
	    ((style-warning #'muffle-warning))
	    (load f)))))))
