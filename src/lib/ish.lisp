(pushnew :ish *FEATURES*)

(format t "; ISH")
(let (seen)
  (defun needz (&rest lst)
    (dolist (f lst)
      (let ((f (format nil "../~a.lisp" f)))
	(unless  (member f seen :test #'equalp)
	  (format t "~&; loading ~a~%" f)
	  (push f seen)  
	  #-sbcl
	  (load f) 
	  #+sbcl
	  (handler-bind
	    ((style-warning #'muffle-warning))
	    (load f)))))))
