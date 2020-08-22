; vim: noai:ts=2:sw=2:et: 

(defvar *gotten* nil)

(defun got (&rest files)
  (mapc 
    (lambda (f)
      (unless (member f *gotten* :test 'equalp)
        (format *error-output* "; ~(~a~).lisp~%" f)
        (push f *gotten*)
        #-sbcl (load f) 
        #+sbcl (handler-bind
                 ((style-warning #'muffle-warning))
                 (load f))))
    files))
