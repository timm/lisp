; vim: noai:ts=2:sw=2:et: 
(load "got")
(got "my")

(defun yes (status)
  (format t "~&~a " (my yes it))
  (if status
    (incf (my yes pass))
    (let ((p (* 100 
               (/ (my yes pass) 
                  (+ 1 (my yes pass) (my yes fail))))))
      (incf (my yes fail))
      (format t "fail passing ~a %"  p)))
   (terpri))

(defmacro dofun (name args &body body)
  `(progn
     (setf (my yes it) ',name)
     (funcall (lambda ,args ,@body))))
