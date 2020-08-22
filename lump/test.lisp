; vim: noai:ts=2:sw=2:et: 
(load "got")
(got "my")

(defun test (yes)
  (format t "~&~a " (my ok it))
  (if yes
    (incf (my ok pass))
    (let ((p (* 100 
               (/ (my ok pass) 
                  (+ 1 (my ok pass) (my ok fail))))))
      (incf (my ok fail))
      (format t "fail passing ~a %"  p)))
   (terpri))

(defmacro dofun (name args &body body)
  `(progn
     (setf (my ok it) ',name)
     (funcall (lambda ,args ,@body))))
