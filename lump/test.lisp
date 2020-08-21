; vim: noai:ts=2:sw=2:et: 
(or (fboundp 'lib) (load "lib"))
(lib "oo")

(defun test (yes)
  (print (? ok it))
  (if yes
    (incf (? ok pass))
    (let ((p (* 100 
               (/ (? ok pass) 
                  (+ 1 (? ok pass) (? ok fail))))))
      (incf (? ok fail))
      (format t "fail passing ~a %"  p)))
   (terpri))

(defmacro dofun (name args &body body)
  `(progn
     (setf (? ok it) ',name)
     (funcall (lambda ,args ,@body))))
