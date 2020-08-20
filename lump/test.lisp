; vim: noai:ts=2:sw=2:et: 
(format *error-output* "; test.lisp~%")
(or (boundp '*the*) (load "the"))
(or (fboundp 'send) (load "oo"))

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
