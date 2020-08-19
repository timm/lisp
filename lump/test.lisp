; vim: noai:ts=4:sw=4:et: 
(format *error-output* "; test.lisp~%")
(or (boundp '*my*) (load "my"))
(or (fboundp 'send) (load "oo"))

(defun test (ok)
  (if ok
    (incf (?? tests pass))
    (progn 
      (incf (?? tests fail))
      (format t "~a fail ~%" (?? tests it)))))

(defmacro dofun (name args &body body)
  `(progn
     (setf (?? tests it) ',name)
     (funcall (lambda ,args ,@body))))
