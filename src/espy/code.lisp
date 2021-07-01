; vim: noai:ts=2:sw=2:et: 
(defpackage :code (:use :cl))
(in-package :code)

(let (gotten)
  (defun got (&rest files)
    (mapc #'(lambda(file)
              (unless (member file gotten :test 'equalp)
                (push file gotten)
                (format *error-output* "; ~(~a~).lisp~%" file)
                (handler-bind ((style-warning #'muffle-warning))
                  (load file))))
          files)))

