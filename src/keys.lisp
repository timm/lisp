; vim: ts=2 sw=2 et:

(defpackage :keys (:use :cl))
(in-package :keys)

(defun loads(f)
  (format *error-output* "; loading ~(~a~) ...~%" f)
  (handler-bind ((style-warning #'muffle-warning)) (load f)))

(defvar +config+
  `(all (fails 0  tries 0
         seed 10013  
         data "../data/aa" 
         loud nil un nil)
    col (p 2)
    dom (samples 100)))

(loads "tricks")
(loads "col")
(loads "num")
(loads "sym")
(loads "row")
(loads "rows")
(loads "egs")

(main +config+ :package :keys)
