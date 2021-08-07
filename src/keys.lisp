; vim: ts=2 sw=2 et:

(defpackage :keys (:use :cl))
(in-package :keys)

(defun loads(f)
  (format *error-output* "; loading ~(~a~) ...~%" f)
  (handler-bind ((style-warning #'muffle-warning)) (load f)))

(loads "tricks")
(loads "config")
(loads "structs")
(loads "col")
(loads "num")
(loads "sym")
(loads "row")
(loads "rows")
(loads "egs")

(main +config+ :package :keys)
