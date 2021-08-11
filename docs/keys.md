---
title: "keys: "
---


```lisp
(defpackage :keys (:use :cl))
(in-package :keys)

(defun loads(f)
  (format *error-output* "; loading ~(~a~) ...~%" f)
  (handler-bind ((style-warning #'muffle-warning)) (load f)))

(loads "structs")
(loads "tricks")
(loads "config")
(loads "col")
(loads "num")
(loads "sym")
(loads "row")
(loads "rows")
(loads "egs")

(main +config+ :package :keys)

```
