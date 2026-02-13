#!/usr/bin/env sbcl --script
;; <!-- vim: set  ts=2 sw=2 sts=2 et : -->
#+sbcl (declaim (sb-ext:muffle-conditions cl:style-warning))

(defvar +it+  '(seed 1 file "autos.lisp"))
(defmacro ? (x) `(getf +it+ ',x))

(print (csv (? file) #'print))
