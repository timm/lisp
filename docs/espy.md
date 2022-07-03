<img src='http://www.lisperati.com/lisplogo_fancy_256.png' width=200 align=right>


(defpackage :espy (:use :cl))

(in-package :espy)

(defun loaded (file)
 (format *error-output* "; loading ~(~a~) ...~%" file)
 (handler-bind ((style-warning #'muffle-warning))
  (load file)))

(defvar +config+
 '(:all (:seed 10013 :data "../data/aaa.csv") :col (:p 2)
   :dom (:samples 100)))

(loaded "etc")

(loaded "es")

(loaded "eg")

(demos (cli))

(halt *fails*)

````


; W? DO DOC  FAIL
