; vim: noai:ts=2:sw=2:et: 
(format *error-output* "; oo.lisp~%")
(or (boundp '*the*) (load "the"))

(defmacro send (obj f &rest args)
  `(funcall (slot-value ,obj ',f) ,obj ,@args))

(defmacro has (x &rest fs) 
  `(getr  slot-value ,x ,@fs))
