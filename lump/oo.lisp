; vim: noai:ts=2:sw=2:et: 
(format *error-output* "; oo.lisp~%")
(or (boundp '*my*) (load "my"))

(defmacro send (obj f &rest args)
  `(funcall (slot-value ,obj ',f) ,obj ,@args))

(defmacro ? (obj head &rest more)
  "From https://goo.gl/dqnmvH:"
  (if (null more)
    `(slot-value ,obj ',head)
    `(? (slot-value ,obj ',head) ,@more)))

(defmacro ?? (head &rest more)
  `(? *my* ,head ,@more))
