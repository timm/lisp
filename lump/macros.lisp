; vim: noai:ts=4:sw=4:et: 
(format *error-output* "; macros.lisp~%")

(defmacro while (test &body body) 
  `(do () ((not ,test)) ,@body))

(defmacro getr (how obj f &rest fs)
  (if fs 
      `(getr ,how (,how ,obj ',f) ,@fs) 
      `(,how ,obj ',f)))


