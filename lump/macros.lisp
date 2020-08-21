; vim: noai:ts=2:sw=2:et: 
(defmacro while (test &body body) 
  `(do () ((not ,test)) ,@body))

(defmacro getr (how obj f &rest fs)
  "e.g. getr slot-value does recursive slot-value access"
  (if fs 
      `(getr ,how (,how ,obj ',f) ,@fs) 
      `(,how ,obj ',f)))


