; vim: noai:ts=2:sw=2:et: 
(defmacro while (test &body body) 
  `(do () ((not ,test)) ,@body))

(defmacro getr (how obj f &rest fs)
  "e.g. getr slot-value does recursive slot-value access"
  (if fs 
      `(getr ,how (,how ,obj ',f) ,@fs) 
      `(,how ,obj ',f)))

(defmacro do-hash ((k v h &optional out) &body body )
  "Set key 'k' and value 'v' to items in hash"
  `(progn (maphash #'(lambda (,k ,v) ,@body) ,h) ,out))

(defmacro doitems ((one n list &optional out) &body body )
  `(let ((,n -1))
     (dolist (,one ,list ,out)
       (incf ,n)
       ,@body)))
