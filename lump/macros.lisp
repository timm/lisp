; vim: noai:ts=2:sw=2:et: 

(defmacro while (test &body body) 
  "Adding a `while` loop to LISP."
  `(do () ((not ,test)) ,@body))

(defmacro getr (how obj f &rest fs)
  "Recursive access to contents."
  (if fs 
      `(getr ,how (,how ,obj ',f) ,@fs) 
      `(,how ,obj ',f)))

(defmacro ? (x &rest fs) 
  "Recursive access to slot instances"
  `(getr  slot-value ,x ,@fs))

(defmacro do-hash ((k v h &optional out) &body body )
  "Set key `k` and value `v` to items in hash. Returns `out`."
  `(progn (maphash #'(lambda (,k ,v) ,@body) ,h) ,out))

(defmacro doitems ((one pos lst &optional out) &body body )
  "Item `one` is found at `pos` in `lst`. Returns `out`."
  `(let ((,pos -1))
     (dolist (,one ,lst ,out)
       (incf ,pos)
       ,@body)))
