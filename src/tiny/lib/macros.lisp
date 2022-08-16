; Simple alist access
(defmacro ! (l x) 
  "Get into association lists."
  `(cdr (assoc ',x ,l)))

(defmacro ? (s x &rest xs)
 "(? obj x y z) == (slot-value (slot-value (slot-value obj 'x) 'y) 'z)"
 (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))

(defmacro geta (x lst &optional (init 0))
  "Endure lst has a slot for `x`. If missing, initialize it with `init`."
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))
