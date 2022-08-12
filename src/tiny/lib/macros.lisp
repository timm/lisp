; Simple alist access
(defmacro ! (l x) `(cdr (assoc ',x ,l)))

; ? obj x y z) == (slot-value (slot-value (slot-value obj 'x) 'y) 'z)
(defmacro ? (s x &rest xs)
 (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))

; Endure lst has a slot for `x`. If missing, initialize it with `init`.
(defmacro geta (x lst &optional (init 0))
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))
