; vim: ts=2 sw=2 et :

(defun chunk (lst &optional (c (sqrt (length lst))))
  (let ((out '(())))
    (dolist (one lst (reverse (mapcar 'reverse out)))
      (when (>= (length (car out)) c)
        (push '() out))
      (push one (car out)))))

(defun pdf (lst)
  (setf lst (sort lst '<))
  (let ((b4 (car lst)))
    (cons b4 (loop for x in (cdr lst) collect 
      (let ((diff (- x b4)))
        (setf b4 x)
        diff)))))



(defun like-num (x mu sd)
  (cond ((< x (- mu (* 4 sd))) 0)
        ((> x (+ mu (* 4 sd))) 0)
        (t  (let 
              ((denom (sqrt (* 2 pi sd sd)))
               (nom   (exp (/ (* -1 (expt (- x mu) 2)) (+ 1E-32 (* 2 sd sd))))))
              (/ nom (+ denom 1E-32))))))



(defun mean (lst ) (print lst)  (float (/ (reduce '+ lst) (+ 1E-32 (length lst)))))

 (print       (mapcar 'mean i
   (chunk '(1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 3 3 3 3 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 6 6 6))))

(defmacro fn (arg &rest body) `(lambda ,arg ,@body))

(def aa (x y) (+ x y))

(defun %let+ (body xs)
  (labels ((fun (x) (and (listp x) (> (length x) 2)))
           (mvb (x) (and (listp x) (listp (car x)))))
    (if (null xs)
      body
      (let ((x (pop xs)))
        (cond
          ((fun x) `(labels ((,(pop x) ,(pop x) ,@x))       ,(%let+ body xs)))
          ((mvb x) `(multiple-value-bind ,(pop x) ,(pop x) ,@(%let+ body xs)))
          (t       `(let (,x)                          ,(%let+ body xs))))))))

(defmacro let+ (spec &rest body) (%let+ body spec))



