; vim:  ts=2 sw=3 sts=2 et :

;------------------;-------------------;-------------------;-------------------;
; macros
(defmacro want (x &rest y) `(assert ,x () ,@y))

(defmacro ? (s x &rest xs) 
  (if xs `(? (slot-value ,s ',x) ,@xs) `(slot-value ,s ',x)))

(defmacro inca (x a &optional (inc  1))
  `(incf (cdr (or (assoc ,x ,a :test #'equal)
                  (car (setf ,a (cons (cons ,x 0) ,a))))) ,inc))

(defmacro ! (s x &rest xs) (if xs `(! (getf ,s ',x) ,@xs) `(getf ,s ',x)))

;------------------;-------------------;-------------------;-------------------;
; csv reading
(defun num? (s)
  (if (eql "?" s)
    s
    (let ((n (read-from-string s)))
      (if (numberp n) n s))))

(defun s2cells (s &optional (x 0) (y (position #\, s :start (1+ x))))
  (labels ((white(x) (member x '(#\Space #\Tab #\Newline))))
    (cons (num? (remove-if #'white (subseq s x y)))
          (and y (s2cells s (1+ y))))))

(defun csv (file fn)
  (with-open-file (str file)
    (loop (funcall fn (s2cells (or (read-line str nil) (return-from csv)))))))

(defmacro with-csv ((lst file &optional out) &body body)
   `(progn (csv ,file #'(lambda (,lst) ,@body)) ,out))

