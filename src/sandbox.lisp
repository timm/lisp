
;.  


(defmethod str2thing1 (x) x)
(defmethod str2thing1 ((x string))
  "coerce `x` from a string to a non-string"
  (let ((x (string-trim '(#\Space #\Tab) x)))
    (if (equal x "?") 
      #\?
      (let ((y (ignore-errors (read-from-string x))))
        (if (numberp y) y x)))))

(defun str2thing2 (x)
  "coerce `x` from a string to a non-string"
  (if (not (stringp x))
    x
    (let ((x (string-trim '(#\Space #\Tab) x)))
      (if (equal x "?") 
        #\?
        (let ((y (ignore-errors (read-from-string x))))
          (if (numberp y) y x))))))


(let ((a 0))
(time (dotimes (i 100000) (incf a))))

(time (dotimes (i 100000) (str2thing1 23)))
(time (dotimes (i 100000) (str2thing2 41)))

