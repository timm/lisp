; vim: noai:ts=2:sw=2:et: 
(format *error-output* "; strings.lisp~%")

(defun word (s lo &optional hi) 
  (string-trim '(#\Space #\Tab #\Newline) 
               (subseq s lo hi)))

(defun words (s &optional 
                (lo 0) 
                (hi (position #\, s :start (1+ lo))))
  (cons (word s lo hi) 
        (if hi (words s (1+ hi)))))

(defun lines (s &optional 
                (lo 0) 
                (hi (position #\Newline s :start (1+ lo))))
  (cons (words (subseq s lo hi)) 
        (if hi (lines s (1+ hi)))))

(defmacro with-csv ((line file) &body body)
  (let ((str (gensym)))
    `(with-open-file (,str ,file)
       (while (setf ,line (read-line ,str nil))
         (setf ,line (words ,line))
         ,@body))))
