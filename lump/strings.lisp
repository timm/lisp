; vim: noai:ts=2:sw=2:et: 
"Misc string tricks"

(defun words (s &optional 
                (lo 0) 
                (hi (position #\, s :start (1+ lo))))
  "Separate a string `s` on commas"
  (cons 
    (string-trim '(#\Space #\Tab #\Newline) (subseq s lo hi))
    (if hi (words s (1+ hi)))))

(defun lines (s &optional 
                (lo 0) 
                (hi (position #\Newline s :start (1+ lo))))
  "Separate a string `s` on newline."
  (cons (words (subseq s lo hi)) 
        (if hi (lines s (1+ hi)))))

(defmacro with-csv ((line file) &body body)
  "Return one list per line, words separated by commas."
  (let ((str (gensym)))
    `(with-open-file (,str ,file)
       (while (setf ,line (read-line ,str nil))
         (setf ,line (words ,line))
         ,@body))))


