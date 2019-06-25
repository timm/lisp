; vim: ts=2 sw=2 sts=2  et
(unless (fboundp 'got) (load "../got"))

(defun string-lines (str)
  "Convert a string to a list of lines."
  (labels
    ((nl    (z)    (char= z #\Newline))
     (where (pos0) (position-if #'nl str :start pos0))
     (worker (pos0 &aux pos)
        (if (setf pos (where pos0))
            (cons (subseq str pos0 pos)
                  (worker (1+ pos)))
            (list (subseq str pos0)))))
    (worker 0)))

(defun reads (f &key  (act #'print) (get #'read) (str t))
  "Read  a file, calling 'fn' on each s-expression. "
  (with-open-file (s f)
    (loop for lst = (funcall get s nil)
          while lst do 
          (funcall act lst))))      

(defun para1 (f &aux tmp)
  (reads f :get #'read-line :act (lambda (s)
    (if (equalp "" (string-trim '(#\Space #\Tab) s))
        (return-from 
           para1 
          (format nil "~{~a~^~%~}" (reverse tmp)))
        (push s tmp)))))
