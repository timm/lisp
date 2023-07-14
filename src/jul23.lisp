; vi: set ts=2 sw=2 sts=2 et  :
(defun trim (s) 
  "kill whitespace at start, at end"
  (string-trim '(#\Space #\Tab #\Newline) s))

(defun thing (s &aux (s1 s)) ;(trim s)))
  "coerce `s` into a number or string or t or nil or #\?"
  (cond ((equal s1 "?") #\?)
        ((equal s1 "t") t)
        ((equal s1 "nil") nil)
        (t (let ((n (read-from-string s1 nil nil))) 
             (if (numberp n) n s1)))))

(defun with-file (file fun &optional (filter #'split))
  "call `fun` for each line in `file`"
  (with-open-file (s file) 
    (loop (funcall fun (funcall filter (or (read-line s nil) (return)))))))

(defun split (s &optional (sep #\,) (filter #'thing) (here 0))
  "split  `s`, divided by `sep` filtered through `filter`"
  (let* ((there (position sep s :start here))
         (word  (funcall filter (subseq s here there))))
    (labels ((tail () (if there (split s sep filter (1+ there)))))
      (if (equal word "") (tail) (cons word (tail))))))

(with-file "../../../4src/data/auto93.csv"  #'print)
