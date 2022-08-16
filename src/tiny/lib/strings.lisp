(defun charn (x) 
  "Last thing from a string."
  (and (stringp x)
       (> (length x) 0)
       (char x (1- (length x)))))

(defun trim (x) 
  "Kill leading tailing whitespace."
  (string-trim '(#\Space #\Tab #\Newline) x))

(defun thing (x &aux (y (trim x)))
  "Turn `x` into a number or string or `?`."
  (cond ((string= y "?") #\?)
        ((string= y "t") t)
        ((string= y "nil") nil)
        (t (let ((z (read-from-string y nil nil)))
             (if (numberp z) z y)))))

(defun splits (str &key (char #\,) (filter #'identity))
  "Divide `str` on `char`, filtering all items through `filter`."
  (loop for start = 0 then (1+ finish)
    for        finish = (position char str :start start)
    collecting (funcall filter (trim (subseq str start finish)))
    until      (null finish)))

; String to lines or cells of things
(defun lines (string) (splits string :char   #\Newline))
(defun cells (string &key (char #\,)) (splits string :char char :filter #'thing))

(defun with-lines (file fun)
  "Call `fun` for each line in `file`."
  (with-open-file (s file)
    (loop (funcall fun (or (read-line s nil) (return))))))
