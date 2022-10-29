"Macros
 Simple alist access"

(defmacro ! (l x)
  "--> any;  get into association lists"
  `(cdr (assoc ',x ,l)))

(defmacro ? (s x &rest xs) 
  "--> any; recursive slot-value"
  (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))

(defmacro geta (x lst &optional (init 0)) 
  "--> x; ensure `lst` has `(x val)`; return `val`"
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))

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
