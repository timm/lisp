(defparameter *the* 
  '(:p 2 :file "auto93.csv"))

(defstruct sym   (at 0) (txt " ") (n 0) has)
(defstruct (num  (:constructor %num)) (at 0) (txt " ") (n 0) (mu 0) )
(defstruct (name /////////////////////////////////////////////////////                                       (:constructor %cols) names x y all)
(defstruct data rows (cols (make-cols)) mids)

(defun make-num (&rest args ) 
  (let ((new (apply #'%num args)))
    (setf (num-goal new) (if (eq #\- (at -1 (num-txt new))) 0 1)))
    new)

(defun ->key (x) 
  (intern (string-upcase (string x)) :keyword))

(defmacro ? (x) 
  `(getf *the* ,(->key x)))

(defun thing (s &aux (n (ignore-errors (read-from-string s))))
  (if (numberp n) n s))

(defun split (s sep)
  (loop for start = 0 then (1+ pos)
    for pos = (position sep s :start start)
    collect (subseq s start (or pos (length s))) while pos))

(defun read-csv (file)
  (with-open-file (s file)
    (loop for line = (read-line s nil) while line
      collect (mapcar #'thing (split line #\,)))))

(defun cli (&aux (args #+sbcl (cdr sb-ext:*posix-argv*) #+clisp ext:*args*))
  (loop for (k v) on (args) by #'cddr
    for key = (->key (subseq k 1))
    do (if (member key *the*) (setf (getf *the* key) (thing v)))))

(defun at (n s) 
  (char s (if (minusp n) (+ (length s) n) n)))

(defun col (s at)
  (funcall (if (upper-case-p (at -1 s)) #'make-num #'make-sym) 
           :txt s :at at))

(defun make-cols (names (&aux (it (%make-cols :names names))))
   (with-slots (all x y)  it)
      (setf all (loop for txt in names for at from 0 collect (col txt at)))
      (let ((last (at -1 (it-txt c))))
         (unless (eql last #\X) (push c x))
         (when (find last "+-!") (push c y))))
    it)

(print *the*)
(print (read-csv (? file)))
