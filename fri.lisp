; vim: set ft=lisp ts=2 sw=2 et lispwords+=with-slots :
(defparameter *the* 
  '(:p 2 :file "auto93.csv"))

(defmacro ! (x) `(getf *the* ',(intern (string x) :keyword)))
(defmacro ? (x &rest at) (if at `(? (slot-value ,x ',(car at)) ,@(cdr at)) x))

(set-macro-character #\$ (lambda (s c) `(slot-value i ',(read s t nil t))))

(defstruct sym (at 0) (txt " ") (n 0) has)
(defstruct (num  (:constructor %make-num)) (at 0) (txt " ") (n 0) (mu 0) )
(defstruct (cols (:constructor %make-cols)) names x y all)
(defstruct (data (:constructor %make-data)) rows cols  _mid)

(defun make-num (&rest args &aux (i (apply #'%make-num args)))
  (setf $goal (if (eq #\- (ch $txt -1)) 0 1))
  i)

(defun make-cols (names &aux (i (%make-cols :names names)) (n -1))
  (labels ((one (s) (funcall (ako s) :txt txt :at (incf n)))
           (ako (s) (if (upper-case-p (ch txt 0)) #'make-num #'make-sym))
           (end (col) (ch (? col txt) -1)))
    (setf $all (mapcar #'one names))
    (dolist (col $all i)
      (unless (eql (end col) #\X)
        (if (find (end col) "!-+") (push col $y) (push col $x))))))

(defun make-data (&optional rows &aux (i (%make-data :cols (cols (car rows)))))
  (dolist (row (cdr rows) i) (add i row)))

(defun clone ((i data) &optional rows)
  (make-data (cons $names rows)))

;------------------------------------------------------------------------------
(defun add (x v (w 1))
  (unless (eql #\? x)
    (incf (slot-vale x 'n))
    (add1 x v w))
  x)

(defmethod add1 ((i data) row w)
  (setf $_mids nil)
  (push row $rows)
  (mapcar (lambda (col v) (add col v w)) all row))

(defun _sym+ (i v inc) 
  (incf (cdr (or (assoc v $has :test #'equal)
                 (car (setf $has (cons (cons v 0) $has))))) inc))

(defun _cols+ (i v inc) 
  (mapcar (=> (x col) (add col x inc)) row $all))

(defun _num+ (i v inc) 
  (setf $lo (min v $lo)
        $hi (max v $hi))
  (if (and (< inc 0) (< $n 2))
    (setf $mu 0 $m2 0 $sd 0 $n 0)
    (let* ((d (- v $mu)))
      (incf $mu (* inc (/ d $n)))
      (incf $m2 (* inc (* d (- v $mu))))
      (setf $sd (if (< $n 2) 0 (sqrt (/ (max 0 $m2) (1- $n))))))))

(defun _data+  (i v inc zap)
  (cond ((not $cols) (setf $cols (new+(make-cols) :names row)))
        ((> inc 0) (push row $rows)
                   (add $cols row inc))
        (t         (if zap (setf $rows (remove row $rows :test #'equal)))
                   (sub $cols v))))


(defmethod add1 ((n num) n)
  (let ((d 

(defun ->key (x) 
  (intern (string-upcase (string x)) :keyword))

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

(defun ch (s n) 
  (char s (if (minusp n) (+ (length s) n) n)))

(defun col! (s at)
  (if (upper-case-p (at -1 s)) 
    (num! :at at :txt s)
    (sym! :at at :txt s)))

  
(print *the*)
(let ((rows (read-csv (? file)))
