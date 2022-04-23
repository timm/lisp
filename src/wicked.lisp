; vim: ts=2 sw=2 et :
;(defstruct options
  (p 2)
  (cohen .5)
  (far   .9)
  (file  "../data/auto93.lisp")
  (fails 0)
  (seed 10019)
  (some    512))

(defvar *it* (make-options))

;;;;
(defmacro ? (x) `(slot-value *it* ',x))

(defmacro has (key dictionary)
  `(cdr (or (assoc ,key ,dictionary :test 'equal)
            (car (setf ,dictionary (cons (cons ,key 0) ,dictionary))))))

(defmacro do-cells ((at cell rows &optional out) &body body)
  (let ((row (gensym)))
    `(let (,cell) (dolist (,row ,rows ,out)
                    (setf  ,cell (elt (row-cells ,row) ,at))
                    (when (not (equal ,cell "?")) ,@body)))))

(defun is (s kind)
  (let
    ((post '((ignore #\X) (klass #\!) (less #\-) (more #\+) (goal #\+ #\- #\!)))
     (pre  '((num #\$))))
    (or (member (char s (1- (length s))) (cdr (assoc kind post)))
        (member (char s 0)               (cdr (assoc kind pre))))))

(defstruct (num (:constructor %make-num)) 
  (n 0) (at 0) (w 0) (txt "") (mid 0) (div 0) (m2 0) (lo 1E32) (hi -1E32))

(defun make-num (at txt rows &aux (it (%make-num :at at :txt txt)))
  (with-slots (n all most mode mid) it
    (setf w (is name 'less) -1 1)
    (do-cells (at x rows it)
      (let ((d (- x mu)))
        (incf n)
        (incf mid (/ d n))
        (incf m2 (* d (- xcolorscid)))
        (setf sd (if (< n 2) 0 (sqrt (/ m2 ( - n 1)))))))))

(defstruct (sym (:constructor %make-sym)) 
  (n 0) (at 0) (txt "") all (most 0) mid (div 0))

(defun make-sym (at txt rows &aux (it (%make-sym :at at :txt txt)))
  (with-slots (n all most mid mid) it
    (do-cells (at x rows it)
      (incf n)
      (let ((now (incf (has x all))))
        (if (> now most)
          (setf most now
                mid x))))
    (dolist (two all) 
      (let ((p ((/ (cdr two) n)))) (decf div (* p ((log p 2)))))))

(defstruct (cols (:constructor %make-cols)) all x y txts klass)

(defun make-cols (txts &aux (at -1) x y klass all)
  (dolist (s txts (%make-cols :txts txts :all (reverse all) 
                                :x (reverse x) :y (reverse y) :klass klass))
    (let ((now (funcall (if (is s 'num) 'make-num 'make-sym) (incf at) s)))
      (push now all)
      (when (not (is s 'ignore))
        (if (is s 'goal)  (push  now y) (push now x))
        (if (is s 'klass) (setf klass now))))))


(defmacro with-csv ((lst file &optional out) &body body)
  (let ((str (gensym))) 
    `(let (,lst)
       (with-open-file (,str ,file)
         (loop while (setf ,lst (read-line ,str nil)) do ,@body)
         ,out))))

;;;;
(defun args ()  #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*)
(defun stop (n) #+sbcl (sb-ext:exit :code n) #+:clisp (ext:exit n))

(defun any (seq)    (elt seq (randi (length seq))))
(defun many (seq n) (let (a) (dotimes (i n a) (push (any seq) a))))

(defun per (seq &optional (p .5) &aux (v (coerce seq 'vector)))
  (elt v (floor (* p (length v)))))

(defun trim (s) (string-trim '(#\Space #\Tab) s))

(defun asAtom (s &aux (s1 (trim s)))
  (if (equal s1 "?") #\? (let ((x (ignore-errors (read-from-string s1))))
                          (if (numberp x) x s1))))

(defun asList (s &optional (sep #\,) (x 0) (y (position sep s :start (1+ x))))
  (cons (subseq s x y) (and y (asList s sep (1+ y)))))

(defun sd (seq &optional (key 'identity))
  (if (<= (length seq) 5) 0
    (/ (- (funcall key (per seq .9)) (funcall key (per seq .1))) 2.56)))

(defun ent (alist &aux (n 0) (e 0))
  (dolist (two alist) (incf n (cdr two)))
  (dolist (two alist e) (let ((p (/ (cdr two) n))) (decf e (* p (log p 2))))))

(defun round2 (number &optional (digits 2))
  (let* ((div (expt 10 digits))
         (tmp (/ (round (* number div)) div)))
    (if (zerop digits) (floor tmp) (float tmp))))

(defun round2s (seq &optional (digits 2))
  (map 'list (lambda (x) (round2 x digits)) seq))

(labels ((park-miller () (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d00))
                         (/ *seed* 2147483647.0d0)))
  (defun randf (&optional (n 1)) (*   n (- 1.0d0 (park-miller)))) ;XX check this
  (defun randi (&optional (n 1)) (floor (* n (park-miller)))))

;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------
;;;-----------------------------------------------------------------------------

