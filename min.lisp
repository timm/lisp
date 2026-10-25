;;; vim: set ts=2 sw=2 sts=2 et lisp cc=75 lispwords+=loop,aif,defm :
#+sbcl (declaim (sb-ext:muffle-conditions warning))
#+sbcl (setf sb-ext:*invoke-debugger-hook* (lambda (c h)
  (format *error-output* "~&~a~%" c) (sb-ext:exit :code 1 :abort t)))

;;;; config --------------------------------------------------------------
(defstruct opts
  (seed 1234567891) (p 2) (budget 50) (goods t) (file "auto93.csv"))

(defparameter *the* (make-opts))

;;;; macros --------------------------------------------------------------
(defmacro ? (x k &rest ks)
  (if ks `(? (slot-value ,x ',k) ,@ks) `(slot-value ,x ',k)))

(defmacro @ (k &rest ks) `(? *the* ,k ,@ks))

(defmacro want (place) `(or ,place (setf ,place (num))))

(defmacro aset (place new) `(let ((it ,place)) (setf ,place ,new)))

(defmacro aif (test ok &optional no) `(let ((it ,test)) (if it ,ok ,no)))

(set-macro-character #\$ (lambda (s c) (declare (ignore c))
                           `(slot-value self ',(read s t nil t))) t)

(defmacro defm (name args &body body &aux doc)
  (if (stringp (car body)) (setf doc (list (pop body))))
  `(defun ,name ,args ,@doc
     (let ((self ,(if (consp (car args)) (caar args) (car args))))
       (declare (ignorable self))
       ,@body)))

;;;; main structs --------------------------------------------------------
(defstruct num (n 0) (at 0) (txt "") (mu 0.0) (m2 0.0) (w 1))
(defstruct sym (n 0) (at 0) (txt "") (has (o)))
(defstruct tbl rows cols x y names)

;;;; constructorr --------------------------------------------------------
(defun sym (&optional (txt "") (at 0)) (make-sym :at at :txt txt))
(defun num (&optional (txt "") (at 0))
  (make-num :at at :txt txt :w (if (eql #\- (at txt -1)) 0 1)))

(defun col (&optional (txt "") (at 0))
  (if (upper-case-p (at txt 0)) (num txt at) (sym txt at)))

(defun tbl (&optional (src (@ file)) &aux (self (make-tbl)))
  (dolist (r (if (stringp src) (csv src) src) self) (row self r)))

(defm add (col x &aux d) 
  (unless (eql x '?)
    (incf $n)
    (if (sym-p col)
      (incf (gethash x $has 0))
      (setf d   (- x $mu)
            $mu (+ $mu (/ d $n))
            $m2 (+ $m2 (* d (- x $mu)))))))

(defm head (tbl names &aux (cols (loop for txt in (coerce names 'list)
                                       for n from 0 collect (col txt n))))
  (setf $names names)
  (dolist (c cols cols)
    (case (at (? c txt) -1)
      (#\X)                              ; summarized, not modelled
      ((#\+ #\- #\!) (push c $y))
      (t             (push c $x)))))

(defm row (tbl r) 
  (if $cols
    (push (dolist (c $cols r) (add c (elt r (? c at)))) $rows)
    (setf $cols (head tbl r))))

(defm clone (tbl &optional rows) (tbl (cons $names rows)))

;;;; query --------------------------------------------------------------
(defm div (col) 
  (if (sym-p col)
    (loop for v being the hash-values of $has
          sum (* (- (/ v $n)) (log (/ v $n) 2)))
    (if (< $n 2) 0.0 (sqrt (/ $m2 (1- $n))))))

(defm norm (col x) 
  (if (or (eql x '?) (sym-p col)) x
    (let ((z (max -3.0 (min 3.0 (/ (- x $mu) (+ (div col) 1e-32))))))
      (/ 1 (+ 1 (exp (* -1.7 z)))))))

(defm bin (col x &optional (b 5)) 
  (if (or (eql x '?) (sym-p col)) x
    (min b (1+ (floor (* b (norm col x)))))))

(defm ydist (tbl r &aux (n 0) (d 0)) 
  (dolist (c $y)
    (incf n)
    (incf d (expt (abs (- (norm c (elt r (? c at))) (? c w))) (@ p))))
  (expt (/ d n) (/ 1 (@ p))))

(defm split (tbl &optional (k (@ budget))) 
  (let* ((rows (shuffle $rows)) (n (min k (length rows))))
    (values (subseq rows 0 n) (subseq rows n))))

;;;; inference: rmeans -------------------------------------------------
(defstruct arm bins w)                 ; one column: its bins, its weight
(defstruct model all arms cols)        ; grand num, arms, x cols

(defm arm! (col rows y all &aux (a (make-arm :bins (o))))
  (dolist (r rows)
    (add (want (gethash (bin col (elt r $at)) (? a bins)))
         (funcall y r)))
  (setf (? a w) (vcut a all))
  a)

(defm train (tbl rows &optional (y (lambda (r) (ydist tbl r)))
            &aux (arms (o)))
  (let* ((i   (clone tbl rows))
         (all (nums (mapcar y (? i rows)))))
    (dolist (c (? i x) (make-model :all all :arms arms
                                   :cols (? i x)))
      (setf (gethash (? c at) arms) (arm! c (? i rows) y all)))))

(defm vcut (arm all) 
  (max 0 (- (expt (div all) 2)
            (loop for v being the hash-values of $bins
                  sum (* (/ (? v n) (? all n)) (expt (div v) 2))))))

(defm est (num all &optional (m 2)) 
  (/ (+ (* $n $mu) (* m (? all mu))) (+ $n m)))

(defun hit (m col r &aux (a (gethash (? col at) (? m arms))))
  (if a (values (gethash (bin col (elt r (? col at))) (? a bins)) a)))

(defm bins (m r) 
  (loop for c in $cols for v = (hit m c r) when v collect v))

(defm guess (m r &aux (s 0) (w 0)) 
  (dolist (c $cols)
    (multiple-value-bind (v a) (hit m c r)
      (when (and v (plusp (? a w)))
        (incf s (* (? a w) (est v $all)))
        (incf w (? a w)))))
  (if (plusp w) (/ s w) (? $all mu)))

;;;; stats -------------------------------------------------------------
(defun nums (lst &aux (col (num))) (dolist (x lst col) (add col x)))

(defun cohen (xs ys &aux (a (nums xs)) (b (nums ys))
                         (n (? a n)) (m (? b n)))
  (/ (abs (- (? a mu) (? b mu)))
     (+ 1e-32 (sqrt (/ (+ (* (1- n) (expt (div a) 2))
                          (* (1- m) (expt (div b) 2)))
                       (- (+ n m) 2))))))

(defun cliffs (xs ys &aux (b (coerce ys 'vector)) (m (length b))
                          (gt 0) (lt 0) (j 0) (k 0))
  (dolist (x xs (/ (abs (- gt lt)) (* (length xs) m)))
    (loop while (and (< j m) (< (aref b j) x)) do (setf k (incf j)))
    (loop while (and (< k m) (= (aref b k) x)) do (incf k))
    (incf gt j)
    (incf lt (- m k))))

(defun ks (xs ys &aux (a (coerce xs 'vector)) (b (coerce ys 'vector))
                      (n (length a)) (m (length b))
                      (p 0) (q 0) (d 0) v)
  (loop while (and (< p n) (< q m)) do
    (setf v (min (aref a p) (aref b q)))
    (loop while (and (< p n) (= (aref a p) v)) do (incf p))
    (loop while (and (< q m) (= (aref b q) v)) do (incf q))
    (setf d (max d (abs (- (/ p n) (/ q m))))))
  (/ d (sqrt (/ (+ n m) (* n m)))))

(defun same (xs ys &aux (a (sort (copy-list xs) #'<))
                        (b (sort (copy-list ys) #'<)))
  (and (<= (cohen  a b) 0.350)          ; `and` is lazy, so the
       (<= (cliffs a b) 0.195)          ; cheap tests run first
       (<= (ks     a b) 1.360)))

;;;; misc -----------------------------------------------------------
(defun at (s i &aux (n (length s))) (when (plusp n) (elt s (mod i n))))

(defun o (&rest kvs)
  (let ((h (make-hash-table :test #'equal))) 
    (loop for (k v) on kvs by #'cddr do (setf (gethash k h) v))
    h))

(defun thing (str &aux (*read-eval* nil)
                   (s (string-trim '(#\Space #\Tab #\Return) str))
                   (v (ignore-errors (read-from-string s nil :none))))
  (if (or (numberp v) (eql v '?)) v s))

(defun cells (s &optional (lo 0))
  (let ((hi (position #\, s :start lo)))
    (cons (thing (subseq s lo hi)) (if hi (cells s (1+ hi))))))

(defun csv (file) 
  (with-open-file (s (truename file))
    (loop for line = (read-line s nil) while line
      collect (coerce (cells line) 'vector))))

;;;; rand --------------------------------------------------------------
(defparameter *seed* 1234567891 "State of the one random stream.")

(defun srand (&optional (n (@ seed))) (setf *seed* n))

(defun rint (&optional (n 1)) 
  (setf *seed* (mod (* 16807 *seed*) 2147483647))
  (1+ (floor (* n (/ *seed* 2147483647.0)))))

(defun shuffle (lst &aux (v (coerce lst 'vector)) j) 
  (loop for i from (1- (length v)) downto 1
        do (setf j (1- (rint (1+ i)))) (rotatef (aref v i) (aref v j)))
  (coerce v 'list))

;;;; system --------------------------------------------------------------
(defun cli-args () #+clisp ext:*args* #+sbcl (cdr sb-ext:*posix-argv*))

(defun run (f &aux (b4 (copy-opts *the*)) (bad 0)) 
  (srand)
  (handler-case (funcall f)
    (error (c) (format t "~&FAIL ~a: ~a~%" f c) (setf bad 1)))
  (setf *the* b4)
  bad)

(defun cli (&optional (av (cli-args)) (bad 0))
  "-x runs eg-x, -k v sets k; leaves with the failure count."
  (loop for s = (pop av) while s
        for k = (string-upcase (string-left-trim "-" s))
        for sym = (find-symbol k) do
    (aif (find-symbol (format nil "EG-~a" k))
      (incf bad (run it))
      (if (and sym (slot-exists-p *the* sym))
        (aset (slot-value *the* sym)
              (if (member it '(t nil)) (not it) (thing (pop av))))
        (format t "?? ~a~%" s))))
  (halt bad))

(defun halt (&optional (n 0)) 
  #+sbcl (sb-ext:exit :code n :abort t) #+clisp (ext:quit n))
