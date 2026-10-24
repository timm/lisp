;;; vim: set ts=2 sw=2 sts=2 et lisp cc=75 lispwords+=loop,aif,defm :
#+sbcl (declaim (sb-ext:muffle-conditions warning))
#+sbcl (setf sb-ext:*invoke-debugger-hook* (lambda (c h)
  (format *error-output* "~&~a~%" c) (sb-ext:exit :code 1 :abort t)))

(defstruct opts
  (seed 1234567891) (p 2) (budget 50) (goods t) (file "auto93.csv"))

(defparameter *the* (make-opts))

(defparameter *opts* '(seed p budget goods file) "What cli may set.")

; -----------------------------------------------------------------------
(defmacro ? (x k &rest ks)
  (if ks `(? (slot-value ,x ',k) ,@ks) `(slot-value ,x ',k)))

(defmacro @ (k &rest ks) `(? *the* ,k ,@ks))

(set-macro-character #\$            ; $k is (? self k)
   (lambda (s c) (declare (ignore c))
      `(slot-value self ',(read s t nil t))) t)

(defmacro defm (name args &body body &aux doc) "defun + self."
  (if (stringp (car body)) (setf doc (list (pop body))))
  `(defun ,name ,args ,@doc
     (let ((self ,(if (consp (car args)) (caar args) (car args))))
       (declare (ignorable self))
       ,@body)))

(defmacro aif (test then &optional else)
  `(let ((it ,test)) (if it ,then ,else)))

; -----------------------------------------------------------------------
(defstruct num (n 0) (at 0) (txt "") (mu 0.0) (m2 0.0) (w 1))
(defstruct sym (n 0) (at 0) (txt "") (has (o)))
(defstruct tbl rows cols x y names)

(defun sym (&optional (txt "") (at 0)) (make-sym :at at :txt txt))
(defun num (&optional (txt "") (at 0))
  (make-num :at at :txt txt :w (if (eql #\- (at txt -1)) 0 1)))

(defun col (&optional (txt "") (at 0))
  (if (upper-case-p (at txt 0)) (num txt at) (sym txt at)))

; -----------------------------------------------------------------------
(defun at (s i &aux (n (length s))) (when (plusp n) (elt s (mod i n))))

(defun o (&rest kvs)
  (let ((h (make-hash-table :test #'equal))) 
    (loop for (k v) on kvs by #'cddr do (setf (gethash k h) v))
    h))

(defun thing (str &aux (*read-eval* nil)
                   (s (string-trim '(#\Space #\Tab #\Return) str))
                   (v (ignore-errors (read-from-string s nil :none))))
  "Coerce STR to a number, `?`, or the trimmed string itself."
  (if (or (numberp v) (eql v '?)) v s))

(defun cells (s &optional (lo 0)) "Split S on commas."
  (let ((hi (position #\, s :start lo)))
    (cons (thing (subseq s lo hi)) (if hi (cells s (1+ hi))))))

(defun csv (file) "FILE's rows, each a vector of cells."
  (with-open-file (s (truename file))
    (loop for line = (read-line s nil) while line
      collect (coerce (cells line) 'vector))))

; -----------------------------------------------------------------------
(defm add (col x &aux d) "Fold X in: counts, or Welford."
  (unless (eql x '?)
    (incf $n)
    (if (sym-p col)
      (incf (gethash x $has 0))
      (setf d   (- x $mu)
            $mu (+ $mu (/ d $n))
            $m2 (+ $m2 (* d (- x $mu)))))))

(defm div (col) "Spread: sd, or entropy."
  (if (sym-p col)
    (loop for v being the hash-values of $has
          sum (* (- (/ v $n)) (log (/ v $n) 2)))
    (if (< $n 2) 0.0 (sqrt (/ $m2 (1- $n))))))

(defm head (tbl names) "Name the cols; + - ! are goals, X skipped."
  (setf $names names
        $cols  (loop for txt in (coerce names 'list) for n from 0
                     collect (col txt n)))
  (dolist (c $cols)
    (case (at (? c txt) -1)
      (#\X)                              ; summarized, not modelled
      ((#\+ #\- #\!) (push c $y))
      (t                (push c $x)))))

(defm row (tbl r) "Add R; the first row names the columns."
  (if $cols
    (push (dolist (c $cols r) (add c (elt r (? c at)))) $rows)
    (head tbl r)))

(defun tbl (&optional (src (@ file)) &aux (self (make-tbl)))
  (dolist (r (if (stringp src) (csv src) src) self) (row self r)))

(defm clone (tbl &optional rows) "An empty copy of TBL, plus ROWS."
  (tbl (cons $names rows)))

; -----------------------------------------------------------------------
(defm norm (col x) "Logistic cdf of X."
  (if (or (eql x '?) (sym-p col)) x
    (let ((z (max -3.0 (min 3.0 (/ (- x $mu) (+ (div col) 1e-32))))))
      (/ 1 (+ 1 (exp (* -1.7 z)))))))

(defm bin (col x &optional (b 5)) "X into B bins, 1..B."
  (if (or (eql x '?) (sym-p col)) x
    (min b (1+ (floor (* b (norm col x)))))))

; -----------------------------------------------------------------------
(defparameter *seed* 1234567891 "State of the one random stream.")

(defun srand (&optional (n (@ seed))) "Restart that stream."
  (setf *seed* n))

(defun rint (&optional (n 1)) "A random int in 1..N."
  (setf *seed* (mod (* 16807 *seed*) 2147483647))
  (1+ (floor (* n (/ *seed* 2147483647.0)))))

(defun shuffle (lst &aux (v (coerce lst 'vector)) j) "LST, reordered."
  (loop for i from (1- (length v)) downto 1
        do (setf j (1- (rint (1+ i)))) (rotatef (aref v i) (aref v j)))
  (coerce v 'list))

(defm ydist (tbl r &aux (n 0) (d 0)) "R's goals to heaven; 0 is best."
  (dolist (c $y)
    (incf n)
    (incf d (expt (abs (- (norm c (elt r (? c at))) (? c w))) (@ p))))
  (expt (/ d n) (/ 1 (@ p))))

(defm split (tbl &optional (k (@ budget))) "Rows, shuffled, as train+test."
  (let* ((rows (shuffle $rows)) (n (min k (length rows))))
    (values (subseq rows 0 n) (subseq rows n))))

(defstruct arm bins w)                  ; one column: its bins, its weight
(defstruct model all arms)              ; the grand num, and arm per at

(defm train (tbl rows &optional (y (lambda (r) (ydist tbl r)))
            &aux (arms (o)) (all (num)))
  "rmeans: per x column, Y per bin, and that column's variance cut."
  (let ((i (clone tbl rows)))
    (dolist (r (? i rows)) (add all (funcall y r)))
    (dolist (c (? i x) (make-model :all all :arms arms))
      (let ((a (setf (gethash (? c at) arms) (make-arm :bins (o)))))
        (dolist (r (? i rows))
          (let ((b (bin c (elt r (? c at)))))
            (add (or (gethash b (? a bins))
                     (setf (gethash b (? a bins)) (num)))
                 (funcall y r))))
        (setf (? a w) (vcut a all))))))

(defm vcut (arm all &aux (s 0)) "Variance ARM's bins cut from ALL."
  (loop for v being the hash-values of $bins
        do (incf s (* (/ (? v n) (? all n)) (expt (div v) 2))))
  (max 0 (- (expt (div all) 2) s)))

(defun est (v all &optional (m 2)) "V's mean, pulled to ALL's by M."
  (/ (+ (* (? v n) (? v mu)) (* m (? all mu))) (+ (? v n) m)))

(defm bins (tbl m r) "The trained nums that R's x cells fall in."
  (loop for c in $x
        for a = (gethash (? c at) (? m arms))
        for v = (and a (gethash (bin c (elt r (? c at))) (? a bins)))
        when v collect v))

(defm guess (tbl m r &aux (s 0) (w 0)) "rmeans guess of R's ydist."
  (dolist (c $x)
    (let* ((a (gethash (? c at) (? m arms)))
           (v (and a (gethash (bin c (elt r (? c at))) (? a bins)))))
      (when (and v (plusp (? a w)))
        (incf s (* (? a w) (est v (? m all))))
        (incf w (? a w)))))
  (if (plusp w) (/ s w) (? (? m all) mu)))

; -----------------------------------------------------------------------
(defun cohen (xs ys &aux (a (num)) (b (num)) sd) "Gap, in pooled sds."
  (dolist (x xs) (add a x))
  (dolist (y ys) (add b y))
  (setf sd (sqrt (/ (+ (* (1- (? a n)) (expt (div a) 2))
                       (* (1- (? b n)) (expt (div b) 2)))
                    (- (+ (? a n) (? b n)) 2))))
  (/ (abs (- (? a mu) (? b mu))) (+ sd 1e-32)))

(defun cliffs (xs ys &aux (gt 0) (lt 0) (j 0) (k 0)) "Rank imbalance."
  (setf ys (coerce ys 'vector))               ; sorted lists in
  (dolist (x xs)
    (loop while (and (< j (length ys)) (< (aref ys j) x))
          do (incf j) (setf k j))
    (loop while (and (< k (length ys)) (= (aref ys k) x)) do (incf k))
    (incf gt j)
    (incf lt (- (length ys) k)))
  (/ (abs (- gt lt)) (* (length xs) (length ys))))

(defun ks (xs ys &aux (p 0) (q 0) (d 0) v) "Max cdf gap, in crits."
  (setf xs (coerce xs 'vector) ys (coerce ys 'vector))
  (loop while (and (< p (length xs)) (< q (length ys)))
        do (setf v (min (aref xs p) (aref ys q)))
           (loop while (and (< p (length xs)) (= (aref xs p) v))
                 do (incf p))
           (loop while (and (< q (length ys)) (= (aref ys q) v))
                 do (incf q))
           (setf d (max d (abs (- (/ p (length xs))
                                  (/ q (length ys)))))))
  (/ d (sqrt (/ (+ (length xs) (length ys))
                (* (length xs) (length ys))))))

(defun same (xs ys) "Do all three tests call XS and YS the same?"
  (and (<= (cohen  xs ys) 0.350)        ; sorted lists in; `and` is
       (<= (cliffs xs ys) 0.195)        ; lazy, so cheap tests first
       (<= (ks     xs ys) 1.360)))

; -----------------------------------------------------------------------
(defun halt (&optional (n 0)) "Leave, with N as the exit status."
  #+sbcl (sb-ext:exit :code n :abort t) #+clisp (ext:quit n))

(defun cli-args ()
  #+clisp ext:*args* #+sbcl (cdr sb-ext:*posix-argv*))

(defun run (f &aux (b4 (copy-opts *the*)) (bad 0)) "Run F; restore *the*."
  (srand)
  (handler-case (funcall f)
    (error (c) (format t "~&FAIL ~a: ~a~%" f c) (setf bad 1)))
  (setf *the* b4)
  bad)

(defun cli (&optional (av (cli-args)) (bad 0)) "-x runs eg-x, -k v sets k."
  (loop for s = (pop av) while s
        for k = (string-upcase (string-left-trim "-" s)) do
    (aif (find-symbol (format nil "EG-~a" k))
      (incf bad (run it))
      (aif (find (intern k) *opts*)
        (let ((b4 (slot-value *the* it)))
          (setf (slot-value *the* it)
                (if (member b4 '(t nil)) (not b4) (thing (pop av)))))
        (format t "?? ~a~%" s))))
  bad)
