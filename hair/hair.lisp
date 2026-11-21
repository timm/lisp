; vim: set ts=2 sw=2 sts=2 et lisp cc=75 lispwords+=loop,defm,aif,cond,labels :
(load "sys") ; system tricks. start package. tell sbcl debugger to chill
(load "min") ; all my general lisp tricks
(in-package :hair)  ; `load` restores *package*, so say it again here
;;; hair: tiny AI, explained. Sections follow the python hair/ files:
;;; about, data, dist, stats, tree, then the egs (tests).
;;; (c) 2026, Tim Menzies <timm@ieee.org>, MIT license

;;;; about -------------------------------------------------------------
(defstruct about
  (file "~/gits/moot/optimize/misc/auto93.csv")
  (seed 1234567891) (pow 2) (leaf 3) (maxd 8) (eps 0.35))

(defvar *k* (make-about))

;;;; data --------------------------------------------------------------
;;; Header: Upper case = Num, else Sym. Last char: + - goal (a y),
;;; ! class, X skip, else an x. "?" cells are unknown.
(defstruct num (at 0) (txt "") (n 0) (mu 0) (m2 0) (goal 1))
(defstruct sym (at 0) (txt "") (n 0) (has (o)))
(defstruct tbl (n 0) names cols x y klass rows)

(defun col (at txt)
  (if (upper-case-p (charn txt 0))
    (make-num :at at :txt txt :goal (if (eql #\- (charn txt)) 0 1))
    (make-sym :at at :txt txt)))

(defm add (t v &optional (inc 1)) "Grow (inc=-1: shrink) a num,sym,tbl"
  (unless (eql v '?) (incf $n inc) (add1 self v inc))
  v)

(defm add1 (tbl r inc)
  (if (plusp inc) (push r $rows) (setf $rows (remove r $rows)))
  (dolist (c $cols) (add c (aref r (? c at)) inc)))

(defm add1 (sym v inc) (incf (gethash v $has 0) inc))

(defm add1 (num v inc)
  (if (<= $n 0)
    (setf $n 0 $mu 0 $m2 0)
    (let ((d (- v $mu)))
      (incf $mu (/ (* inc d) $n))
      (setf $m2 (max 0 (+ $m2 (* inc d (- v $mu))))))))

(defun adds (vs &optional (col (make-num))) 
  (dolist (v vs col) (add col v)))

(defm mid (num) "Center: the mean" $mu)

(defm mid (sym &aux (most (argmost))) "Center: the mode"
  (maphash most $has) 
  (funcall most))

(defm div (num) "Spread: standard deviation"
  (if (< $n 2) 0 (sqrt (/ $m2 (1- $n)))))

(defm div (sym) "Spread: entropy"
  (loop :for v :being :the :hash-values :of $has
    :when (plusp v) :sum (* -1 (/ v $n) (log (/ v $n) 2))))

(defm norm (num v) "A Num's value to 0..1, by a logistic z-score"
  (if (eql v '?) v
    (let ((z (/ (- v $mu) (+ (div self) +tiny+))))
      (/ 1 (+ 1 (exp (* -1.7 (max -3 (min 3 z)))))))))

(defun tbl (rows &aux (self (make-tbl))) "First row names the cols"
  (setf $names (pop rows)
        $cols  (loop :for txt :across $names :for at :from 0
                     :collect (col at txt)))
  (dolist (c $cols)
    (case (charn (? c txt))
      (#\X)
      (#\! (setf $klass c))
      ((#\+ #\-) (push c $y))
      (t (push c $x))))
  (dolist (r rows self) (add self r)))

(defm clone (tbl &optional rows) (tbl (cons $names rows)))

;;;; dist --------------------------------------------------------------
(defun minkowski (vs &aux (n 0) (d 0) (p (? *k* pow)))
  (dolist (v vs (expt (/ d (max n 1)) (/ 1 p)))
    (incf n) (incf d (expt v p))))

(defun gap (col u v) "0..1 distance between two values of one col"
  (cond ((and (eql u '?) (eql v '?)) 1)
        ((sym-p col) (if (equal u v) 0 1))
        (t (let ((u (norm col u)) (v (norm col v)))
             (if (eql u '?) (setf u (if (< v .5) 1 0)))
             (if (eql v '?) (setf v (if (< u .5) 1 0)))
             (abs (- u v))))))

(defm distx (tbl r q) "r to query q, over q's known x values"
  (minkowski (loop :for c :in $x :for v := (aref q (? c at))
               :unless (eql v '?) :collect (gap c (aref r (? c at)) v))))

(defm disty (tbl r) "Distance to heaven (0 = best)"
  (minkowski (loop :for c :in $y :for v := (aref r (? c at))
               :unless (eql v '?)
               :collect (abs (- (norm c v) (? c goal))))))

(defm near (tbl q &optional rows) "rows (default: all), nearest q first"
  (sort (copy-list (or rows $rows)) #'< :key (lambda (r) (distx self r q))))

;;;; stats -------------------------------------------------------------
(defun median (xs) (nth (floor (length xs) 2) (sort (copy-list xs) #'<)))

(defun cohen (xs ys &aux (a (adds xs)) (b (adds ys)))
  (let ((n (? a n)) (m (? b n)))
    (/ (abs (- (? a mu) (? b mu)))
       (+ +tiny+ (sqrt (/ (+ (* (1- n) (expt (div a) 2))
                             (* (1- m) (expt (div b) 2)))
                          (max 1 (- (+ n m) 2))))))))

(defun cliffs (xs ys &aux (b (coerce ys 'vector)) (m (length b)))
  "Cliff's delta; ys sorted. Per x: #ys below it - #ys above it"
  (/ (abs (loop :for x :in xs
                :sum (- (bisect b x) (- m (bisect b x #'<=)))))
     (* (length xs) m)))

(defun ks (xs ys &aux (a (coerce xs 'vector)) (b (coerce ys 'vector))
                      (n (length a)) (m (length b)))
  "Max CDF gap, xs ys sorted, in units of the 0.05 critical value"
  (/ (loop :for v :in (append xs ys)
           :maximize (abs (- (/ (bisect a v #'<=) n)
                             (/ (bisect b v #'<=) m))))
     (sqrt (/ (+ n m) (* n m)))))

(defun same (xs ys &aux (xs (sort (copy-list xs) #'<))
                        (ys (sort (copy-list ys) #'<)))
  "Indistinguishable? all three tests must agree"
  (and (<= (cohen xs ys) (? *k* eps))
       (<= (cliffs xs ys) 0.195)
       (<= (ks xs ys) 1.36)))

(defun rank (d) "alist (name . nums) -> alist (name . rank), 1 = best"
  (loop :with r := 1 :and pool
    :for (k . xs) 
    :in (sort (copy-list d) #'< :key (lambda (x) (median (cdr x))))
    :do (when (and pool (not (same pool xs))) (setf r (1+ r) pool nil))
    (setf pool (append pool xs))
    :collect (cons k r)))

(defun xval (rows fn &key (m 5) (n 5)) "m*n cross-val: (fn train test)"
  (dotimes (_ m)
    (let ((rows (shuffle rows)))
      (dotimes (k n)
        (loop :for r :in rows :for i :from 0
          :if (= k (mod i n)) :collect r :into test
          :else :collect r :into train
          :finally (funcall fn train test))))))

(defun rae (pairs &aux (mu (/ (loop :for (w) :in pairs :sum w)
                               (max 1 (length pairs)))))
  "Relative absolute error over (want . got): 100 = guessing the mean"
  (* 100 (/ (loop :for (w . g) :in pairs :sum (abs (- w g)))
            (+ +tiny+ (loop :for (w) :in pairs :sum (abs (- w mu)))))))

;;;; tree --------------------------------------------------------------
(defstruct node at v mid n rows yes no)

(defun xpect (a b &aux (n (? a n)) (m (? b n)))
  "Size-weighted spread of two summaries (lower = better)"
  (/ (+ (* n (div a)) (* m (div b))) (+ n m +tiny+)))

(defun has (col w v) "Is cell w on the yes-side of v? (? = yes)"
  (or (eql w '?) (if (sym-p col) (equal w v) (<= w v))))

(defun xys (c rows y) "(x . y) for rows whose x in col c is known"
  (loop :for r :in rows :for x := (aref r (? c at))
        :unless (eql x '?) :collect (cons x (funcall y r))))

(defm cuts (sym rows y make fn &aux (xy (xys self rows y)))
  "Offer (fn v yes no) per value v: v vs the rest"
  (dolist (k (remove-duplicates (mapcar #'car xy) :test #'equal))
    (let ((yes (funcall make)) (no (funcall make)))
      (loop :for (x . v) :in xy :do (add (if (equal x k) yes no) v))
      (funcall fn k yes no))))

(defm cuts (num rows y make fn
            &aux (xy (sort (xys self rows y) #'< :key #'car)))
  "Offer (fn x yes no) per split x <= v; sweep yes up, no down"
  (let ((yes (funcall make)) 
        (no  (adds (mapcar #'cdr xy) (funcall make))))
    (loop :for ((x . v) nxt) :on xy
          :do (add yes v) (add no v -1)
          :when (and nxt (/= x (car nxt))) 
          :do (funcall fn x yes no))))

(defm best-cut (tbl rows y make &aux (best (argmost #'<)))
  "Lowest-cost (col v) whose sides both have >= leaf rows, or nil"
  (dolist (c $x (funcall best))
    (cuts c rows y make (lambda (v yes no)
                          (if (<= (? *k* leaf) (min (? yes n) (? no n)))
                            (funcall best (list c v) (xpect yes no)))))))

(defm tree (tbl rows y &optional (make #'make-num) (lvl 0))
  "Split rows on the min-cost cut, recursively"
  (let* ((all (adds (mapcar y rows) (funcall make)))
         (tr  (make-node :mid (mid all) :n (length rows) :rows rows))
         (cut (and (< lvl (? *k* maxd)) (best-cut self rows y make))))
    (when cut              ; best-cut is nil unless both sides get >= leaf
      (setf (? tr at) (first cut) (? tr v) (second cut))
      (labels ((kid (rs) (tree self rs y make (1+ lvl))))
        (loop :for r :in rows           ; one pass: split, then recurse
              :if (yes? tr r) :collect r :into yes
              :else           :collect r :into no
              :finally (setf (? tr yes) (kid yes) (? tr no) (kid no)))))
    tr))

(defun yes? (tr r) "Does row r go down tr's yes branch? (? = yes)"
  (has (? tr at) (aref r (? tr at at)) (? tr v)))

(defun leaf (tr r) "Walk row r down to its leaf's mid"
  (loop :while (? tr at)
        :do (setf tr (if (yes? tr r) (? tr yes) (? tr no))))
  (? tr mid))

(defun show (tr &optional (pad "") (txt "all"))
  "One line per node: mid, n, then the test that leads here"
  (format t "~6,2f ~5d  ~a~a~%" (? tr mid) (? tr n) pad txt)
  (aif (? tr at)
    (labels ((kid (sub op)
               (show sub (format nil "~a|  " pad)
                     (format nil "~a ~a ~a" (? it txt) op (? tr v)))))
      (kid (? tr yes) (if (sym-p it) "==" "<="))
      (kid (? tr no)  (if (sym-p it) "!=" ">")))))

;;;; egs ---------------------------------------------------------------
(defun eg-config () (format t "~a~%" *k*))

(defun eg-tbl (&aux (tb (tbl (csv (? *k* file)))))
  (format t "~a rows, ~a x, ~a y~%"
          (length (? tb rows)) (length (? tb x)) (length (? tb y)))
  (dolist (c (? tb cols)) (format t "~10a ~,2f~%" (? c txt) (mid c))))

(defun eg-dist (&aux (tb (tbl (csv (? *k* file)))))
  (let ((rows (sort (copy-list (? tb rows)) #'< :key (lambda (r) (disty tb r)))))
    (dolist (r (subseq rows 0 3)) (format t "~,2f ~a~%" (disty tb r) r))
    (format t "nearest to first: ~a~%" (second (near tb (car rows))))))

(defun eg-same ()
  (let ((xs (loop :repeat 50 :collect (normal 10 1))))
    (dolist (d '(0 .1 .2 .3 .4 .5 1 2))
      (format t "~a ~a~%" d (same xs (mapcar (lambda (x) (+ x d)) xs))))))

(defun eg-rank ()
  (labels ((g (mu) (loop :repeat 30 :collect (normal mu 1))))
    (print (rank `((a . ,(g 10)) (b . ,(g 10.1)) (c . ,(g 14)) (d . ,(g 14.2)))))))

(defun eg-tree (&aux (tb (tbl (csv (? *k* file)))))
  (show (tree tb (? tb rows) (lambda (r) (disty tb r)))))

(defun eg-all ()
  (dolist (f '(eg-config eg-tbl eg-dist eg-same eg-rank eg-tree))
    (format t "~&~%-- ~(~a~)~%" f) (srand (? *k* seed)) (funcall f)))

;;;; main --------------------------------------------------------------
;;; sbcl --script hair.lisp --leaf=20 tree   (--key=val, then eg-NAMEs)
(defun cli (a &aux (i (position #\= a)))
  "--key=val sets that slot of *k*; NAME runs eg-NAME"
  (if (and i (eql 0 (search "--" a)))
    (let ((k (find-symbol (string-upcase (subseq a 2 i)) :hair)))
      (if (slot-exists-p *k* k)            ; k=nil is fine: no such slot
        (setf (slot-value *k* k) (thing (subseq a (1+ i))))
        (format t "?? ~a~%" a)))
    (aif (find-symbol (string-upcase (format nil "eg-~a" a)) :hair)
      (progn (srand (? *k* seed)) (funcall it))
      (format t "?? ~a~%" a))))

(mapc #'cli (args))
