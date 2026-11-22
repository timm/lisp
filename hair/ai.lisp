; vim: set ts=2 sw=2 sts=2 et lisp cc=75 lispwords+=loop,defm,aif,cond,labels :
;;; ai: tiny AI, explained. Sections follow the python hair/ files:
;;; about, data, dist, stats, tree, then the egs (tests). Plain lisp;
;;; the system tricks live in hair.lisp (run that, not this).
;;; (c) 2026, Tim Menzies <timm@ieee.org>, MIT license
(in-package :hair)
(in-hair)

;;;; about -------------------------------------------------------------
(defstruct about
  (file "~/gits/moot/optimize/misc/auto93.csv")
  (seed 1234567891) (pow 2) (leaf 3) (maxd 8) (eps 0.35))

(defvar *k* (make-about))

;;;; data --------------------------------------------------------------
;;; Header: Upper case = Num, else Sym. Last char: + - goal (a y),
;;; ! class, X skip, else an x. "?" cells are unknown.
(defstruct num (at 0) (txt "") (n 0) (mu 0) (m2 0) (goal 1))
(defstruct sym (at 0) (txt "") (n 0) has)   ; has = alist value . count
(defstruct cols names all x y klass)
(defstruct tbl (n 0) cols rows)

(defun col (at txt)
  (if (upper-case-p (charn txt 0))
    (make-num :at at :txt txt :goal (if (eql #\- (charn txt)) 0 1))
    (make-sym :at at :txt txt)))

(defm add (t v &optional (inc 1)) "Grow (inc=-1: shrink) a num,sym,tbl"
  (unless (eql v '?) (incf $n inc) (add1 self v inc))
  v)

(defm add1 (tbl r inc)
  (if (plusp inc) (push r $rows) (setf $rows (remove r $rows)))
  (dolist (c (? $cols all)) (add c (aref r (? c at)) inc)))

(defm add1 (sym v inc) (incf (has v $has) inc))

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
  (loop :for (k . n) :in $has :do (funcall most k n))
  (funcall most))

(defm div (num) "Spread: standard deviation"
  (if (< $n 2) 0 (sqrt (/ $m2 (1- $n)))))

(defun ent (cs n) "Entropy of counts cs, summing to n"
  (loop :for c :in cs :when (plusp c)
        :sum (let ((p (/ c (float n)))) (- (* p (log p 2))))))

(defm div (sym) "Spread: entropy"
  (ent (mapcar #'cdr $has) $n))

(defm norm (num v) "A Num's value to 0..1, by a logistic z-score"
  (if (eql v '?) v
    (let ((z (/ (- v $mu) (+ (div self) +tiny+))))
      (/ 1 (+ 1 (exp (* -1.7 (max -3 (min 3 z)))))))))

(defun skip? (txt) "Ignore this col? Name ends in X, X+, X- or X!"
  (or (eql #\X (charn txt))
      (and (find (charn txt) "+-!") (eql #\X (charn txt -2)))))

(defun cols (names &aux (self (make-cols :names names)))
  "Columns from the header row; last char of name says x, y, ! or X"
  (setf $all (loop :for txt :across names :for at :from 0
                   :collect (col at txt)))
  (dolist (c $all self)
    (let ((e (charn (? c txt))))
      (cond ((skip? (? c txt)))
            ((eql e #\!) (setf $klass c))
            ((find e "+-") (push c $y))
            (t (push c $x))))))

(defun tbl (rows) "First row names the cols"
  (adds (cdr rows) (make-tbl :cols (cols (car rows)))))

(defm clone (tbl &optional rows) (tbl (cons (? $cols names) rows)))

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
  (minkowski (loop :for c :in (? $cols x) :for v := (aref q (? c at))
               :unless (eql v '?) :collect (gap c (aref r (? c at)) v))))

(defm disty (tbl r) "Distance to heaven (0 = best)"
  (minkowski (loop :for c :in (? $cols y) :for v := (aref r (? c at))
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
(defstruct node at v ys n rows yes no)

(defun xpect (a b &aux (n (? a n)) (m (? b n)))
  "Size-weighted spread of two summaries (lower = better)"
  (/ (+ (* n (div a)) (* m (div b))) (+ n m +tiny+)))

(defun xys (c rows y) "(x . y) for rows whose x in col c is known"
  (loop :for r :in rows :for x := (aref r (? c at))
        :unless (eql x '?) :collect (cons x (funcall y r))))

(defm cuts (sym rows y make fn &aux (xy (xys self rows y)))
  "Offer (fn self v yes no) per value v: v vs the rest"
  (dolist (k (remove-duplicates (mapcar #'car xy) :test #'equal))
    (let ((yes (funcall make)) (no (funcall make)))
      (loop :for (x . v) :in xy :do (add (if (equal x k) yes no) v))
      (funcall fn self k yes no))))

(defm cuts (num rows y make fn
            &aux (xy (sort (xys self rows y) #'< :key #'car)))
  "Offer (fn self x yes no) per split x <= v; sweep yes up, no down"
  (let ((yes (funcall make)) 
        (no  (adds (mapcar #'cdr xy) (funcall make))))
    (loop :for ((x . v) nxt) :on xy
          :do (add yes v) (add no v -1)
          :when (and nxt (/= x (car nxt))) 
          :do (funcall fn self x yes no))))

(defun best-cut (cols rows y make
                 &key (leaf (? *k* leaf)) (ok (constantly t))
                 &aux (best (argmost #'<)))
  "Lowest-cost (col v) over cols, both sides >= leaf rows and
   (ok yes no); and its cost"
  (labels ((fun (c v yes no)            ; cuts passes its col as c
                (if (and (<= leaf (min (? yes n) (? no n)))
                         (funcall ok yes no))
                  (funcall best (list c v) (xpect yes no)))))
    (dolist (c cols (funcall best))
      (cuts c rows y make #'fun))))

(defun rdiv (rows y cut &optional (make #'make-num) (lvl 0))
  "Shared recursion for tree and bins: a node per level. (cut rows)
   => nil (a leaf) or (col v): rows where (yes? col v r) go left."
  (let ((it (and (< lvl (? *k* maxd)) (funcall cut rows))))
    (labels ((kid (rows) (rdiv rows y cut make (1+ lvl))))
      (loop :for r :in rows              ; one pass: split, then recurse
            :if (and it (yes? (first it) (second it) r))
              :collect r :into yes
            :else :collect r :into no
            :finally (return
                       (make-node
                        :ys (adds (mapcar y rows) (funcall make))
                        :n (length rows) :rows rows
                        :at (first it) :v (second it)
                        :yes (and it (kid yes)) :no (and it (kid no))))))))

(defm tree (tbl rows y &optional (make #'make-num) (xs (? $cols x)))
  "Split rows on the min-cost cut over cols xs, recursively"
  (labels ((cut (rows) (best-cut xs rows y make)))
    (rdiv rows y #'cut make)))

(defun yes? (c v r &aux (w (aref r (? c at))))
  "Is row r on the yes side of col c's cut v? (? = yes)"
  (or (eql w '?) (if (sym-p c) (equal w v) (<= w v))))

(defun leaf (tr r) "Walk row r down to its leaf"
  (loop :while (? tr at)
        :do (setf tr (if (yes? (? tr at) (? tr v) r)
                       (? tr yes)
                       (? tr no))))
  tr)

(defun show (tr &optional (pad "") (txt "all"))
  "One line per node: mid, n, then the test that leads here"
  (format t "~6,2f ~5d  ~a~a~%" (mid (? tr ys)) (? tr n) pad txt)
  (aif (? tr at)
    (labels ((kid (sub op)
               (show sub (format nil "~a|  " pad)
                     (format nil "~a ~a ~a" (? it txt) op (? tr v)))))
      (kid (? tr yes) (if (sym-p it) "==" "<="))
      (kid (? tr no)  (if (sym-p it) "!=" ">")))))

;;;; bins --------------------------------------------------------------
(defm bins (num rows y &optional (make #'make-num)
            &aux (at (? self at))
                 (rs (remove '? rows :key (lambda (r) (aref r at))))
                 (all (adds (mapcar y rs) (funcall make)))
                 (leaf (max (? *k* leaf) (floor (sqrt (length rs))))))
  "Cut points: recurse on the best cut worth making (see worth);
   bins hold >= sqrt(n) rows"
  (labels ((ok (yes no) (worth all yes no))
           (cut (rows) (best-cut (list self) rows y make
                                 :leaf leaf :ok #'ok))
           (vs (tr)                       ; cuts, left to right
             (and (? tr at) (append (vs (? tr yes)) (list (? tr v))
                                    (vs (? tr no))))))
    (vs (rdiv rs y #'cut make))))

(defm worth (num yes no) "Halves' means differ by > eps * all's sd"
  (> (abs (- (mid yes) (mid no))) (* (? *k* eps) (div self))))

(defun kinds (s) "#classes seen in sym s"
  (count-if #'plusp (? s has) :key #'cdr))

(defm worth (sym yes no
             &aux (a (? yes has)) (b (? no has))
                  (cs (append                  ; counts in yes + no
                       (loop :for (k . v) :in a
                             :collect (+ v (or (cdr (assoc k b
                                                           :test #'equal))
                                               0)))
                       (loop :for (k . v) :in b
                             :unless (assoc k a :test #'equal)
                             :collect v))))
  "Fayyad-Irani MDL: the entropy gain beats the bits to say the cut"
  (let* ((n (+ (? yes n) (? no n))) (e (ent cs n))
         (k (count-if #'plusp cs)))
    (> (- e (xpect yes no))
       (/ (+ (log (1- n) 2)
             (log (- (expt 3 k) 2) 2)
             (- (* k e))
             (* (kinds yes) (div yes))
             (* (kinds no) (div no)))
          n))))

(defstruct (bin (:include num)) edges)   ; a num, cut only at its edges

(defm cuts (bin rows y make fn
            &aux (es (coerce $edges 'vector))
                 (bs (make-array (1+ (length es)) :initial-element nil))
                 (yes (funcall make)) (no (funcall make)))
  "Offer (fn self e yes no) per edge e: x <= e vs the rest. No sort:
   drop each y in its bin, then sweep bins from no to yes"
  (dolist (r rows)
    (let ((x (aref r $at)))
      (unless (eql x '?)
        (push (add no (funcall y r)) (aref bs (bisect es x))))))
  (loop :for e :across es :for b :across bs
        :do (dolist (v b) (add yes v) (add no v -1))
            (funcall fn self e yes no)))

(defun quantiles (c rows q &aux (xs (coerce (sort (xys c rows #'identity)
                                                  #'< :key #'car)
                                            'vector))
                                (n (length xs)))
  "Equal-frequency edges: up to q-1 cuts, y ignored"
  (remove-duplicates (loop :for j :from 1 :below (if (plusp n) q 0)
                           :collect (car (aref xs (floor (* j n) q))))))

(defun discretize (xs rows y &key (make #'make-num) q)
  "xs, each num swapped for a bin holding its edges: its bins, or
   (q = an integer) its q equal-frequency quantiles"
  (loop :for c :in xs
        :collect (if (num-p c)
                   (make-bin :at (? c at) :txt (? c txt)
                             :edges (if q (quantiles c rows q)
                                        (bins c rows y make)))
                   c)))

;;;; coded: binned rows as fixnums, for fast class trees ----------------
(defun choices (c) "Vector of c's cut values: sym values, or bin edges"
  (coerce (if (sym-p c) (mapcar #'car (? c has)) (? c edges)) 'vector))

(defun coder (xs klass rows &aux (h (make-hash-table :test #'eq))
                                 (cs (mapcar #'choices xs))
                                 (ks (choices klass)))
  "Hash: row -> fixnum vector, one bin (sym: value) index per col in
   xs (-1 = ?), then the class index. Built once; shared by all trees"
  (labels ((idx (c ch x)
             (cond ((eql x '?) -1)
                   ((sym-p c) (position x ch :test #'equal))
                   (t (bisect ch x)))))
    (dolist (r rows h)
      (setf (gethash r h)
            (coerce (append (loop :for c :in xs :for ch :in cs
                                  :collect (idx c ch (aref r (? c at))))
                            (list (position (aref r (? klass at)) ks
                                            :test #'equal)))
                    '(simple-array fixnum (*)))))))

(defun ent2 (yes all)
  "Size-weighted entropy of class counts yes vs all-yes; and both sizes"
  (declare (optimize speed) (type (simple-array fixnum (*)) yes all))
  (let* ((ny (reduce #'+ yes)) (na (reduce #'+ all)) (nn (- na ny))
         (sy 0d0) (sn 0d0))
    (declare (fixnum ny na nn) (double-float sy sn))
    (loop :for c :of-type fixnum :across yes
          :for a :of-type fixnum :across all :for d := (- a c)
          :do (if (plusp c) (decf sy (* c (log (/ (float c 1d0) ny)))))
              (if (plusp d) (decf sn (* d (log (/ (float d 1d0) nn))))))
    (values (/ (+ sy sn) (log 2d0) (max na 1)) ny nn)))  ; ln -> bits

(defun bcut (xs cs vs nk &optional m
                         &aux (best (argmost #'<)) (at (length xs))
                              (pick (and m (subseq
                                            (shuffle (loop :for i :below at
                                                           :collect i))
                                            0 (min m at))))
                              (nb (1+ (reduce #'max (mapcar #'length cs)
                                              :initial-value 0)))
                              (n (make-array (list nb nk)
                                             :element-type 'fixnum))
                              (all (make-array nk :element-type 'fixnum))
                              (yes (make-array nk :element-type 'fixnum)))
  "Lowest-cost (col v) over coded rows vs: per col, count classes per
   bin in one pass, then sweep the bins. Bins: x <= edge; syms: = v.
   m: try just m random cols (a random forest)"
  (declare (fixnum at nk))
  (loop :for c :in xs :for ch :in cs :for i :of-type fixnum :from 0
        :for sym := (sym-p c)
        :when (or (null pick) (member i pick))
        :do (fill (sb-ext:array-storage-vector n) 0)
            (fill all 0) (fill yes 0)
            (dolist (v vs)
              (let ((b (aref (the (simple-array fixnum (*)) v) i))
                    (k (aref (the (simple-array fixnum (*)) v) at)))
                (declare (fixnum b k))
                (when (>= b 0) (incf (aref n b k)) (incf (aref all k)))))
            (dotimes (b (length ch))
              (dotimes (k nk)            ; bins: add up; syms: just this
                (if sym (setf (aref yes k) (aref n b k))
                    (incf (aref yes k) (aref n b k))))
              (multiple-value-bind (e ny nn) (ent2 yes all)
                (if (<= (? *k* leaf) (min ny nn))
                  (funcall best (list c (aref ch b)) e)))))
  (funcall best))

;;;; forest ------------------------------------------------------------
(defun vote (trees r &optional ks &aux h (w (float (length trees))))
  "class -> probability: mean over trees of r's leaf's class share.
   Given all classes ks, Laplace-smoothed: (n_k + 1) / (n + |ks|)"
  (dolist (tr trees h)
    (let* ((s (? (leaf tr r) ys)) (n (? s n)))
      (if ks
        (dolist (k ks)
          (incf (has k h) (/ (1+ (or (cdr (assoc k (? s has)
                                                 :test #'equal))
                                     0))
                             (+ n (length ks)) w)))
        (loop :for (k . v) :in (? s has)
              :do (incf (has k h) (/ v n w)))))))

(defun auc (xs &aux (xs (sort (copy-list xs) #'< :key #'car))
                    (np (count t xs :key #'cdr)) (nn (- (length xs) np)))
  "ROC AUC, 0..1, over (score . in-class?): the chance a random
   in-class row outscores a random other (Mann-Whitney, ties averaged)"
  (unless (or (zerop np) (zerop nn))
    (let ((i 0) (rsum 0))
      (loop :while xs
            :do (let* ((s (caar xs))
                       (tie (loop :while (and xs (= s (caar xs)))
                                  :collect (pop xs)))
                       (j (+ i (length tie))))
                  (incf rsum (* (/ (+ i 1 j) 2) (count t tie :key #'cdr)))
                  (setf i j)))
      (/ (- rsum (/ (* np (1+ np)) 2)) (* np nn)))))

(defun aucs (pairs &aux (n 0) (sum 0))
  "Expected AUC, 0..100, over (want . probs): one-vs-rest auc per
   class, weighted by that class's share of the rows"
  (dolist (c (remove-duplicates (mapcar #'car pairs) :test #'equal))
    (aif (auc (loop :for (w . p) :in pairs
                    :collect (cons (or (cdr (assoc c p :test #'equal)) 0)
                                (equal w c))))
      (let ((m (count c pairs :key #'car :test #'equal)))
        (incf n m) (incf sum (* m it)))))
  (* 100 (/ sum (max n 1))))

;;;; egs ---------------------------------------------------------------
(defun eg-config () (format t "~a~%" *k*))

(defun eg-tbl (&aux (tb (tbl (csv (? *k* file)))))
  (format t "~a rows, ~a x, ~a y~%"
          (length (? tb rows))
          (length (? tb cols x)) (length (? tb cols y)))
  (dolist (c (? tb cols all)) (format t "~10a ~,2f~%" (? c txt) (mid c))))

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
  (show (tree tb (? tb rows)   ; tb frozen during build, so memo is safe
              (memo (lambda (r) (disty tb r))))))

(defun eg-bins (&aux (tb (tbl (csv (? *k* file)))))
  (let ((y (memo (lambda (r) (disty tb r)))))
    (dolist (c (? tb cols x))
      (if (num-p c)
        (format t "~10a ~a~%" (? c txt) (bins c (? tb rows) y))))))

(defparameter *ens* ; 20 classify sets, 150..1000 rows
  '("balance.scale" "blood-transfusion-service-center" "breast.w"
    "climate-model-simulation-crashes" "column2C" "column3C" "credit.a"
    "diabetes" "ecoli" "eucalyptus" "glass" "heart.statlog" "ilpd"
    "ionosphere" "iris" "kc2" "sonar" "vehicle" "wdbc" "wine"))

(defun capped (rows y n &aux h) "At most n random rows per class"
  (loop :for r :in (shuffle rows)
        :when (<= (incf (has (funcall y r) h)) n) :collect r))

(defun stub (tr) "Copy of node tr, as a leaf"
  (let ((s (copy-node tr))) (setf (? s at) nil (? s yes) nil (? s no) nil) s))

(defun ffts (tr &aux (l (? tr yes)) (r (? tr no)))
  "Every fast-frugal tree inside tr: at each node, stop (tr is a leaf),
   exit left (left is a leaf; go on right), or exit right. <= 3^depth"
  (flet ((fork (yes no) (let ((n (copy-node tr)))
                          (setf (? n yes) yes (? n no) no) n)))
    (if (null (? tr at))
      (list tr)
      (cons (stub tr)
            (append (mapcar (lambda (r2) (fork (stub l) r2)) (ffts r))
                    (mapcar (lambda (l2) (fork l2 (stub r))) (ffts l)))))))

(defun fft (tb rows y &aux (rs (shuffle rows))
                           (n (floor (* 2 (length rs)) 3))
                           (best (argmost)))
  "Grow one tree on 2/3 of rows; return its fft with the best auc on
   the other 1/3"
  (let ((tr (tree tb (subseq rs 0 n) y #'make-sym))
        (valid (subseq rs n)))
    (dolist (f (ffts tr) (funcall best))
      (funcall best f (aucs (mapcar (lambda (r)
                                      (cons (funcall y r)
                                            (vote (list f) r)))
                                    valid))))))

(defun ens (tb train test y n b4 &key lap rf q cap fft)
  "(want . probs) per test row, voting n trees grown on random
   halves of train (n=1: all of it); b4: discretize train first
   (q: into q quantiles); lap: Laplace leaves; rf: sqrt(#x) random
   cols per node; cap: train on <= cap rows per class; fft: one
   tree, then its best fast-frugal subtree (see fft)"
  (let* ((train (if cap (capped train y cap) train))
         (k (? tb cols klass))
         (xs (if b4 (discretize (? tb cols x) train y
                                :make #'make-sym :q q)
                 (? tb cols x)))
         (m (and rf (ceiling (sqrt (length xs)))))
         (cs (and b4 (mapcar #'choices xs)))
         (h (and b4 (coder xs k train)))
         (ts (if fft (list (fft tb train y)) (loop :repeat n
                   :for rs := (if (= n 1) train
                                (subseq (shuffle train) 0
                                        (floor (length train) 2)))
                   :collect (if b4
                              (rdiv rs y (lambda (rs)
                                           (bcut xs cs
                                                 (mapcar (lambda (r)
                                                           (gethash r h))
                                                         rs)
                                                 (length (? k has)) m))
                                    #'make-sym)
                              (tree tb rs y #'make-sym xs))))))
    (mapcar (lambda (r)
              (cons (funcall y r)
                    (vote ts r (and lap (mapcar #'car (? k has))))))
            test)))

(defvar *ens-file* "ens.dat")

(defparameter *treatments*            ; name, #trees, b4?, then ens keys
  '(("1:inc" 1 nil) ("1:b4" 1 t) ("30:inc" 30 nil) ("30:b4" 30 t)
    ("+lap" 30 t :lap t) ("+rf" 30 t :lap t :rf t)
    ("+q16" 30 t :lap t :rf t :q 16) ("15:all" 15 t :lap t :rf t :q 16)))

(defun eg-ens (&aux (ts *treatments*) (ks (mapcar #'car ts))
                    (means (mapcar #'list ks)) (secs (mapcar #'list ks))
                    (wins (mapcar (lambda (k) (cons k 0)) ks)))
  "AUC, secs per *treatments*. ! = rank 1 on that data's 5x2 fold
   aucs; wins = #data ranked 1. ens.dat gets a #names line, then per
   fold: data | aucs | secs | test row #s (for lgbm.py, which adds
   lightgbm on the same folds)"
  (setf (? *k* leaf) 2 (? *k* maxd) 30)
  (let ((out (open *ens-file* :direction :output :if-exists :supersede)))
  (unwind-protect                 ; plain close: a killed run keeps its rows
   (progn
  (format out "#~{~a ~}~%" ks)
  (labels ((loss (alist)                 ; rank wants smaller = better
             (mapcar (lambda (e) (cons (car e)
                                       (mapcar (lambda (a) (- 100 a))
                                               (cdr e))))
                     alist))
           (mu (xs) (/ (reduce #'+ xs) (max 1 (length xs))))
           (f1 (x) (format nil "~5,1f" x)))
    (format t "~&~34a~{~12a~}~%" "#data" ks)
    (dolist (f *ens*)
      (let* ((tb (tbl (csv (format nil "~~/gits/moot/classify/~a.csv" f))))
             (at (? tb cols klass at))
             (y (lambda (r) (aref r at)))
             (d (mapcar #'list ks))
             (ix (make-hash-table :test #'eq)))  ; row -> its csv row #
        (loop :for r :in (reverse (? tb rows)) :for i :from 0
              :do (setf (gethash r ix) i))
        (xval (remove '? (? tb rows) :key y)
              (lambda (train test)
                (finish-output out)
                (loop :for tr :in ts :for e :in d :for s :in secs
                      :for t0 := (get-internal-real-time)
                      :do (push (aucs (apply #'ens tb train test y
                                             (cdr tr)))
                                (cdr e))
                          (push (- (get-internal-real-time) t0) (cdr s)))
                (format out "~a|~{~,3f ~}|~{~,3f ~}|~{~a ~}~%" f
                        (mapcar #'cadr d)
                        (mapcar (lambda (s)
                                  (/ (cadr s) 1.0
                                     internal-time-units-per-second))
                                secs)
                        (mapcar (lambda (r) (gethash r ix)) test)))
              :m 5 :n 2)
        (let ((r (rank (loss d))))
          (format t "~34a~{~12a~}~%" f
                  (loop :for (k . as) :in d :for m :in means
                        :for w :in wins
                        :for one := (= 1 (cdr (assoc k r :test #'equal)))
                        :do (push (mu as) (cdr m))
                            (if one (incf (cdr w)))
                        :collect (format nil "~5,1f~a" (mu as)
                                         (if one "!" ""))))
          (finish-output))))
    (format t "~%~34a~{~12a~}~%" "#wins"
            (mapcar (lambda (w) (format nil "~5d" (cdr w))) wins))
    (format t "~34a~{~12a~}~%" "#mean auc"
            (mapcar (lambda (m) (f1 (mu (cdr m)))) means))
    (format t "~34a~{~12a~}~%" "#secs (all data)"
            (loop :for (k . ts) :in secs
                  :collect (f1 (/ (reduce #'+ ts)
                                  internal-time-units-per-second))))))
   (close out))))

(defun eg-all ()
  (dolist (f '(eg-config eg-tbl eg-dist eg-same eg-rank eg-tree eg-bins))
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

