; vim: set ts=2 sw=2 sts=2 et lisp cc=75 lispwords+=loop,defm,aif,cond,labels :
;;; ai: tiny AI, explained: the model. Sections follow the python
;;; hair/ files: about, data, dist, stats, tree. No printing, no files:
;;; input is rows, output is structs and alists. Csv reading lives in
;;; lithp.lisp, printing and experiments in eg.lisp.
;;; (c) 2026, Tim Menzies <timm@ieee.org>, MIT license
(in-package :hair)

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
  "Columns from the header row; last char of name says x, y, ! or X.
   ! = the class, and a y too"
  (with-slots (all klass x y) self
    (setf all (loop :for txt :across names :for at :from 0
                    :collect (col at txt)))
    (dolist (c all self)
      (let ((e (charn (? c txt))))
        (cond ((skip? (? c txt)))
              ((find e "+-!") (push c y)
                              (if (eql e #\!) (setf klass c)))
              (t (push c x)))))))

(defun tbl (rows) "First row names the cols"
  (adds (cdr rows) (make-tbl :cols (cols (car rows)))))

(defm clone (tbl &optional rows) (tbl (cons (? $cols names) rows)))

;;;; dist --------------------------------------------------------------
(defun minkowski (vs &optional (p (? *k* pow)) &aux (n 0) (d 0))
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

(defun same (xs ys &optional (eps (? *k* eps))
                &aux (xs (sort (copy-list xs) #'<))
                     (ys (sort (copy-list ys) #'<)))
  "Indistinguishable? all three tests must agree"
  (and (<= (cohen xs ys) eps)
       (<= (cliffs xs ys) 0.195)
       (<= (ks xs ys) 1.36)))

(defun rank (d &optional (lt #'<))
  "alist (name . nums) -> alist (name . rank), 1 = best (by lt)"
  (loop :with r := 1 :and pool
    :for (k . xs) 
    :in (sort (copy-list d) lt :key (lambda (x) (median (cdr x))))
    :do (when (and pool (not (same pool xs))) (setf r (1+ r) pool nil))
    (setf pool (append pool xs))
    :collect (cons k r)))

(defun xval (rows fn &key (m 5) (n 2)) "m*n cross-val: (fn train test)"
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

(defun best-cut (cols rows y make &key (leaf (? *k* leaf))
                                      &aux (best (argmost #'<)))
  "Lowest-cost (col v) over cols, both sides >= leaf rows; and its cost"
  (labels ((fun (c v yes no)            ; cuts passes its col as c
                (if (<= leaf (min (? yes n) (? no n)))
                  (funcall best (list c v) (xpect yes no)))))
    (dolist (c cols (funcall best))
      (cuts c rows y make #'fun))))

(defun tree (rows y cut &key (make #'make-num) (maxd (? *k* maxd))
                             (lvl 0))
  "A node per level, down to maxd. (cut rows) => nil (a leaf) or
   (col v): rows where (yes? col v r) go left. E.g. cut = best-cut"
  (let ((it (and (< lvl maxd) (funcall cut rows))))
    (labels ((kid (rows) (tree rows y cut :make make :maxd maxd
                                          :lvl (1+ lvl))))
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

(defun yes? (c v r &aux (w (aref r (? c at))))
  "Is row r on the yes side of col c's cut v? (? = yes)"
  (or (eql w '?) (if (sym-p c) (equal w v) (<= w v))))

(defun leaf (tr r) "Walk row r down to its leaf"
  (loop :while (? tr at)
        :do (setf tr (if (yes? (? tr at) (? tr v) r)
                       (? tr yes)
                       (? tr no))))
  tr)

(defun walk (tr &optional (lvl 0) test)
  "Nodes, parents first: (lvl test mid n); test = (txt op v), the
   cut that leads here (nil at the root)"
  (cons (list lvl test (mid (? tr ys)) (? tr n))
        (aif (? tr at)
          (flet ((kid (sub op) (walk sub (1+ lvl) (list (? it txt) op
                                                        (? tr v)))))
            (append (kid (? tr yes) (if (sym-p it) "==" "<="))
                    (kid (? tr no)  (if (sym-p it) "!=" ">")))))))

;;;; bins --------------------------------------------------------------
(defun quantiles (c rows q &aux (xs (coerce (sort (xys c rows #'identity)
                                                  #'< :key #'car)
                                            'vector))
                                (n (length xs)))
  "Equal-frequency edges: up to q-1 cuts, y ignored"
  (remove-duplicates (loop :for j :from 1 :below (if (plusp n) q 0)
                           :collect (car (aref xs (floor (* j n) q))))))

(defun choices (c rows q)
  "Vector of c's cut values: sym values, or num's q quantile edges"
  (coerce (if (sym-p c) (mapcar #'car (? c has)) (quantiles c rows q))
          'vector))

;;;; coded: binned rows as fixnums, for fast class trees ----------------
(defun coder (xs cs klass rows &aux (h (make-hash-table :test #'eq))
                                    (ks (mapcar #'car (? klass has))))
  "Hash: row -> fixnum vector, one bin (sym: value) index per col in
   xs, cut at cs (-1 = ?), then the class index. Built once; shared
   by all trees"
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

(defun bcut (xs cs vs nk &key m (leaf (? *k* leaf))
                         &aux (best (argmost #'<)) (at (length xs))
                              (pick (and m (subseq
                                            (shuffle (loop :for i :below at
                                                           :collect i))
                                            0 (min m at))))
                              (nb (1+ (reduce #'max (mapcar #'length cs)
                                              :initial-value 0)))
                              (n (make-array (list nb nk)
                                             :element-type 'fixnum))
                              (flat (make-array (* nb nk) :displaced-to n
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
        :do (fill flat 0)
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
                (if (<= leaf (min ny nn))
                  (funcall best (list c (aref ch b)) e)))))
  (funcall best))

;;;; forest ------------------------------------------------------------
(defun capped (rows y n &aux h) "At most n random rows per class"
  (loop :for r :in (shuffle rows)
        :when (<= (incf (has (funcall y r) h)) n) :collect r))

(defun binned (tb rows &key (q 8) rf leaf
               &aux (k (? tb cols klass)) (xs (? tb cols x))
                    (cs (mapcar (lambda (c) (choices c rows q)) xs))
                    (h (coder xs cs k rows))
                    (m (and rf (ceiling (sqrt (length xs))))))
  "A cut for tree: bcut on rows coded once into q bins (rf: just
   sqrt(#x) random cols per node)"
  (lambda (rs) (bcut xs cs (mapcar (lambda (r) (gethash r h)) rs)
                     (length (? k has)) :m m :leaf leaf)))

(defun forest (tb rows y &key (n 10) (cap 200) b4 (q 8) rf
                              (leaf 2) (maxd 30)
               &aux (rows (if cap (capped rows y cap) rows))
                    (cut (if b4 (binned tb rows :q q :rf rf :leaf leaf)
                             (lambda (rs) (best-cut (? tb cols x) rs y
                                                    #'make-sym
                                                    :leaf leaf)))))
  "n deep class trees (small leaves), each on a random half of rows
   (n=1: all of them), from <= cap rows per class. b4: cut on rows
   binned first (see binned)"
  (loop :repeat n
        :collect (tree (if (= n 1) rows
                           (subseq (shuffle rows) 0
                                   (floor (length rows) 2)))
                       y cut :make #'make-sym :maxd maxd)))

(defun vote (trees r &aux h (w (float (length trees))))
  "class -> probability: mean over trees of r's leaf's class share"
  (dolist (tr trees h)
    (let ((s (? (leaf tr r) ys)))
      (loop :for (k . v) :in (? s has)
            :do (incf (has k h) (/ v (? s n) w))))))

(defun votes (trees rows y) "(want . probs) per row, for aucs"
  (mapcar (lambda (r) (cons (funcall y r) (vote trees r))) rows))

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
