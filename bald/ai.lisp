; vim: set ts=2 sw=2 sts=2 et lisp cc=75 lispwords+=loop,defm,aif,cond,labels :
;;; ai: tiny AI, explained: the model. Sections follow the python
;;; hair/ files: about, data, dist, stats, tree. No printing, no files:
;;; input is rows, output is structs and alists. Csv reading lives in
;;; lithp.lisp, printing and experiments in eg.lisp.
;;; (c) 2026, Tim Menzies <timm@ieee.org>, MIT license
(in-package :bald)

;;;; about -------------------------------------------------------------
(defstruct about
  (file "~/gits/moot/optimize/misc/auto93.csv")
  (seed 1234567891) (pow 2) (leaf 3) (maxd 8) (eps 0.35) (bins 8))

(defvar *k* (make-about))

;;;; data --------------------------------------------------------------
;;; Header: Upper case = Num, else Sym. Last char: + - goal (a y),
;;; ! class, X skip, else an x. "?" cells are unknown. Rows keep their
;;; raw cells, and cooked ones: each x's bin # (see discretize).
(defstruct row raw cooked)
(defun row (cells) (make-row :raw cells))
(defun raw    (r i) (aref (row-raw r) i))
(defun cooked (r i) (aref (row-cooked r) i))

(defstruct counted (n 0))                         ; things that add
(defstruct (col0 (:include counted)) (at 0) (txt "") bins) ; # -> value
(defstruct (num (:include col0)) (mu 0) (m2 0) (goal 1))
(defstruct (sym (:include col0)) has)   ; has = alist value . count
(defstruct cols names all x y klass)
(defstruct (tbl (:include counted)) cols rows)

(defun col (at txt)
  (if (upper-case-p (charn txt 0))
    (make-num :at at :txt txt :goal (if (eql #\- (charn txt)) 0 1))
    (make-sym :at at :txt txt)))

(defm add (counted v &optional (inc 1)) "Grow (inc=-1: shrink) num,sym,tbl"
  (unless (eql v '?) (incf $n inc) (add1 self v inc))
  v)

(defm add1 (tbl r inc)
  (if (plusp inc) (push r $rows) (setf $rows (remove r $rows)))
  (dolist (c (? $cols all)) (add c (raw r (? c at)) inc)))

(defm add1 (sym v inc) (incf (has v $has) inc))

(defm add1 (num v inc)
  (if (<= $n 0)
    (setf $n 0 $mu 0 $m2 0)
    (let ((d (- v $mu)))
      (incf $mu (/ (* inc d) $n))
      (setf $m2 (max 0 (+ $m2 (* inc d (- v $mu))))))))

(defm fold (sym o &optional (inc 1)) "Add (inc=-1: take) o into self"
  (loop :for (k . v) :in (? o has) :do (incf (has k $has) (* inc v)))
  (incf $n (* inc (? o n)))
  self)

(defm fold (num o &optional (inc 1))
  "Add (inc=-1: take) o into self (Chan et al.'s merge)"
  (let* ((m (? o n)) (n (+ $n (* inc m))))
    (if (<= n 0)
      (setf $n 0 $mu 0 $m2 0)
      (let* ((mu (+ $mu (/ (* inc m (- (? o mu) $mu)) n)))
             (d  (- (? o mu) (if (< n $n) mu $mu)))) ; o vs the rest
        (setf $m2 (max 0 (+ $m2 (* inc (+ (? o m2)
                                          (/ (* (min n $n) m d d)
                                             (max n $n))))))
              $mu mu
              $n  n))))
  self)

(defun adds (vs &optional (col (make-num))) 
  (dolist (v vs col) (add col v)))

(defm mid (num) "Center: the mean" $mu)

(defm mid (sym) "Center: the mode"
  (car (first (stable-sort (copy-list $has) #'> :key #'cdr))))

(defm div (num) "Spread: standard deviation"
  (if (< $n 2) 0 (sqrt (/ $m2 (1- $n)))))

(defun ent (cs n) "Entropy of counts cs, summing to n"
  (loop :for c :in cs :when (plusp c)
        :sum (let ((p (/ c (float n 1d0)))) (- (* p (log p 2))))))

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

(defun tbl (rows) "First row names the cols. Then discretize"
  (discretize (adds (mapcar #'row (cdr rows))
                    (make-tbl :cols (cols (car rows))))))

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
  (minkowski (loop :for c :in (? $cols x) :for v := (raw q (? c at))
               :unless (eql v '?) :collect (gap c (raw r (? c at)) v))))

(defm disty (tbl r) "Distance to heaven (0 = best)"
  (minkowski (loop :for c :in (? $cols y) :for v := (raw r (? c at))
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

;;;; bins: discretize once; trees then cut on bin #s ------------------
;; q=8 equal-freq bins: on 39 classify sets (5x2), -0.3 auc vs q=256;
;; equal-width 8 bins: -0.7 (and -17 on skewed hypothyroid)
(defm choices (sym rows q) "Bins: the q-1 commonest values"
  (let ((vs (sort (copy-list $has) #'> :key #'cdr)))
    (map 'vector #'car (subseq vs 0 (min (1- q) (length vs))))))

(defm choices (num rows q)
  "Bins: each one's lowest value; q bins, equal #known xs in each"
  (let* ((xs (sort (remove '? (map 'vector (lambda (r) (raw r $at)) rows))
                   #'<))
         (n (length xs)))
    (coerce (remove-duplicates
             (loop :for j :below (if (plusp n) q 0)
                   :collect (aref xs (floor (* j n) q))))
            'vector)))

(defm cook (num x) "x -> bin #: the last bin whose low <= x; ? = -1"
  (if (eql x '?) -1 (max 0 (1- (bisect $bins x #'<=)))))

(defm cook (sym x) "x -> bin #: its place in bins, else (length bins)"
  (if (eql x '?) -1 (or (position x $bins :test #'equal) (length $bins))))

(defun discretize (tb &optional (q (? *k* bins)) &aux (xs (? tb cols x)))
  "Once per tbl: each x col's bins, then each row's bin # per x col"
  (dolist (c xs) (setf (? c bins) (choices c (? tb rows) q)))
  (dolist (r (? tb rows) tb)
    (setf (row-cooked r) (make-array (length (row-raw r))
                                     :initial-element -1))
    (dolist (c xs)
      (setf (aref (row-cooked r) (? c at)) (cook c (raw r (? c at)))))))

;;;; tree --------------------------------------------------------------
(defstruct node at v ys n rows yes no)

(defun xpect (a b &aux (n (col0-n a)) (m (col0-n b)))
  "Size-weighted spread of two summaries (lower = better)"
  (/ (+ (* n (div a)) (* m (div b))) (+ n m +tiny+)))

(defun tree (rows y xs &key (make #'make-num) m (leaf (? *k* leaf))
                            (maxd (? *k* maxd)) (lvl 0)
                       &aux (ys (mapcar y rows)))
  "A node per level, down to maxd. Cut on the best (col bin#) of cols
   xs (m: just m random ones per node); rows where yes? go left"
  (let ((it (and (< lvl maxd) (best-cut xs rows ys make m leaf))) yes no)
    (if it
      (dolist (r rows)
        (if (yes? (first it) (second it) r) (push r yes) (push r no))))
    (labels ((kid (rows) (tree rows y xs :make make :m m :leaf leaf
                                         :maxd maxd :lvl (1+ lvl))))
      (make-node :ys (adds ys (funcall make)) :n (length rows) :rows rows
                 :at (first it) :v (second it)
                 :yes (and it (kid yes)) :no (and it (kid no))))))

(defun yes? (c b r &aux (w (cooked r (col0-at c))))
  "Is row r on the yes side of col c's cut at bin b? (? = yes)"
  (or (< w 0) (if (sym-p c) (= w b) (<= w b))))

(defun leaf (tr r) "Walk row r down to its leaf"
  (loop :while (node-at tr)
        :do (setf tr (if (yes? (node-at tr) (node-v tr) r)
                       (node-yes tr)
                       (node-no tr))))
  tr)

(defun walk (tr &optional (lvl 0) test)
  "Nodes, parents first: (lvl test mid n); test = (txt op v), the
   cut that leads here (nil at the root), in raw values"
  (cons (list lvl test (mid (? tr ys)) (? tr n))
        (aif (? tr at)
          (let* ((b (? tr v)) (s (sym-p it))
                 (v (aref (? it bins) (if s b (1+ b)))))  ; num: next low
            (flet ((kid (sub op)
                     (walk sub (1+ lvl) (list (? it txt) op v))))
              (append (kid (? tr yes) (if s "==" "<"))
                      (kid (? tr no)  (if s "!=" ">="))))))))

(defun best-cut (xs rows ys make m leaf &aux (best (argmost #'<)))
  "Lowest-xpect (col bin#) over cols xs, for rows with y values ys.
   make: y summary (make-sym = classes). m: just m random cols"
  (dolist (i (pick (length xs) m) (funcall best))
    (cuts (nth i xs) rows ys make leaf best)))

(defun cuts (c rows ys make leaf best &aux (k (length (col0-bins c))))
  "Offer each cut (c b) of col c to best, scored by xpect: summarize
   ys per bin (one pass over rows), then sweep the bins, folding
   summaries. Nums: bin <= b; syms: bin = b"
  (let ((bins (coerce (loop :repeat (1+ k) :collect (funcall make))
                      'vector)))
    (loop :for r :in rows :for y :in ys :for b := (cooked r (col0-at c))
          :unless (< b 0) :do (add (aref bins b) y))
    (loop :with yes := (funcall make)
          :and no := (reduce #'fold bins :initial-value (funcall make))
          :for b :below k :for bin := (aref bins b)
          :do (fold no bin -1)                ; syms: bin vs rest
              (unless (sym-p c) (fold yes bin)) ; nums: bins <= b
              (let ((yes (if (sym-p c) bin yes)))
                (if (<= leaf (min (col0-n yes) (col0-n no)))
                  (funcall best (list c b) (xpect yes no))))
              (if (sym-p c) (fold no bin)))))

;;;; forest ------------------------------------------------------------
(defun capped (rows y n &aux h) "At most n random rows per class"
  (loop :for r :in (shuffle rows)
        :when (<= (incf (has (funcall y r) h)) n) :collect r))

(defun forest (tb rows y &key (n 10) (cap 200) rf (leaf 2) (maxd 30)
                          &aux (xs (? tb cols x)))
  "n deep class trees (small leaves), each on a random half of rows
   (n=1: all of them), from <= cap rows per class. rf: sqrt(#x)
   random cols per node"
  (let ((rows (if cap (capped rows y cap) rows)))
    (loop :repeat n
          :collect (tree (if (= n 1) rows
                             (subseq (shuffle rows) 0
                                     (floor (length rows) 2)))
                         y xs :make #'make-sym :leaf leaf :maxd maxd
                         :m (and rf (ceiling (sqrt (length xs))))))))

(defun vote (trees r &aux h (w (float (length trees))))
  "class -> probability: mean over trees of r's leaf's class share"
  (dolist (tr trees h)
    (let ((s (node-ys (leaf tr r))))
      (loop :for (k . v) :in (sym-has s)
            :do (incf (has k h) (/ v (float (sym-n s)) w))))))

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
