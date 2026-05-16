#!/usr/bin/env -S sbcl --script
; vim: set ft=lisp ts=2 sw=2 et lw+=defmethods,defeg,let+ :

;; wise.lisp -- data-lite active learning. one file.
;; (c) 2026 Tim Menzies, timm@ieee.org, MIT license.

;; Usage:
;;   sbcl --script wise.lisp --cmd [arg] [-flag value]...

;; Commands:
;;   --the     print config *the*
;;   --rows    read CSV, print some rows
;;   --data    build Data, show y-columns
;;   --stats   mid, spread per column
;;   --norm    raw vs normalized y of row 0
;;   --ydata   stride rows by goal-distance
;;   --xdata   stride rows by feature-distance from row 0
;;   --tree    build tree on first BUDGET random rows
;;   --atree   active-label tree (label only 'closer' rows)
;;   --active  20 train/test splits
;;   --check   prudence: distance, norm, stat invariants

;; Options:
;;   -p N  minkowski exponent (default 2)
;;   -f F  CSV file           (default auto93.csv)
;;   -s N  random seed        (default 1)
;;   -b N  label budget       (default 50)
;;   -u N  unlabelled cap     (default 128)

;; CSV: header names cols; suffix sets role.
;;   [A-Z]* numeric    [a-z]* symbolic
;;   *-     min (y)    *+     max (y)
;;   *!     class (y)  *X     skip
;;   ?      missing

;; Names: r/rs row(s)  d data  c col  i self  ys Num of distys

(load "lib")

;; =================================================================
;; Config
;; =================================================================

(defparameter *the*
  '((p      2            "-p" "minkowski exponent")
    (file   "auto93.csv" "-f" "data file")
    (seed   1            "-s" "seed")
    (budget 50           "-b" "max labels")
    (few    128          "-u" "max unlabelled pool")))

(defun the-of (k) (second (assoc k *the*)))

(setf *seed* @seed)

;; =================================================================
;; Sym -- symbolic column: count + frequency table.
;;   mid     mode (most common value)
;;   spread  entropy
;;   distx   0 if equal, else 1
;;   %cuts   one cut per known symbol
;; =================================================================

(defstruct sym (at 0) (txt " ") (n 0) has)

(defmethods sym
  (norm     (v)     v)
  (distx    (a b)   (if (equal a b) 0 1))
  (go-left? (v cut) (or (eq v '?) (equal v cut)))
  (ops      ()      '("==" "!="))
  (%cuts    (rs)    (declare (ignore rs))
                    (mapcar #'car $has))

  (mid () "Mode."
    (car (reduce (ff+ (if (> (cdr _) (cdr __)) _ __))
                 $has)))

  (spread () "Entropy."
    (loop for (_ . v) in $has
          sum (let ((p (/ v $n))) (- p (log p 2)))))

  (add (v &optional (w 1))
    "Fold V (weight W) into frequency table."
    (unless (eql v '?)
      (incf $n w)
      (let ((cell (assoc v $has :test #'equal)))
        (if cell
            (incf (cdr cell) w)
            (push (cons v w) $has))))
    v))

;; =================================================================
;; Num -- numeric column: Welford count, mean, m2, goal.
;;   mid     mean
;;   spread  sample standard deviation
;;   norm    logistic z-score, in [0,1]
;;   distx   normalized abs diff with '?' handling
;;   %cuts   median as the single cut
;;   goal    0 if txt ends in '-', else 1
;; =================================================================

(defstruct (num (:constructor %make-num))
  (at 0) (txt " ") (n 0) (mu 0) (m2 0) (goal 1))

(defun make-num (&rest args
                 &aux (i (apply #'%make-num args)))
  "Build a num; goal=0 if txt ends in '-', else 1."
  (setf $goal (if (eq #\- (ch $txt -1)) 0 1))
  i)

(defmethods num
  (mid      ()      $mu)
  (ops      ()      '("<=" ">"))
  (go-left? (v cut) (or (eq v '?) (<= v cut)))

  (spread () (if (< $n 2) 0
                 (sqrt (/ (max 0 $m2) (1- $n)))))

  (%cuts (rs)
    "Median as the lone cut."
    (let ((vs (loop for r in rs
                    for v = (elt r $at)
                    unless (eq v '?) collect v)))
      (and vs (list (elt (sort vs #'<)
                         (floor (length vs) 2))))))

  (norm (v)
    (if (eq v '?) v
        (let ((z (/ (- v $mu) (+ (spread i) 1e-32))))
          (/ 1
             (+ 1 (exp (* -1.7
                          (max -3 (min 3 z)))))))))

  (distx (a b)
    "Normalized abs diff with missing-value handling."
    (let ((a (norm i a)) (b (norm i b)))
      (cond ((eq a '?) (abs (- b (if (> b 0.5) 0 1))))
            ((eq b '?) (abs (- a (if (> a 0.5) 0 1))))
            (t         (abs (- a b))))))

  (add (v &optional (w 1))
    "Fold V (weight W) into running mean+m2."
    (unless (eq v '?)
      (cond ((and (minusp w) (<= $n 2))
             (setf $n 0 $mu 0 $m2 0))
            (t (incf $n w)
               (let ((delta (- v $mu)))
                 (incf $mu (/ (* w delta) $n))
                 (incf $m2 (* w delta (- v $mu)))))))
    v))

;; =================================================================
;; Cols -- header-derived collection: names, x, y, all.
;;   Column suffix decides role:
;;     ends ! - +  -> y-column
;;     ends X      -> skip
;;     else        -> x-column
;; =================================================================

(defstruct (cols (:constructor %make-cols))
  names x y all)

(defun make-cols (txts &aux (i (%make-cols :names txts))
                            (n -1))
  "Parse header TXTS; partition cols into x/y/skip."
  (let+ ((one (txt) (! (if (upper-case-p (ch txt 0))
                           #'make-num #'make-sym)
                        :txt txt :at (incf n)))
         (end (col) (ch (? col txt) -1)))
    (setf $all (mapcar #'one txts))
    (dolist (col $all)
      (unless (eql (end col) #\X)
        (if (find (end col) "!-+")
            (push col $y)
            (push col $x)))))
  i)

(defmethods cols
  (add (row &optional (w 1))
    (mapcar (ff+ (add _ __ w)) $all row)
    row))

;; =================================================================
;; Data -- table: rows + parsed columns + cached row-mid.
;;   make        first row is header
;;   add         fold row in (w=-1 removes)
;;   mid         per-column mid, cached
;;   distx       Minkowski distance over x-columns
;;   disty       distance to goals (0 best, 1 worst)
;;   wins        closure scoring rows 0..100, 100=best
;; =================================================================

(defstruct (data (:constructor %make-data))
  rows cols mid)

(defun make-data (&optional rows
                  &aux (i (%make-data
                            :cols (make-cols (car rows)))))
  "First row = header."
  (adds (cdr rows) i))

(defmethods data
  (mid ()
    "Cached centroid: per-column mid."
    (or $mid
        (setf $mid (mapcar #'mid (? i cols all)))))

  (distx (r1 r2)
    (dist (? i cols x)
          (lambda (i &aux (a (elt r1 $at))
                          (b (elt r2 $at)))
            (if (and (eq a '?) (eq b '?))
                1 (distx i a b)))))

  (disty (row &optional (cols (? i cols y)))
    "0 = perfect, 1 = worst."
    (dist cols
          (lambda (i)
            (abs (- (norm i (elt row $at)) $goal)))))

  (wins ()
    "Closure: score row 0..100. 100=best, 50=med."
    (let+ ((ys (mapcar (f+ (disty i _)) $rows))
           (ds (sort ys #'<))
           (lo (first ds))
           (md (elt ds (floor (length ds) 2))))
      (f+ (floor (* 100 (- 1 (/ (- (disty i _) lo)
                                (+ (- md lo) 1e-32))))))))

  (add (row &optional (w 1))
    "W=-1 removes ROW."
    (setf $mid nil)
    (add $cols row w)
    (if (plusp w)
        (push row $rows)
        (setf $rows
              (remove row $rows :test #'equal :count 1)))
    row))

;; =================================================================
;; Tree -- decision tree built by make-tree.
;;   Each node holds a column + cut. tree-splits enumerates
;;   candidate splits; one minimizing weighted spread of disty
;;   wins. Leaves hold a Num of distys for ranking.
;; =================================================================

(defstruct (tree (:constructor %make-tree) (:copier nil))
  data ynum col cut left right hdr)

(defun make-tree
          (data rs &optional (min 3) (hdr "")
                  &aux (i (%make-tree
                            :data (make-data
                                    (cons (? data cols names)
                                          rs))
                            :ynum (adds
                                    (mapcar
                                      (f+ (disty data _))
                                      rs))
                            :hdr hdr)))
  "Build node from RS; recurse on best split if big enough."
  (when (>= (length rs) (* 2 min))
    (let ((cands (tree-splits data rs min)))
      (when cands
        (let+ ((best (argm cands #'first))
               (c (second best)) (cut (third best)))
          (setf $col c $cut cut
                $left  (make-tree data (fourth best) min
                                  (tree-kid-hdr c cut t))
                $right (make-tree data (fifth best) min
                                  (tree-kid-hdr c cut nil)))))))
  i)

(defmethods tree
  (leaf (r)
    (if (null $left) i
        (leaf (if (go-left? $col
                            (elt r (? $col at))
                            $cut)
                  $left $right)
              r)))

  (show (&optional (lvl 0))
    "Show: indent+hdr, mu, n, y-centroids. Kids sorted."
    (format t "~&~30a ,~4,2f ,(~3d),  {~{~A~^, ~}}~%"
            (format nil "~{~A~}~A"
                    (loop repeat (max 0 (1- lvl))
                          collect "|   ")
                    (if (zerop lvl) "" $hdr))
            (mid $ynum) (? $ynum n)
            (loop for c in (? $data cols y)
                  collect
                    (format nil "~A=~A" (? c txt)
                            (if (typep c 'num)
                                (format nil "~,2f" (mid c))
                                (mid c)))))
    (when $left
      (dolist (k (lt (f+ (mid (? _ ynum)))
                     (list $left $right)))
        (show k (1+ lvl))))))

(defun tree-kid-hdr (c cut left?)
  (destructuring-bind (l r) (ops c)
    (format nil "~a ~a ~a"
            (? c txt) (if left? l r) cut)))

(defun tree-split (data c cut rs)
  "Returns (score c cut left-rs right-rs)."
  (let (l r (ln (make-num)) (rn (make-num)))
    (dolist (row rs)
      (if (go-left? c (elt row (? c at)) cut)
          (progn (push row l)
                 (add ln (disty data row)))
          (progn (push row r)
                 (add rn (disty data row)))))
    (list (+ (* (? ln n) (spread ln))
             (* (? rn n) (spread rn)))
          c cut l r)))

(defun tree-splits (data rs leaf)
  "Splits where both sides have at least LEAF rows."
  (loop for c in (? data cols x)
        append
          (loop for cut in (%cuts c rs)
                for s = (tree-split data c cut rs)
                when (>= (min (length (fourth s))
                              (length (fifth s)))
                         leaf)
                  collect s)))

;; =================================================================
;; Acquire -- active learner: lab, best, rest, ys, pool.
;;   Warm-start: label 4 rows, split into best+rest by disty.
;;   Loop: pick rows closer to best mid than rest mid, label,
;;   add to lab+best, rebalance so |best| <= sqrt(|lab|).
;; =================================================================

(defstruct (acquire (:constructor %make-acquire))
  lab best rest ys pool)

(defun make-acquire (rs
                     &aux (hd (list (car rs)))
                          (i  (%make-acquire
                                :lab  (make-data hd)
                                :best (make-data hd)
                                :rest (make-data hd)
                                :ys   (make-num)
                                :pool (shuffle (cdr rs)))))
  "Init from RS (first row = header). Warm-start 4 labels."
  (acquire-warm-start i)
  i)

(defun acquire-warm-start (i &aux (start 4))
  "Label 4 rows; split by disty into best vs rest."
  (dotimes (_ (min start (length $pool)))
    (let ((r (pop $pool)))
      (add $lab r) (add $ys (disty $lab r))))
  (let+ ((sorted (lt (f+ (disty $lab _))
                     (? $lab rows)))
         (n (max 1 (floor (sqrt (length sorted))))))
    (dolist (r (subseq sorted 0 n)) (add $best r))
    (dolist (r (subseq sorted n))   (add $rest r))))

(defun acquire-closer? (i r)
  (< (distx $lab r (mid $best))
     (distx $lab r (mid $rest))))

(defun acquire-rebalance (i)
  "Cap best at sqrt(|lab|); worst row -> rest."
  (when (> (length (? $best rows))
           (sqrt (length (? $lab rows))))
    (let ((bad (argm (? $best rows)
                     (f+ (disty $lab _))
                     #'>)))
      (sub $best bad) (add $rest bad))))

(defun acquire-train (i)
  "Run loop; return best, lab, label-count."
  (loop for r in (subseq $pool 0
                         (min @few (length $pool)))
        while (< (? $ys n) @budget) do
          (when (acquire-closer? i r)
            (add $ys (disty $lab r))
            (add $lab r) (add $best r)
            (acquire-rebalance i)))
  (values $best $lab (? $ys n)))

(defun validate (rs god w &optional (check 5))
  "Active + tree-rank holdout vs random baseline.
   Pay CHECK extra labels on each."
  (let+ ((body  (shuffle (cdr rs)))
         (n     (floor (length body) 2))
         (train (cons (car rs) (subseq body 0 n)))
         (test  (subseq body n))
         ((best lab labels)
          (acquire-train (make-acquire train)))
         (y-god (f+ (disty god _)))
         (train-best (argm (? lab rows) y-god))
         (tr (make-tree lab (? lab rows)))
         (ranked (lt (f+ (mid
                           (? (leaf tr _) ynum)))
                     test))
         (top  (subseq ranked 0
                       (min check (length ranked))))
         (pick (argm top y-god))
         (rnd-rows (subseq (shuffle test) 0
                           (min check (length test))))
         (rnd  (argm rnd-rows y-god)))
    (declare (ignore best))
    (values (! w train-best)
            (! w pick)
            (! w rnd)
            (+ labels check))))

;; =================================================================
;; Examples -- CLI entry points. Each defeg registers a --name.
;;   eg--check runs invariants; exits non-zero on failure.
;; =================================================================

(defun eg--the (&optional _)
  (declare (ignore _)) (print *the*))

(defeg eg--rows  "Print CSV tail."
  (print (subseq rs 380)))

(defeg eg--data  "Show y-columns."
  (print (? i cols y)))

(defeg eg--ydata "Stride by disty."
  (stride $rows (f+ (disty i _))))

(defeg eg--xdata "Stride by distx from row 0."
  (let ((r0 (car $rows)))
    (stride $rows (f+ (distx i r0 _)))))

(defeg eg--stats "Print mid, spread per column."
  (dolist (c (? i cols all))
    (format t "~&~12a  mid=~a  spread=~a~%"
            (? c txt) (mid c) (spread c))))

(defeg eg--norm "Row 0: raw vs normalized per y-column."
  (let ((r (car $rows)))
    (dolist (c (? i cols y))
      (let ((v (elt r (? c at))))
        (format t "~&~8a  raw=~a  norm=~a~%"
                (? c txt) v (norm c v))))))

(defeg eg--dbg "Per-y-col contribution to disty for row 0."
  (let ((r (car (? i rows))))
    (format t "~&row[0] = ~a~%" r)
    (dolist (c (? i cols y))
      (let* ((v  (elt r (? c at)))
             (nv (norm c v))
             (g  (? c goal)))
        (format t "~&  ~8a v=~a mu=~,3f sd=~,3f norm=~,3f goal=~a |.-goal|=~,3f~%"
                (? c txt) v (mid c) (spread c) nv g
                (abs (- nv g)))))
    (format t "~&  disty=~,4f~%" (disty i r))))

(defeg eg--tree "Build tree on first BUDGET random rows."
  (let* ((body   (shuffle (cdr rs)))
         (sample (subseq body 0
                         (min @budget (length body))))
         (lab    (make-data (cons (car rs) sample))))
    (format t "~&;; labels used: ~d~%" (length sample))
    (show (make-tree lab (? lab rows)))))

(defeg eg--atree "Active-label tree: label only 'closer' rows."
  (let+ (((best lab labels)
          (acquire-train (make-acquire rs))))
    (declare (ignore best))
    (format t "~&;; labels used: ~d~%" labels)
    (show (make-tree lab (? lab rows)))))

(defeg eg--active "20 train/test runs."
  (let+ ((god (make-data rs))
         (w   (wins god))
         (tr  (make-num))
         (ho  (make-num))
         (rn  (make-num)))
    (loop repeat 20 do
      (let+ (((train hold rnd labels)
              (validate rs god w)))
        (add tr train) (add ho hold) (add rn rnd)
        (format t "~&~3d ~3d ~3d ~3d~%"
                train hold rnd labels)))
    (format t "~&;; mean   train=~4,1f hold=~4,1f rnd=~4,1f~%"
            (mid tr) (mid ho) (mid rn))
    (format t "~&;; spread train=~4,1f hold=~4,1f rnd=~4,1f~%"
            (spread tr) (spread ho) (spread rn))))

(defvar *fails* 0)

(defun ok (cond label)
  (format t "~&~a ~a~%" (if cond "PASS" "FAIL") label)
  (unless cond (incf *fails*)))

(defun ck-norm (i)
  (let ((r0 (car (? i rows))))
    (dolist (c (? i cols all))
      (when (typep c 'num)
        (let ((v (norm c (elt r0 (? c at)))))
          (ok (or (eq v '?) (and (<= 0 v) (<= v 1)))
              (format nil "norm ~a in [0,1]"
                      (? c txt))))))))

(defun ck-dist (i)
  (let+ ((r0 (car (? i rows)))
         (r1 (cadr (? i rows))))
    (ok (= (distx i r0 r1) (distx i r1 r0))
        "distx symmetric")
    (ok (zerop (distx i r0 r0)) "distx self=0")))

(defun ck-triangle (i)
  (let ((rs (? i rows)) (n (length (? i rows))) (bad 0))
    (loop repeat 20 do
      (let* ((a (elt rs (rint n)))
             (b (elt rs (rint n)))
             (c (elt rs (rint n))))
        (when (> (distx i a c)
                 (+ (distx i a b) (distx i b c) 1e-9))
          (incf bad))))
    (ok (zerop bad)
        (format nil "triangle ineq (~d viol)" bad))))

(defun ck-disty (i)
  (let ((d (disty i (car (? i rows)))))
    (ok (and (<= 0 d) (<= d 1)) "disty in [0,1]")))

(defun ck-shuffle (i)
  (ok (= (length (shuffle (? i rows)))
         (length (? i rows)))
      "shuffle preserves length"))

(defun ck-sym ()
  (let ((s (make-sym :txt "t")))
    (dolist (v '(a b a c a b)) (add s v))
    (ok (eq 'a (mid s)) "sym mid = mode")))

(defun ck-num ()
  (let ((nm (make-num :txt "t")))
    (dolist (v '(1 2 3 4 5)) (add nm v))
    (ok (< (abs (- (spread nm) (sqrt 5/2))) 1e-6)
        "num spread = sample sd")))

(defun ck-missing ()
  (let ((nm (make-num :txt "t")))
    (dolist (v '(? 1 ? 2 ? 3)) (add nm v))
    (ok (= (? nm n) 3) "num skips '?"))
  (let ((nm (make-num :txt "t")))
    (add nm 5)
    (ok (eq '? (norm nm '?)) "norm '? -> '?")))

(defeg eg--check "Run all invariants; exit 1 on fail."
  (setf *fails* 0)
  (ck-norm i)   (ck-dist i)    (ck-triangle i)
  (ck-disty i)  (ck-shuffle i)
  (ck-sym)      (ck-num)       (ck-missing)
  (format t "~&;; failures: ~d~%" *fails*)
  (when (plusp *fails*) (sb-ext:exit :code 1)))

(cli *the*)
