#!/usr/bin/env -S sbcl --script
; vim: set ft=lisp ts=2 sw=2 et :


; fri.lisp -- data-lite active learning, one file.
; (c) 2026 Tim Menzies, timm@ieee.org, MIT license.
;
; USAGE
;   clisp fri.lisp --cmd [arg]  [-flag value]...
;   sbcl --script fri.lisp --cmd [arg] [-flag value]...
;
; CMDS  (--all runs every --xxx command)
;   --the     print config *the*
;   --rows    read CSV, print some rows
;   --data    build Data, show y-columns
;   --stats   print mid, spread per column
;   --norm    show raw vs normalized y of row 0
;   --ydata   sort rows by goal-distance; stride-print
;   --xdata   sort rows by feature-distance from row 0
;   --tree    active-label, grow tree, pretty-print it
;   --active  20 train/test splits; train hold labels
;
; OPTIONS
;   -p N   Minkowski distance exponent     default 2
;   -f F   CSV file                        auto93.csv
;   -s N   random seed                     default 1
;   -b N   label budget                    default 50
;   -u N   unlabelled pool cap             default 128
;
; CSV INPUT. Header row names columns; suffix sets role.
;   [A-Z]*   numeric            [a-z]*   symbolic
;   [A-Z]*-  minimize (y)       [A-Z]*+  maximize (y)
;   *!       class column (y)   *X       skip column
;   ?        missing value
;
; NAMING: r/rs=row(s), d=data, c=column, i=self,
;         ys=Num of distys.

(load "plus")

(defparameter *the*
 '((p      2            "-p"  "minkowski exponent")
   (file   "auto93.csv" "-f"  "data file")
   (seed   1            "-s"  "seed")
   (budget 50           "-b"  "max labels")
   (few    128          "-u"  "max unlabelled pool")))

(setf *seed* @seed)

;  _      ._ _
; _>  \/  | | |
;     /

(plus sym "Symbolic column: count + frequency table."
  ((at 0) (txt " ") (n 0) has)

  (add (v &optional (w 1))
    "Fold V (with weight W) into frequency table."
    (unless (eql v '?)
      (incf $n w)
      (let ((cell (assoc v $has :test #'equal)))
        (if cell
          (incf (cdr cell) w)
          (push (cons v w) $has))))
    v)

  (mid () "Mode."
    (car (reduce (ff+ (if (> (cdr _) (cdr __)) _ __)) $has)))

  (spread () "Entropy."
    (loop for (_ . v) in $has
      sum (let ((p (/ v $n))) (- p (log p 2)))))

  (norm    (v)     v)
  (distx   (a b)   (if (equal a b) 0 1))
  (go-left? (v cut) (or (eq v '?) (equal v cut)))
  (%cuts   (rs)    (declare (ignore rs)) (mapcar #'car $has))
  (ops     ()      '("==" "!=")))

; ._        ._ _
; | |  |_|  | | |

(plus num "Numeric column: Welford count, mean, m2, goal."
  ((at 0) (txt " ") (n 0) (mu 0) (m2 0) (goal 1))

  (make ()
    "Build a num; goal=0 if txt ends in '-', else 1."
    (setf $goal (if (eq #\- (ch $txt -1)) 0 1)))

  (add (v &optional (w 1))
    "Fold V (weight W) into running mean+m2."
    (unless (eq v '?)
      (cond ((and (minusp w) (<= $n 2))
              (setf $n 0 $mu 0 $m2 0))
        (t (incf $n w)
          (let ((delta (- v $mu)))
            (incf $mu (/ (* w delta) $n))
            (incf $m2 (* w delta (- v $mu)))))))
    v)

  (mid () $mu)

  (spread () (if (< $n 2) 0
               (sqrt (/ (max 0 $m2) (1- $n)))))

  (norm (v) (if (eq v '?) v
              (let ((z (/ (- v $mu) (+ (spread i) 1e-32))))
                (/ 1
                  (+ 1 (exp (* -1.7 (max -3 (min 3 z)))))))))

  (distx (a b)
    "Normalized abs diff with missing-value handling."
    (let ((a (norm i a)) (b (norm i b)))
      (cond ((eq a '?) (abs (- b (if (> b 0.5) 0 1))))
        ((eq b '?) (abs (- a (if (> a 0.5) 0 1))))
        (t         (abs (- a b))))))

  (go-left? (v cut) (or (eq v '?) (<= v cut)))

  (%cuts (rs) "Median as the lone cut."
    (let ((vs (loop for r in rs for v = (elt r $at)
                unless (eq v '?) collect v)))
      (and vs (list (elt (sort vs #'<) (floor (length vs) 2))))))

  (ops () '("<=" ">")))

;  _   _   |   _
; (_  (_)  |  _>

(plus cols "Header-derived collection: names, x-cols, y-cols, all."
  (names x y all)

  (make (txts &aux (i (%make-cols :names txts)) (n -1))
    "Parse header TXTS; partition columns into x/y/skip."
    (let+ ((one (txt) (! (if (upper-case-p (ch txt 0))
                           #'make-num #'make-sym)
                        :txt txt :at (incf n)))
           (end (col) (ch (? col txt) -1)))
      (setf $all (mapcar #'one txts))
      (dolist (col $all)
        (unless (eql (end col) #\X)
          (if (find (end col) "!-+")
            (push col $y)
            (push col $x))))))

  (add (row &optional (w 1))
    (mapcar (ff+ (add _ __ w)) $all row)
    row))

;  _|   _.  _|_   _.
; (_|  (_|   |_  (_|

(plus data "Table: rows + their parsed columns + cached row-mid."
  (rows cols mid)

  (make (&optional rows &aux (i (%make-data :cols (make-cols (car rows)))))
    "First row = header."
    (adds (cdr rows) i))

  (add (row &optional (w 1))
    "W=-1 removes ROW."
    (setf $mid nil)
    (add $cols row w)
    (if (plusp w)
      (push row $rows)
      (setf $rows (remove row $rows :test #'equal :count 1)))
    row)

  (mid () "Cached centroid: per-column mid."
    (or $mid (setf $mid (mapcar #'mid (? i cols all)))))

  (distx (r1 r2)
    (dist (? i cols x)
      (lambda (i &aux (a (elt r1 $at)) (b (elt r2 $at)))
        (if (and (eq a '?) (eq b '?)) 1 (distx i a b))))))

(defun data-disty (i row &optional (cols (? i cols y)))
  "0 = perfect, 1 = worst."
  (dist cols
    (lambda (i) (abs (- (norm i (elt row $at)) $goal)))))

(defun data-wins (i)
  "Closure: score row 0..100 vs this data; 100=best, 50=med."
  (let+ ((ys (mapcar (f+ (data-disty i _)) $rows))
         (ds (sort ys #'<))
         (lo (first ds))
         (md (elt ds (floor (length ds) 2))))
    (f+ (floor (* 100 (- 1 (/ (- (data-disty i _) lo)
                              (+ (- md lo) 1e-32))))))))

; _|_  ._   _    _
;  |_  |   (/_  (/_

(plus tree "Decision tree: built recursively by make-tree."
  (data ynum col cut left right hdr)

  (:opts ((:copier nil)))

  (make (data rs &optional (leaf 3) (hdr "")
           &aux (i (%make-tree
                     :data (make-data
                             (cons (? data cols names) rs))
                     :ynum (adds (mapcar (f+ (data-disty data _)) rs))
                     :hdr  hdr)))
    "Build node from RS; recurse on best split if big enough."
    (when (>= (length rs) (* 2 leaf))
      (let ((cands (tree-splits data rs leaf)))
        (when cands
          (let+ ((best (%extremum-by cands #'first #'<))
                  (c (second best)) (cut (third best)))
            (setf $col c $cut cut
              $left  (make-tree data (fourth best) leaf
                       (tree-kid-hdr c cut t))
              $right (make-tree data (fifth best) leaf
                       (tree-kid-hdr c cut nil)))))))))

(defun tree-leaf (i r)
  (if (null (? i left)) i
    (tree-leaf (if (go-left? (? i col) (elt r (? i col at)) (? i cut))
                 (? i left) (? i right))
      r)))

(defun tree-show (i &optional (lvl 0))
  "Pretty-print: mu, n on LHS; indent + condition on RHS."
  (format t "~&~4,2f (~3d)   ~{~A~}~a~%"
    (mid (? i ynum)) (? i ynum n)
    (loop repeat (max 0 (1- lvl))
      collect "|.. ")
    (? i hdr))
  (when (? i left)
    (tree-show (? i left)  (1+ lvl))
    (tree-show (? i right) (1+ lvl))))

(defun tree-kid-hdr (c cut left?)
  (destructuring-bind (l r) (ops c)
    (format nil "~a ~a ~a" (? c txt) (if left? l r) cut)))

(defun tree-split (data c cut rs)
  "Returns (score c cut left-rs right-rs)."
  (let (l r (ln (make-num)) (rn (make-num)))
    (dolist (row rs)
      (if (go-left? c (elt row (? c at)) cut)
          (progn (push row l) (add ln (data-disty data row)))
          (progn (push row r) (add rn (data-disty data row)))))
    (list (+ (* (? ln n) (spread ln)) (* (? rn n) (spread rn)))
          c cut l r)))

(defun tree-splits (data rs leaf)
  "Splits where both sides have at least LEAF rows."
  (loop for c in (? data cols x)
    append (loop for cut in (%cuts c rs)
             for s = (tree-split data c cut rs)
             when (>= (min (length (fourth s))
                        (length (fifth s))) leaf)
             collect s)))

;  _.   _   _.       o  ._   _
; (_|  (_  (_|  |_|  |  |   (/_
;            |

(plus acquire "Active learner: lab, best, rest, ys, pool."
    (lab best rest ys pool)

  (make (rs &aux (hd (list (car rs)))
               (i  (%make-acquire
                     :lab  (make-data hd)
                     :best (make-data hd)
                     :rest (make-data hd)
                     :ys   (make-num)
                     :pool (shuffle (cdr rs)))))
    "Init from RS (first row = header). Warm-start 4 labels."
    (acquire-warm-start i)))

(defun acquire-warm-start (i &aux (start 4))
  "Label 4 rows; split by disty into best vs rest."
  (dotimes (_ (min start (length $pool)))
    (let ((r (pop $pool)))
      (add $lab r) (add $ys (data-disty $lab r))))
  (let+ ((sorted (sortby (f+ (data-disty $lab _)) (? $lab rows)))
         (n (max 1 (floor (sqrt (length sorted))))))
    (dolist (r (subseq sorted 0 n)) (add $best r))
    (dolist (r (subseq sorted n))   (add $rest r))))

(defun acquire-closer? (i r)
  (< (distx $lab r (mid $best)) (distx $lab r (mid $rest))))

(defun acquire-rebalance (i)
  "Cap best at sqrt(|lab|); worst row -> rest."
  (when (> (length (? $best rows))
           (sqrt (length (? $lab rows))))
    (let ((bad (%extremum-by (? $best rows)
                             (f+ (data-disty $lab _)) #'>)))
      (sub $best bad) (add $rest bad))))

(defun acquire-train (i)
  "Run loop; return best, lab, label-count."
  (loop for r in (subseq $pool 0 (min @few (length $pool)))
        while (< (? $ys n) @budget) do
        (when (acquire-closer? i r)
          (add $ys (data-disty $lab r))
          (add $lab r) (add $best r)
          (acquire-rebalance i)))
  (values $best $lab (? $ys n)))

(defun validate (rs god w &optional (check 5))
  "Run active; grow tree; rank holdout; pay CHECK."
  (let+ ((body  (shuffle (cdr rs)))
         (n     (floor (length body) 2))
         (train (cons (car rs) (subseq body 0 n)))
         (test  (subseq body n))
         ((best lab labels) (acquire-train (make-acquire train)))
         (y-god (f+ (data-disty god _)))
         (train-best (%extremum-by (? lab rows) y-god #'<))
         (tr (make-tree lab (? lab rows)))
         (ranked (sortby (f+ (mid (? (tree-leaf tr _) ynum))) test))
         (top  (subseq ranked 0 (min check (length ranked))))
         (pick (%extremum-by top y-god #'<)))
    (declare (ignore best))
    (values (! w train-best)
            (! w pick)
            (+ labels check))))

;  _        _.  ._ _   ._   |   _    _
; (/_  ><  (_|  | | |  |_)  |  (/_  _>
;                      |

(defun eg--the (&optional _) (declare (ignore _)) (print *the*))

(defeg eg--rows  "Print CSV tail."  (print (subseq rs 380)))
(defeg eg--data  "Show y-columns."  (print (? i cols y)))
(defeg eg--ydata "Stride by disty." (%stride $rows (f+ (data-disty i _))))

(defeg eg--xdata "Stride by distx from row 0."
  (let ((r0 (car $rows))) (%stride $rows (f+ (distx i r0 _)))))

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

(defeg eg--tree "Active-label, grow tree, show."
  (let+ (((best lab labels) (acquire-train (make-acquire rs))))
    (declare (ignore best))
    (format t "~&;; labels used: ~d~%" labels)
    (tree-show (make-tree lab (? lab rows)))))

(defeg eg--active "20 train/test runs."
  (let+ ((god (make-data rs))
         (w   (data-wins god)))
    (loop repeat 20 do
      (let+ (((train hold labels) (validate rs god w)))
        (format t "~&~3d ~3d ~3d~%" train hold labels)))))

(cli *the*)
