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
;
; READER MACROS & ANAPHORA
;   !key    -> (second (assoc 'key *the*))
;   $field  -> (slot-value i 'field)   ; i = self
;   (? x a) -> (slot-value x 'a); chains: (? x a b c)
;   (aif t x y)  binds `it` to t's value in x/y

;## Part 1 : Config & Macros -------------------------
(defparameter *the*
  '((p      2            "-p"  "minkowski exponent")
    (file   "auto93.csv" "-f"  "data file")
    (seed   1            "-s"  "seed")
    (budget 50           "-b"  "max labels")
    (few    128          "-u"  "max unlabelled pool")))

(load "lib")

;## Part 2 : TYPES -----------------------------------
(defstruct sym (at 0) (txt " ") (n 0) has)

(defstruct (num (:constructor %make-num))
  (at 0) (txt " ") (n 0) (mu 0) (m2 0) (goal 1))

(defstruct (cols (:constructor %make-cols))
  names x y all)

(defstruct (data (:constructor %make-data))
  rows cols mid)

;## Part 3 : CONSTRUCTORS ----------------------------
(defun make-num (&rest args
                 &aux (i (apply #'%make-num args)))
  (setf $goal (if (eq #\- (ch $txt -1)) 0 1))
  i)

(defun make-cols (txts 
                   &aux (i (%make-cols :names txts)) (n -1))
  (labels 
    ((ako (txt) (if (upper-case-p (ch txt 0))
                  #'make-num #'make-sym))
     (one (txt) (funcall (ako txt) :txt txt :at (incf n)))
     (end (col) (ch (? col txt) -1)))
    (setf $all (mapcar #'one txts))
    (dolist (col $all i)
      (unless (eql (end col) #\X)
        (if (find (end col) "!-+")
          (push col $y)
          (push col $x))))))

(defun make-data (&optional rows)
  (adds (cdr rows) 
        (%make-data :cols (make-cols (car rows)))))

;## Part 4 : ADD -------------------------------------
(defun adds (lst &optional (summary (make-num)))
  "Fold LST into SUMMARY via `add`. Returns SUMMARY."
  (dolist (v lst summary)
    (add summary v)))

(defun sub (it v)
  "Remove V from IT (symmetric with `add`)."
  (add it v -1))

(defmethod add ((i sym) v &optional (w 1))
  (unless (eql v '?)
    (incf $n w)
    (let ((cell (assoc v $has :test #'equal)))
      (if cell
          (incf (cdr cell) w)
          (push (cons v w) $has))))
  v)

(defmethod add ((i num) v &optional (w 1))
  (unless (eq v '?)
    (cond ((and (minusp w) (<= $n 2))
           (setf $n 0 $mu 0 $m2 0))
          (t (incf $n w)
             (let ((delta (- v $mu)))
               (incf $mu (/ (* w delta) $n))
               (incf $m2 (* w delta (- v $mu)))))))
  v)

(defmethod add ((i data) row &optional (w 1))
  (setf $mid nil)
  (add $cols row w)
  (if (plusp w)
      (push row $rows)
      (setf $rows 
            (remove row $rows :test #'equal :count 1)))
  row)

(defmethod add ((i cols) row &optional (w 1))
  (mapcar (lambda (col v) (add col v w)) $all row)
  row)

;## Part 5 : COLUMN STATS ----------------------------
(defmethod mid ((i sym))
  (labels ((most (a b) (if (> (cdr a) (cdr b)) a b)))
    (car (reduce #'most $has))))

(defmethod mid ((i num)) $mu)

(defmethod mid ((i data))
  (or $mid
      (setf $mid (mapcar #'mid (? i cols all)))))

(defmethod spread ((i sym))
  (labels ((fn (p) (- p (log p 2))))
    (loop for (_ . v) in $has sum (fn (/ v $n)))))

(defmethod spread ((i num))
  (if (< $n 2) 0
      (sqrt (/ (max 0 $m2) (1- $n)))))

(defmethod norm ((i sym) v) v)

(defmethod norm ((i num) v)
  (if (eq v '?) v
      (let ((z (/ (- v $mu) (+ (spread i) 1e-32))))
        (/ 1 (+ 1 (exp (* -1.7 (max -3 (min 3 z)))))))))

;## Part 6 : DISTANCE --------------------------------
(defun dist (lst f &aux (d 0))
  (dolist (v lst (expt (/ d (length lst)) (/ 1 !p)))
    (incf d (expt (funcall f v) !p))))

(defun disty (data row &optional (cols (? data cols y)))
  (dist cols 
     (lambda (i) (abs (- (norm i (elt row $at)) $goal)))))

(defmethod distx ((i data) r1 r2)
  (dist (? i cols x) 
     (lambda (i &aux (a (elt r1 $at)) (b (elt r2 $at)))
       (if (and (eq a '?) (eq b '?))
         1
         (distx i a b)))))

(defmethod distx ((i sym) a b)
  (if (equal a b) 0 1))

(defmethod distx ((i num) a b
                  &aux (a (norm i a)) (b (norm i b)))
  (cond ((eq a '?) (abs (- b (if (> b 0.5) 0 1))))
        ((eq b '?) (abs (- a (if (> a 0.5) 0 1))))
        (t         (abs (- a b)))))

; ## Part 7 : Trees (grow) ----------------------------
(defstruct (tree (:constructor %make-tree)
                 (:copier nil))
  d ynum col cut left right hdr)

(defmethod go-left? ((i sym) v cut)
  (or (eq v '?) (equal v cut)))

(defmethod go-left? ((i num) v cut)
  (or (eq v '?) (<= v cut)))

(defmethod %cuts ((i sym) rs)
  (declare (ignore rs))
  (mapcar #'car $has))

(defmethod %cuts ((i num) rs)
  (let ((vs (loop for r in rs for v = (elt r $at)
                  unless (eq v '?) collect v)))
    (and vs
         (list (elt (sort vs #'<)
                    (floor (length vs) 2))))))

(defun make-tree (d rs &optional (hdr ""))
  "Tree node: cloned data, num of distys, header."
  (%make-tree
    :d    (make-data (cons (? d cols names) rs))
    :ynum (adds (mapcar (lambda (r) (disty d r)) rs))
    :hdr  hdr))

(defun tree-split (d c cut rs)
  "Returns (score c cut left-rs right-rs)."
  (let (l r (ln (make-num)) (rn (make-num)))
    (dolist (row rs)
      (if (go-left? c (elt row (? c at)) cut)
          (progn (push row l) (add ln (disty d row)))
          (progn (push row r) (add rn (disty d row)))))
    (list (+ (* (? ln n) (spread ln))
             (* (? rn n) (spread rn)))
          c cut l r)))

(defun tree-splits (d rs leaf)
  "Splits where both sides have >= LEAF rows."
  (loop for c in (? d cols x)
        append (loop for cut in (%cuts c rs)
                     for s = (tree-split d c cut rs)
                     when (>= (min (length (fourth s))
                                   (length (fifth s)))
                              leaf)
                     collect s)))

(defun kid-hdr (c cut left?)
  "Display header for a child branch."
  (format nil "~a ~a ~a" (? c txt)
          (if left?
              (if (typep c 'sym) "==" "<=")
              (if (typep c 'sym) "!=" ">"))
          cut))

(defun tree-grow (d rs &optional (leaf 3) (hdr ""))
  "Grow tree; stop when node has < 2*LEAF rows."
  (let ((t0 (make-tree d rs hdr)))
    (when (>= (length rs) (* 2 leaf))
      (let ((splits (tree-splits d rs leaf)))
        (when splits
          (let* ((best (extremum-by splits #'first #'<))
                 (c (second best)) (cut (third best)))
            (setf
              (? t0 col) c
              (? t0 cut) cut
              (? t0 left)  
                   (tree-grow d (fourth best) leaf
                                (kid-hdr c cut t))
              (? t0 right) 
                    (tree-grow d (fifth best) leaf
                                 (kid-hdr c cut nil)))))))
    t0))

; ## Part 7 : Trees (use) ----------------------------
(defun tree-leaf (t0 r)
  "Route R to its leaf."
  (if (null (? t0 left)) t0
      (tree-leaf
        (if (go-left? (? t0 col)
                      (elt r (? t0 col at))
                      (? t0 cut))
            (? t0 left) (? t0 right))
        r)))

(defun tree-show (t0 &optional (lvl 0))
  "mu, n on LHS; indent + condition on RHS."
  (format t "~&~4,2f (~3d)   ~{~A~}~a~%"
          (mid (? t0 ynum)) (? (? t0 ynum) n)
          (loop repeat (max 0 (1- lvl))
                collect "|.. ")
          (? t0 hdr))
  (when (? t0 left)
    (tree-show (? t0 left)  (1+ lvl))
    (tree-show (? t0 right) (1+ lvl))))

; ## Part 8 : ACTIVE LEARNING -------------------------
(defun wins (god)
  "Score 0..100 against god truth: 100=best, 50=med."
  (let* ((ys (mapcar (lambda (r) (disty god r))
                     (? god rows)))
         (ds (sort ys #'<))
         (lo (first ds))
         (md (elt ds (floor (length ds) 2))))
    (lambda (r)
      (floor (* 100 (- 1 (/ (- (disty god r) lo)
                            (+ (- md lo) 1e-32))))))))

(defun extremum-by (lst key cmp)
  "Argmin/argmax of LST under KEY, ordered by CMP."
  (let* ((best (car lst)) (m (funcall key best)))
    (dolist (x (cdr lst) best)
      (let ((k (funcall key x)))
        (when (funcall cmp k m) (setf best x m k))))))

(defun rebalance (best rest lab)
  "Cap best at sqrt(|lab|): move worst row to rest."
  (when (> (length (? best rows))
           (sqrt (length (? lab rows))))
    (let ((bad (extremum-by
                 (? best rows)
                 (lambda (r) (disty lab r))
                 #'>)))
      (sub best bad) (add rest bad))))

(defun closer? (lab best rest r)
  "Is R closer to best's centroid than rest's?"
  (< (distx lab r (mid best))
     (distx lab r (mid rest))))

(defun warm-start (rs lab best rest ys
                   &aux (start 4))
  "Label 4 rows; split by disty into best vs rest."
  (dotimes (_ (min start (length rs)))
    (let ((r (pop rs)))
      (add lab r) (add ys (disty lab r))))
  (let* ((sorted (sort (copy-list (? lab rows)) #'<
                       :key (lambda (r)
                              (disty lab r))))
         (n (max 1
                 (floor (sqrt (length sorted))))))
    (dolist (r (subseq sorted 0 n)) (add best r))
    (dolist (r (subseq sorted n))   (add rest r)))
  rs)

(defun active (rs &aux (hd (list (car rs))))
  (let* ((lab  (make-data hd))
         (best (make-data hd))
         (rest (make-data hd))
         (ys   (make-num))
         (pool (warm-start (shuffle (cdr rs))
                           lab best rest ys)))
    (loop for r in (subseq pool 0
                           (min !few (length pool)))
          while (< (? ys n) !budget) do
          (when (closer? lab best rest r)
            (add ys (disty lab r))
            (add lab r) (add best r)
            (rebalance best rest lab)))
    (values best lab (? ys n))))

(defun validate (rs god w &optional (check 5))
  "Run active; grow tree; rank holdout; pay CHECK."
  (let* ((body (shuffle (cdr rs)))
         (n (floor (length body) 2))
         (train (cons (car rs) (subseq body 0 n)))
         (test  (subseq body n)))
    (multiple-value-bind (best lab labels)
        (active train)
      (declare (ignore best))
      (let* ((y-god (lambda (r) (disty god r)))
             (train-best
              (extremum-by (? lab rows) y-god #'<))
             (tr (tree-grow lab (? lab rows)))
             (ranked
              (sort (copy-list test) #'<
                    :key (lambda (r)
                           (mid (? (tree-leaf tr r)
                                   ynum)))))
             (top  (subseq ranked 0
                           (min check (length ranked))))
             (pick (extremum-by top y-god #'<)))
        (values (funcall w train-best)
                (funcall w pick)
                (+ labels check))))))

; ## Part 9 : EXAMPLES --------------------------------
(defun %stride (rows key &optional (n 30))
  "Sort ROWS by KEY; print every Nth row."
  (loop for x in (sort rows #'< :key key)
    by (lambda (l) (nthcdr n l)) do (print x)))

(defun eg--all (&optional (arg !file))
  "Run every eg--* function except eg--all itself."
  (do-symbols (s *package*)
    (unless (eq s 'eg--all) (run s arg))))

(defun eg-s (&optional (seed !seed))
  "Set seed (both *seed* and !seed)."
  (setf *seed* seed  !seed  seed))

(defun eg--the (_)
  "Print *the* (command-line defaults)."
  (print *the*))

(defun eg--rows (&optional (file !file))
  "Read the CSV, print its tail."
  (let ((rows (read-csv file)))
    (print (subseq rows 380))))

(defun eg--data (&optional (file !file))
  "Build a Data object, print its y-columns."
  (let ((i (make-data (read-csv file))))
    (print (? i cols y))))

(defun eg--stats (&optional (file !file))
  "Print mid, spread per column."
  (let ((i (make-data (read-csv file))))
    (dolist (c (? i cols all))
      (format t "~&~12a  mid=~a  spread=~a~%"
              (? c txt) (mid c) (spread c)))))

(defun eg--norm (&optional (file !file))
  "One row: raw vs normalized for every y-column."
  (let* ((i (make-data (read-csv file)))
         (r (car $rows)))
    (dolist (c (? i cols y))
      (let ((v (elt r (? c at))))
        (format t "~&~8a  raw=~a  norm=~a~%"
                (? c txt) v (norm c v))))))

(defun eg--ydata (&optional (file !file))
  "Sort rows by disty; stride-print."
  (let ((i (make-data (read-csv file))))
    (%stride $rows (lambda (r) (disty i r)))))

(defun eg--xdata (&optional (file !file))
  "Sort rows by distx from row 0; stride-print."
  (let* ((i (make-data (read-csv file)))
         (r0 (car $rows)))
    (%stride $rows (lambda (r) (distx i r0 r)))))

(defun eg--tree (&optional (file !file))
  "Active-label, grow tree, pretty-print it."
  (let ((rs (read-csv file)))
    (multiple-value-bind (best lab labels) (active rs)
      (declare (ignore best))
      (format t "~&;; labels used: ~d~%" labels)
      (tree-show (tree-grow lab (? lab rows))))))

(defun eg--active (&optional (file !file))
  "20 train/test runs; print: train hold labels."
  (let* ((rs (read-csv file))
         (god (make-data rs))
         (w (wins god)))
    (loop repeat 20 do
      (multiple-value-bind (train hold labels)
          (validate rs god w)
        (format t "~&~3d ~3d ~3d~%"
                train hold labels)))))

; ## Last : -------------------------------------------
(cli *the*)
