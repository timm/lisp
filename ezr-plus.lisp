;; vim: ft=lisp ts=2 sw=2 et tw=72 :
;;
;; ezr-plus.lisp -- ezr active learning. Logic in plus forms.
;; Tree + naive Bayes + active learning + wins + metrics.
;; (c) 2026 Tim Menzies <timm@ieee.org>  MIT.
;;
;; Run from this directory:
;;   sbcl --script ezr-plus.lisp --tree
;;   sbcl --script ezr-plus.lisp --nb
;;   sbcl --script ezr-plus.lisp --acquire
;; Override flags:  ... -f data.csv -s 42 -b 30

(load "plus.lisp")

;; ## Options
(defvar *the* '(
  (seed   1234567891 "-s" "random seed")
  (file   "auto93.csv" "-f" "csv file")
  (k      1   "-k" "Laplace")
  (m      2   "-m" "Bayes m")
  (p      2   "-p" "Minkowski p")
  (leaf   4   "-l" "min leaf rows")
  (maxd   8   "-d" "max tree depth")
  (few    128 "-F" "train pool cap")
  (start  4   "-S" "warm-start labels")
  (budget 50  "-b" "labels to acquire")
  (check  5   "-c" "top-k verify")
  (Show   40  "-W" "tree label width")))

(defun safe/ (a b) (if (zerop b) 0 (/ a b)))

;; ## SYM

(plus sym "frequency counter"
  ((n 0) (at 0) (txt " ")
   (has (make-hash-table :test 'equal)))

  (add (v)
    (unless (eq v '?)
      (incf $n)
      (incf (gethash v $has 0)))
    v)

  (mid (&aux best (bn -1))
    (maphash (ff+ (when (> __ bn) (setf best _ bn __))) $has)
    best)

  (div (&aux (s 0))
    (maphash (ff+ (let+ ((p (/ __ $n)))
                    (incf s (* p (log p 2)))))
             $has)
    (- s))

  (norm (v) v)

  (xdist (u v)
    (cond ((and (eq u '?) (eq v '?)) 1)
          (t (if (equal u v) 0 1))))

  (like (v)
    (if (zerop $n) 1
        (/ (1+ (gethash v $has 0))
           (+ $n (max 1 (hash-table-count $has))))))

  (cut-label (cut lo?)
    (format nil "~a ~a ~a" $txt
            (if lo? "==" "!=") cut)))

;; ## NUM

(plus num "running mean/sd/lo/hi"
  ((n 0) (at 0) (txt " ") (mu 0) (m2 0)
   (lo 1d32) (hi -1d32) (goal 1))

  (make ()
    (when (and (stringp $txt) (plusp (length $txt))
               (eql (ch $txt -1) #\-))
      (setf $goal 0)))

  (add (v)
    (unless (eq v '?)
      (incf $n)
      (setf $lo (min v $lo) $hi (max v $hi))
      (let+ ((d (- v $mu)))
        (incf $mu (/ d $n))
        (incf $m2 (* d (- v $mu)))))
    v)

  (mid () $mu)

  (div ()
    (if (< $n 2) 0 (sqrt (/ (max 0 $m2) (1- $n)))))

  (norm (v)
    (if (eq v '?) 0.5
        (let+ ((sd (div i)))
          (if (zerop sd) 0.5
              (/ 1.0 (1+ (exp (* -1.7 (/ (- v $mu) sd)))))))))

  (xdist (u v)
    (cond ((and (eq u '?) (eq v '?)) 1)
          (t (let+ ((nu (and (not (eq u '?)) (norm i u)))
                    (nv (and (not (eq v '?)) (norm i v))))
               (when (null nu) (setf nu (if (> nv 0.5) 0 1)))
               (when (null nv) (setf nv (if (> nu 0.5) 0 1)))
               (abs (- nu nv))))))

  (like (v)
    (let+ ((sd (div i)))
      (if (or (zerop sd) (eq v '?)) 0
          (let+ ((z (/ (- v $mu) sd)))
            (/ (exp (* -0.5 z z))
               (* sd 2.5066282746310002))))))

  (cut-label (cut lo?)
    (format nil "~a ~a ~a" $txt
            (if lo? "<=" "> ")
            (if (and (numberp cut) (= cut (floor cut)))
                (format nil "~d" (floor cut))
                cut))))

;; ## COLS

(plus cols "x/y col splits from header row"
  (x y all names klass)

  (make ()
    (let+ ((at -1))
      (dolist (txt $names)
        (let+ ((zz  (ch txt -1))
               (col (if (upper-case-p (ch txt 0))
                        (new+ num :txt txt :at (incf at))
                        (new+ sym :txt txt :at (incf at)))))
          (push col $all)
          (cond ((eql zz #\X))
                ((eql zz #\!) (setf $klass col) (push col $y))
                ((member zz '(#\- #\+)) (push col $y))
                (t (push col $x)))))
      (setf $all (nreverse $all)
            $x   (nreverse $x)
            $y   (nreverse $y)))))

;; ## DATA  (most ML logic lives here)

(plus data "rows + col stats; ML methods"
  (rows cols)

  (make (&optional rs &aux (i (%make-data)))
    (dolist (row rs) (add i row)))

  (add (row)
    (cond ((null $cols)
           (setf $cols (new+ cols :names row)))
          (t (push row $rows)
             (mapc (ff+ (add _ __)) (slot-value $cols 'all) row))))

  (clone (&optional rs)
    (let+ ((d (new+ data)))
      (setf (data-cols d)
            (new+ cols :names (slot-value $cols 'names)))
      (dolist (r rs d) (add d r))))

  ;; --- distances ---
  (disty (row)
    (let+ ((ys (? i cols y)))
      (expt (/ (loop for c in ys
                     sum (expt (- (norm c (nth (? c at) row))
                                  (? c goal))
                               @p))
               (length ys))
            (/ 1 @p))))

  (distx (r1 r2)
    (let+ ((xs (? i cols x)))
      (expt (/ (loop for c in xs
                     sum (expt (xdist c (nth (? c at) r1)
                                        (nth (? c at) r2))
                               @p))
               (length xs))
            (/ 1 @p))))

  (centroid (&aux (all (? i cols all))
                  (row (make-list (length all))))
    (dolist (c all row)
      (setf (nth (? c at) row) (mid c))))

  ;; --- y-side stats over a row set ---
  (ycol (rows)
    (let+ ((klass (? i cols klass))
           (s (new+ sym))
           (y (new+ num)))
      (cond (klass (dolist (r rows s)
                     (add s (nth (? klass at) r))))
            (t     (dolist (r rows y)
                     (add y (disty i r)))))))

  (spread (rows)
    (* (length rows) (div (ycol i rows))))

  (y-mids (rows)
    (loop for c in (? i cols y)
          collect (let+ ((cc (if (typep c 'num)
                                 (new+ num) (new+ sym)))
                         (at (? c at)))
                    (dolist (r rows) (add cc (nth at r)))
                    (mid cc))))

  ;; --- best-split helpers ---
  (col-vals (col rows &aux (at (? col at)))
    (for+ (nth at r) for r in rows
          if (not (eq (nth at r) '?))))

  (cuts-for (col rows)
    (if (typep col 'sym)
        (remove-duplicates (col-vals i col rows) :test #'equal)
        (let+ ((vs (sort (col-vals i col rows) #'<)))
          (when (>= (length vs) 2)
            (list (nth (floor (length vs) 2) vs))))))

  (split-rows (col cut rows
               &aux (at (? col at))
                    (sym (typep col 'sym))
                    lo hi)
    (dolist (r rows (values lo hi))
      (let+ ((x (nth at r)))
        (if (or (eq x '?) (if sym (equal x cut) (<= x cut)))
            (push r lo) (push r hi)))))

  (score-cut (col cut rows)
    (let+ (((lo hi) (split-rows i col cut rows)))
      (when (and lo hi
                 (>= (length lo) @leaf)
                 (>= (length hi) @leaf))
        (values (+ (spread i lo) (spread i hi)) lo hi))))

  (best-cut (rows &aux (best 1d30) bc bcut blo bhi)
    (dolist (col (? i cols x) (values bc bcut blo bhi))
      (dolist (cut (cuts-for i col rows))
        (let+ (((s lo hi) (score-cut i col cut rows)))
          (when (and s (< s best))
            (setf best s bc col bcut cut blo lo bhi hi))))))

  ;; --- tree training ---
  (tree (rows &optional (dep 0) (label "ROOT"))
    (let+ ((n (new+ node :dep dep :label label
                         :nrows (length rows)
                         :mu (mid (ycol i rows))
                         :ymids (y-mids i rows))))
      (cond
        ((or (< (length rows) (* 2 @leaf)) (>= dep @maxd)) n)
        (t (let+ (((col cut lo hi) (best-cut i rows)))
             (cond ((null col) n)
                   (t (grow n i col cut lo hi) n))))))))

;; ## NODE

(plus node "tree node"
  (col cut kids ymids (dep 0) (mu 0) (nrows 0)
       (label "ROOT") (kind 'leaf))

  (find-leaf (row)
    (if (eq $kind 'leaf) i
        (let+ ((c   $col)
               (x   (nth (? c at) row))
               (sym (typep c 'sym))
               (lo? (cond ((eq x '?) t)
                          (sym (equal x $cut))
                          (t   (<= x $cut)))))
          (find-leaf (nth (if lo? 0 1) $kids) row))))

  (grow (d col cut lo hi)
    (setf $kind 'branch $col col $cut cut
          $kids (list (tree d lo (1+ $dep)
                            (cut-label col cut t))
                      (tree d hi (1+ $dep)
                            (cut-label col cut nil)))))

  (show (d)
    (when (zerop $dep) (tree-header d))
    (let+ ((pad (apply #'concatenate 'string
                  (loop repeat (max 0 (1- $dep))
                        collect "|.. "))))
      (format t "~vA ~6,3F ~5D" @Show
              (concatenate 'string pad $label)
              $mu $nrows)
      (dolist (m $ymids) (format t " ~8,2F" m))
      (format t "~%"))
    (when (eq $kind 'branch)
      (dolist (k $kids) (show k d)))))

(plus+ data
  (tree-header ()
    (format t "~vA ~6A ~5A" @Show "rule" "d2h" "n")
    (dolist (c (slot-value $cols 'y))
      (format t " ~8A" (slot-value c 'txt)))
    (format t "~%"))

  (pool-of ()
    (let+ ((rs (shuffle $rows)))
      (subseq rs 0 (min @few (length rs)))))

  (centroids-of (lab)
    (let+ ((sorted (sort (copy-list lab) #'<
                         :key (f+ (disty i _))))
           (sqn    (max 1 (isqrt (length sorted))))
           (best   (subseq sorted 0 sqn))
           (rest   (subseq sorted sqn)))
      (values (centroid (clone i best))
              (centroid (clone i rest)))))

  (pick-next (unlab mb mr)
    (%extremum-by unlab
      (f+ (- (distx i _ mb) (distx i _ mr))) #'<))

  (acquire ()
    (let+ ((pool  (pool-of i))
           (lab   (subseq pool 0 (min @start (length pool))))
           (unlab (subseq pool (length lab))))
      (dotimes (_ @budget lab)
        (when (null unlab) (return lab))
        (let+ (((mb mr) (centroids-of i lab))
               (pick    (pick-next i unlab mb mr)))
          (push pick lab)
          (setf unlab (remove pick unlab :test #'eq)))))))

;; ## NB

(plus nb "naive Bayes"
  (klass-col cols-by-class (total 0) priors)

  (make (d &aux (i (%make-nb)))
    (setf $klass-col (? d cols klass)))

  (train (d row)
    (let+ ((k (nth (? $klass-col at) row)))
      (incf $total)
      (incf (getf $priors k 0))
      (let+ ((cs (getf $cols-by-class k)))
        (unless cs
          (setf cs (mapcar (f+ (cond ((eq _ $klass-col) nil)
                                     ((typep _ 'num)
                                      (new+ num :at (? _ at)))
                                     (t (new+ sym :at (? _ at)))))
                           (? d cols all))
                (getf $cols-by-class k) cs))
        (mapc (ff+ (when _ (add _ (nth (? _ at) row))))
              cs row))))

  (pred (row &aux best blbl)
    (loop for (k cnt) on $priors by #'cddr
          for cs = (getf $cols-by-class k)
          for ll = (log (/ (+ cnt @k) (+ $total (* @k 2))))
          do (dolist (c cs)
               (when c
                 (let+ ((v (like c (nth (? c at) row))))
                   (when (> v 1d-30) (incf ll (log v))))))
             (when (or (null best) (> ll best))
               (setf best ll blbl k)))
    blbl))

;; ## WINS

(plus wins "percentile scorer (0..100)"
  ((lo 0) (med 0) (sd 0))

  (make (d rows &aux (i (%make-wins)))
    (let+ ((xs (sort (mapcar (f+ (disty d _)) rows) #'<))
           (n  (length xs)))
      (when (>= n 2)
        (let+ ((ten (max 1 (floor n 10))))
          (setf $lo  (first xs)
                $med (nth (floor (1+ n) 2) xs)
                $sd  (/ (- (nth (- n ten) xs)
                           (nth (1- ten) xs))
                        2.56))))))

  (score (d row)
    (let+ ((x (disty d row)))
      (when (< x (+ $lo (* 0.35 $sd))) (setf x $lo))
      (max -100
           (floor (* 100 (- 1 (/ (- x $lo)
                                 (+ (- $med $lo) 1d-32)))))))))

;; ## METRICS

(plus metrics "per-class pd/pf/prec/acc"
  ((cm   (make-hash-table :test 'equal))
   (seen (make-hash-table :test 'equal))
   (n 0))

  (tally (pred actual)
    (incf $n)
    (incf (gethash (cons pred actual) $cm 0))
    (setf (gethash pred $seen) t
          (gethash actual $seen) t))

  (counts (c classes &aux (tp 0) (fn 0) (fp 0))
    (dolist (a classes)
      (dolist (p classes)
        (let+ ((cnt (gethash (cons p a) $cm 0)))
          (cond ((and (equal a c) (equal p c)) (incf tp cnt))
                ((equal a c) (incf fn cnt))
                ((equal p c) (incf fp cnt))))))
    (values tp fn fp (- $n tp fn fp)))

  (say ()
    (let+ ((classes (loop for k being the hash-keys of $seen
                          collect k)))
      (format t "~5A ~6A ~6A ~6A ~6A  ~A~%"
              "n" "pd" "pf" "prec" "acc" "class")
      (dolist (c classes)
        (let+ (((tp fn fp tn) (counts i c classes))
               (pd   (safe/ tp (+ tp fn)))
               (pf   (safe/ fp (+ fp tn)))
               (prec (safe/ tp (+ tp fp)))
               (acc  (safe/ (+ tp tn) $n)))
          (format t "~5D ~6,3F ~6,3F ~6,3F ~6,3F  ~A~%"
                  (+ tp fn) pd pf prec acc c))))))

;; ## eg-- entry points

(defun eg--csv ()
  (dolist (r (read-csv @file)) (print r)))

(defun eg--the () (print *the*))

(defun eg--tree (&optional (file @file))
  (let+ ((d (make-data (read-csv file))))
    (show (tree d (data-rows d)) d)))

(defun eg--nb (&optional (file @file))
  (let+ ((d        (make-data (read-csv file)))
         (rows     (shuffle (copy-list (data-rows d))))
         (half     (floor (length rows) 2))
         (train    (subseq rows 0 half))
         (test     (subseq rows half))
         (klass-at (? (? d cols klass) at))
         (b        (new+ nb d))
         (m        (new+ metrics)))
    (dolist (r train) (train b d r))
    (dolist (r test)
      (tally m (pred b r) (nth klass-at r)))
    (say m)))

(defun eg--acquire (&optional (file @file))
  (let+ ((d   (make-data (read-csv file)))
         (w   (new+ wins d (data-rows d)))
         (lab (acquire d))
         (dt  (clone d lab))
         (tr  (tree dt (data-rows dt))))
    (format t "wins lo=~,3F med=~,3F sd=~,3F~%"
            (wins-lo w) (wins-med w) (wins-sd w))
    (show tr dt)
    (let+ ((ranked (sort (copy-list (data-rows d)) #'<
                         :key (f+ (node-mu (find-leaf tr _)))))
           (top  (subseq ranked 0 (min @check (length ranked))))
           (best (%extremum-by top (f+ (disty d _)) #'<)))
      (format t "top-~D best disty=~,3F  win=~D/100~%"
              @check (disty d best) (score w d best))
      (format t "labelled=~D  budget=~D~%"
              (length lab) @budget))))

;; ## Main
(when (args)
  (setf *seed* @seed)
  (cli *the*))
