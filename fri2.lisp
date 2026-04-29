; vim: set ft=lisp ts=2 sw=2 et lispwords+=with-slots,loop,unless :
#+sbcl (declaim (sb-ext:muffle-conditions warning style-warning))
#+sbcl (setf sb-ext:*invoke-debugger-hook* 
             (lambda (c h) (declare (ignore h))
               (format *error-output* "~&[ERROR] ~a~%" c)
              (sb-ext:exit :code 1)))

; fri.lisp -- data-lite active learning in a single file.
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
;   --norm    show raw vs normalized y-values of row 0
;   --ydata   sort rows by goal-distance    (disty); stride-print
;   --xdata   sort rows by feature-distance (distx) from row 0
;   --tree    active-label, grow tree, pretty-print it
;   --active  20 train/test splits; prints: train hold labels
;
; OPTIONS
;   -p N   Minkowski distance exponent       default 2
;   -f F   CSV file                          default auto93.csv
;   -s N   random seed                       default 1
;   -b N   label budget                      default 50
;   -u N   unlabelled pool cap               default 128
;
; CSV INPUT. Header row names columns; suffix sets role.
;   [A-Z]*   numeric            [a-z]*   symbolic
;   [A-Z]*-  minimize (y)       [A-Z]*+  maximize (y)
;   *!       class column (y)   *X       skip column
;   ?        missing value
;
; NAMING: r/rs=row(s), d=data, c=column, i=self, ys=Num of distys.
;
; READER MACROS & ANAPHORA
;   !key    -> (second (assoc 'key *the*))    config lookup
;   $field  -> (slot-value i 'field)          slot (i = self)
;   (? x a) -> (slot-value x 'a); chains: (? x a b c)
;   (aif t x y)  binds `it` to t's value inside x/y

;=================================================================
; Part 1 -- Config & Macros
;=================================================================
(defparameter *the*
  '((p      2            "-p"  "distance coeffecient")
    (file   "auto93.csv" "-f"  "data file")
    (seed   1            "-s"  "seed")
    (budget 50           "-b"  "max labels")
    (few    128          "-u"  "max unlabelled pool")))

(defmacro defread (name (stream) &body body)
  "Install BODY as reader-macro for char NAME (non-terminating)."
  `(set-macro-character ,(character (symbol-name name))
     (lambda (,stream c) (declare (ignore c)) ,@body)
     t))

(defread !(s) `(second (assoc ',(read s t nil t) *the*)))
(defread $(s) `(slot-value i ',(read s t nil t)))

(defmacro ? (x &rest at)
  "Nested slot access: (? x a b) = (slot-value (slot-value x 'a) 'b)."
  (if at `(? (slot-value ,x ',(car at)) ,@(cdr at)) x))

(defmacro aif (test then &optional else)
  "Anaphoric if: binds `it` to TEST's value for use in THEN/ELSE."
  `(let ((it ,test)) (if it ,then ,else)))

;=================================================================
; Part 2 -- TYPES
;=================================================================
(defstruct sym (at 0) (txt " ") (n 0) has)

(defstruct (num  (:constructor %make-num))
  (at 0) (txt " ") (n 0) (mu 0) (m2 0) (heaven 1))

(defstruct (cols (:constructor %make-cols)) names x y all)

(defstruct (data (:constructor %make-data)) rows cols mid)

(defun make-num (&rest args &aux (i (apply #'%make-num args)))
  (setf $heaven (if (eq #\- (ch $txt -1)) 0 1))
  i)

(defun make-cols (txts &aux (i (%make-cols :names txts)) (n -1))
  (labels ((ako (txt)
             (if (upper-case-p (ch txt 0)) #'make-num #'make-sym))
           (one (txt) (funcall (ako txt) :txt txt :at (incf n)))
           (end (col) (ch (? col txt) -1)))
    (setf $all (mapcar #'one txts))
    (dolist (col $all i)
      (unless (eql (end col) #\X)
        (if (find (end col) "!-+") (push col $y) (push col $x))))))

(defun make-data (&optional rows)
  (adds (cdr rows) (%make-data :cols (make-cols (car rows)))))

(defun adds (lst &optional (summary (make-num)))
  "Fold LST into SUMMARY (default Num) via `add`. Returns SUMMARY."
  (dolist (v lst summary)
    (add summary v)))

(defun sub (it v)
  "Remove V from IT (symmetric with `add`)."
  (add it v -1))

(defmethod add ((i data) row &optional (w 1))
  (setf $mid nil)
  (add $cols row w)
  (if (plusp w)
      (push row $rows)
      (setf $rows (remove row $rows :test #'equal :count 1)))
  row)

(defmethod add ((i cols) row &optional (w 1))
  (mapcar (lambda (col v) (add col v w)) $all row)
  row)

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
               (incf $mu (float (/ (* w delta) $n)))
               (incf $m2 (float (* w delta (- v $mu))))))))
  v)


;=================================================================
; CHAPTER 3 -- COLUMN STATS & DISTANCE
;=================================================================
(defmethod mid ((i num)) $mu)

(defmethod mid ((i sym))
  (labels ((most (a b) (if (> (cdr a) (cdr b)) a b)))
    (car (reduce #'most $has))))

(defmethod mid ((i data))
  (or $mid (setf $mid (mapcar #'mid (? i cols all)))))

(defmethod spread ((i sym))
  (loop for (_ . v) in $has for p = (/ v $n) sum (- p (log p 2))))

(defmethod spread ((i num))
  (if (< $n 2) 0 (sqrt (/ (max 0 $m2) (1- $n)))))

(defmethod norm ((i sym) v) v)
(defmethod norm ((i num) v)
  (if (eq v '?) v
      (let ((z (/ (- v $mu) (+ (spread i) 1e-32))))
        (/ 1 (+ 1 (exp (* -1.7 (max -3 (min 3 z)))))))))

(defun dist (lst f &aux (d 0))
  (dolist (v lst (expt (/ d (length lst)) (/ 1 !p)))
    (incf d (expt (funcall f v) !p))))

(defun disty (data row &optional (cols (? data cols y)))
  (dist cols 
        (lambda (i) (abs (- (norm i (elt row $at)) $heaven)))))

(defmethod distx ((i data) r1 r2)
  (dist (? i cols x)
        (lambda (i)
          (let ((a (elt r1 $at)) (b (elt r2 $at)))
            (if (and (eq a '?) (eq b '?)) 1 (distx i a b))))))

(defmethod distx ((i sym) a b)
  (if (equal a b) 0 1))

(defmethod distx ((i num) a b 
                          &aux (a (norm i a)) (b (norm i b)))
  (cond ((eq a '?) (abs (- b (if (> b 0.5) 0 1))))
        ((eq b '?) (abs (- a (if (> a 0.5) 0 1))))
        (t         (abs (- a b)))))

;------------------ trees (XAI payoff) ----------------------
(defstruct (tree (:copier nil)) d ynum col cut left right hdr)

(defmethod branch? ((c sym) v cut) (equal v cut))
(defmethod branch? ((c num) v cut) (<= v cut))

(defmethod %cuts ((c sym) vs) (remove-duplicates vs :test #'equal))
(defmethod %cuts ((c num) vs)
  (list (elt (sort vs #'<) (floor (length vs) 2))))

(defun new-tree (d rs &optional (hdr ""))
  "Tree node: cloned Data, a Num of distys, a display string."
  (make-tree :d (make-data (cons (? d cols names) rs))
             :ynum (adds (mapcar (lambda (r) (disty d r)) rs)
                         (make-num))
             :hdr hdr))

(defun go-left? (c v cut)
  "Does V go left? Missing goes left."
  (or (eq v '?) (branch? c v cut)))

(defun tree-cuts (c rs)
  "Cut candidates for column C over RS. Median-only on nums is
regularization: at ~50 labels, trying all thresholds overfits."
  (let ((vs (loop for r in rs for v = (elt r (? c at))
                  unless (eq v '?) collect v)))
    (and vs (%cuts c vs))))

(defun tree-split (d c cut rs)
  "Split RS by (C,CUT). Returns (score c cut left-rs right-rs)."
  (let (l r (ln (make-num)) (rn (make-num)))
    (dolist (row rs)
      (if (go-left? c (elt row (? c at)) cut)
          (progn (push row l) (add ln (disty d row)))
          (progn (push row r) (add rn (disty d row)))))
    (list (+ (* (? ln n) (spread ln)) (* (? rn n) (spread rn)))
          c cut l r)))

(defun tree-splits (d rs leaf)
  "Enumerate splits where both sides have >= LEAF rows."
  (loop for c in (? d cols x)
        append (loop for cut in (tree-cuts c rs)
                     for s = (tree-split d c cut rs)
                     when (>= (min (length (fourth s))
                                   (length (fifth s))) leaf)
                     collect s)))

(defun kid-hdr (c cut left?)
  "Display header for a child branch."
  (format nil "~a ~a ~a" (? c txt)
          (if left? (if (typep c 'sym) "==" "<=")
                    (if (typep c 'sym) "!=" ">"))
          cut))

(defun tree-grow (d rs &optional (leaf 3) (hdr ""))
  "Recursively grow tree; stop when node has < 2*LEAF rows."
  (let ((t0 (new-tree d rs hdr)))
    (when (>= (length rs) (* 2 leaf))
      (let ((splits (tree-splits d rs leaf)))
        (when splits
          (let* ((best (extremum-by splits #'first #'<))
                 (c (second best)) (cut (third best)))
            (setf (? t0 col) c (? t0 cut) cut
                  (? t0 left)  (tree-grow d (fourth best) leaf
                                          (kid-hdr c cut t))
                  (? t0 right) (tree-grow d (fifth best) leaf
                                          (kid-hdr c cut nil)))))))
    t0))

(defun tree-leaf (t0 r)
  "Route R to its leaf."
  (if (null (? t0 left)) t0
      (tree-leaf (if (go-left? (? t0 col)
                               (elt r (? (? t0 col) at))
                               (? t0 cut))
                     (? t0 left) (? t0 right)) r)))

(defun tree-show (t0 &optional (lvl 0))
  "Pretty-print: mu, n on LHS; indent + condition on RHS."
  (format t "~&~4,2f (~3d)   ~{~A~}~a~%"
          (mid (? t0 ynum)) (? (? t0 ynum) n)
          (loop repeat (max 0 (1- lvl)) collect "|.. ")
          (? t0 hdr))
  (when (? t0 left)
    (tree-show (? t0 left)  (1+ lvl))
    (tree-show (? t0 right) (1+ lvl))))


;=================================================================
; CHAPTER 4 -- ACTIVE LEARNING
;=================================================================
(defun wins (god)
  "Score a row 0..100: 100=best disty, 50=median. Uses god truth."
  (let* ((ys (mapcar (lambda (r) (disty god r)) (? god rows)))
         (ds (sort ys #'<))
         (lo (first ds))
         (md (elt ds (floor (length ds) 2))))
    (lambda (r) (floor (* 100 (- 1 (/ (- (disty god r) lo)
                                      (+ (- md lo) 1e-32))))))))

(defun extremum-by (lst key cmp)
  "Return argmin/argmax of LST under KEY, ordered by CMP. O(n)."
  (let* ((best (car lst)) (m (funcall key best)))
    (dolist (x (cdr lst) best)
      (let ((k (funcall key x)))
        (when (funcall cmp k m) (setf best x m k))))))

(defun rebalance (best rest lab)
  "Cap best at sqrt(|lab|): move worst-disty row from best to rest."
  (when (> (length (? best rows)) (sqrt (length (? lab rows))))
    (let ((bad (extremum-by (? best rows)
                            (lambda (r) (disty lab r)) #'>)))
      (sub best bad) (add rest bad))))

(defun closer? (lab best rest r)
  "Is R closer to best's centroid than rest's? Acquisition filter."
  (< (distx lab r (mid best)) (distx lab r (mid rest))))

(defun warm-start (rs lab best rest ys &aux (start 4))
  "Label 4 rows, split by disty (top sqrt = best, rest = rest)."
  (dotimes (_ (min start (length rs)))
    (let ((r (pop rs))) (add lab r) (add ys (disty lab r))))
  (let* ((sorted (sort (copy-list (? lab rows)) #'<
                       :key (lambda (r) (disty lab r))))
         (n (max 1 (floor (sqrt (length sorted))))))
    (dolist (r (subseq sorted 0 n)) (add best r))
    (dolist (r (subseq sorted n))   (add rest r)))
  rs)

(defun active (rs &aux (hd (list (car rs))))
  (let* ((lab  (make-data hd)) (best (make-data hd))
         (rest (make-data hd)) (ys   (make-num))
         (pool (warm-start (shuffle (cdr rs)) lab best rest ys)))
    (loop for r in (subseq pool 0 (min !few (length pool)))
          while (< (? ys n) !budget) do
          (when (closer? lab best rest r)
            (add ys (disty lab r)) (add lab r) (add best r)
            (rebalance best rest lab)))
    (values best lab (? ys n))))

(defun validate (rs god w &optional (check 5))
  "Run active; grow tree from labels; rank holdout by leaf y-mean;
pay CHECK labels on top. Return (values train-win hold-win labels)."
  (let* ((body (shuffle (cdr rs)))
         (n (floor (length body) 2))
         (train (cons (car rs) (subseq body 0 n)))
         (test  (subseq body n)))
    (multiple-value-bind (best lab labels) (active train)
      (declare (ignore best))
      (let* ((y-god (lambda (r) (disty god r)))
             (train-best (extremum-by (? lab rows) y-god #'<))
             (tr (tree-grow lab (? lab rows)))
             ;(tr (tree-grow lab (? lab rows) (max 3 (floor (sqrt labels)))))
             (ranked (sort (copy-list test) #'<
                           :key (lambda (r)
                                  (mid (? (tree-leaf tr r) ynum)))))
             (top  (subseq ranked 0 (min check (length ranked))))
             (pick (extremum-by top y-god #'<)))
        (values (funcall w train-best)
                (funcall w pick)
                (+ labels check))))))

;=================================================================
; CHAPTER 5 -- UTILITIES
;=================================================================
(defvar *seed* !seed)

(defun rand (&optional (n 1))
  "Return reproducible float in [0,n). Advances *seed*."
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 100) &aux (base 1E10))
  "Return reproducible integer in [0,n)."
  (floor (* n (/ (rand base) base))))

(defun shuffle (lst &aux (v (coerce lst 'vector)))
  "Fisher-Yates shuffle of LST. Seeded via *seed*."
  (loop for i from (1- (length v)) downto 1 do
    (rotatef (aref v i) (aref v (rint (1+ i)))))
  (coerce v 'list))

(defun ch (s n)
  "Char at position N of string S. Negative N counts from end."
  (char s (if (minusp n) (+ (length s) n) n)))

(defun split (s sep)
  "Split string S on character SEP, returning list of substrings."
  (loop for start = 0 then (1+ pos)
    for pos = (position sep s :start start)
    collect (subseq s start (or pos (length s))) while pos))

(defun thing (str &aux (v (ignore-errors (read-from-string str))))
  "Coerce STR to number, '? for \"?\", else leave as string."
  (cond ((numberp v) v)
        ((string= str "?") '?)
        (t str)))

(defun read-csv (file)
  "Read FILE as CSV, coerce each cell via `thing`, yield list of rows."
  (with-open-file (s file)
    (loop for line = (read-line s nil) while line
      collect (mapcar #'thing (split line #\,)))))

(defun args ()
  "Command-line args as list of strings (portable SBCL/CLISP)."
  #+sbcl (cdr sb-ext:*posix-argv*) #+clisp ext:*args*)

(defun run (it &optional arg)
  "Dispatch --flag to EG-FLAG function (if fbound and named EG-*)."
  (let* ((f (if (symbolp it) it (intern (format nil "EG~:@(~a~)" it))))
         (n (symbol-name f)))
    (when (and (fboundp f) (> (length n) 3) (string= n "EG-" :end1 3))
      (setf *seed* !seed)
      (if arg (funcall f arg) (funcall f))
      t)))

(defun cli (lsts)
  "Walk argv in (flag arg) pairs: dispatch EG-* or update *the* slot."
  (loop for (flag arg) on (args) by #'cddr do
    (unless (run flag (thing arg))
      (aif (find flag lsts :key #'third :test #'equalp)
           (setf (second it) (thing arg))))))


;=================================================================
; CHAPTER 6 -- EXAMPLES  (teaching arc: simple -> complex)
;=================================================================
(defun %stride (rows key &optional (n 30))
  "Sort ROWS by KEY then print every Nth for quick eyeballing."
  (loop for x in (sort rows #'< :key key)
    by (lambda (l) (nthcdr n l)) do (print x)))

(defun eg--all (&optional (arg !file))
  "Run every eg--* function except eg--all itself."
  (do-symbols (s *package*)
    (unless (eq s 'eg--all) (run s arg))))

(defun eg-s (&optional (seed !seed))
  "Set the seed (both *seed* special and !seed entry in *the*)."
  (setf *seed* seed  !seed  seed))

; ---- Step 1: show the config ----
(defun eg--the (_) "Print *the* (command-line defaults)." (print *the*))

; ---- Step 2: read the file ----
(defun eg--rows (&optional (file !file))
  "Read the CSV, print its tail."
  (let ((rows (read-csv file))) (print (subseq rows 380))))

; ---- Step 3: build the Data struct ----
(defun eg--data (&optional (file !file))
  "Build a Data object, print its y-columns."
  (let ((i (make-data (read-csv file))))
    (print (? i cols y))))

; ---- Step 4: column summary stats ----
(defun eg--stats (&optional (file !file))
  "Print mid, spread per column (introduces `mid` and `spread`)."
  (let ((i (make-data (read-csv file))))
    (dolist (c (? i cols all))
      (format t "~&~12a  mid=~a  spread=~a~%"
              (? c txt) (mid c) (spread c)))))

; ---- Step 5: normalization ----
(defun eg--norm (&optional (file !file))
  "One row: raw vs normalized for every y-column (introduces `norm`)."
  (let* ((i (make-data (read-csv file))) (r (car $rows)))
    (dolist (c (? i cols y))
      (let ((v (elt r (? c at))))
        (format t "~&~8a  raw=~a  norm=~a~%"
                (? c txt) v (norm c v))))))

; ---- Step 6: goal distance ----
(defun eg--ydata (&optional (file !file))
  "Sort rows by disty (distance to ideal y), stride-print."
  (let ((i (make-data (read-csv file))))
    (%stride $rows (lambda (r) (disty i r)))))

; ---- Step 7: feature distance ----
(defun eg--xdata (&optional (file !file))
  "Sort rows by distx from row 0 (feature-space distance)."
  (let* ((i (make-data (read-csv file)))
         (r0 (car $rows)))
    (%stride $rows (lambda (r) (distx i r0 r)))))

; ---- Step 8: grow & show a tree from active-labeled rows ----
(defun eg--tree (&optional (file !file))
  "Active-label, grow a tree from those labels, pretty-print it."
  (let ((rs (read-csv file)))
    (multiple-value-bind (best lab labels) (active rs)
      (declare (ignore best))
      (format t "~&;; labels used: ~d~%" labels)
      (tree-show (tree-grow lab (? lab rows))))))

; ---- Step 9: the whole pipeline ----
(defun eg--active (&optional (file !file))
  "20 runs of train/test; report: train-win, hold-win, labels."
  (let* ((rs (read-csv file)) (god (make-data rs)) (w (wins god)))
    (loop repeat 20 do
      (multiple-value-bind (train hold labels) (validate rs god w)
        (format t "~&~3d ~3d ~3d~%" train hold labels)))))

;----------------------------------------------------
(cli *the*)
