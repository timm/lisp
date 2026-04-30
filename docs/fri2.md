#| 
# AUTHOR-CONFIG
audience: newly arrived US-university grad students
assumed: basic programming skills
language: Common Lisp (SBCL target)
depth: standard
tutorial-placement: back
target: literate lisp
naming-prefix: eg--
tone: textbook
voice: third |#

#| 
# overview

fri2 is data-lite active learning in one file. It reads a
CSV, summarises each column in constant space, computes a
distance to the ideal goal vector, spends a fixed budget of
labels to find good rows, then grows a tiny decision tree
from those labels. The same file is the program and the
paper. Read it from top to bottom. |#

    (load "lib")

#| The shared config *the* is an a-list of quadruples
(name default flag doc). The reader macro `!x` from lib.lisp
returns the current default for `x`. The cli walker rewrites
those defaults in place. Every later module reads its knobs
through `!`, so changing a flag at the command line changes
behaviour without threading parameters through call chains. |#

    (defparameter *the*
      '((p      2            "-p"  "minkowski exponent")
        (file   "auto93.csv" "-f"  "csv data file")
        (seed   1            "-s"  "random seed")
        (budget 50           "-b"  "max labels to spend")
        (few    128          "-u"  "unlabelled pool cap")))

#| ## Atoms
A column is either symbolic or numeric. We keep one struct
per kind. `sym` counts how often each value appears: `has`
is an a-list of (value . count). `at` is the column index in
a row; `txt` is the header string. |#

    (defstruct sym
      (at 0) (txt " ") (n 0) has)

#| `num` keeps two scalars instead of a list of values:
running mean `mu` and second-moment accumulator `m2`. Welford
1962 derives sd on demand from m2 and n. The `heaven` slot
records goal direction: 1 means "bigger is better", 0 means
"smaller is better". The reader macro `$x` from lib.lisp
expands to `(slot-value i 'x)` so methods can refer to slots
of self by name. |#

    (defstruct (num (:constructor %make-num))
      (at 0) (txt " ") (n 0) (mu 0) (m2 0) (heaven 1))

#| Constructors come in two layers. `%make-num` is the raw
struct allocator. `make-num` wraps it: it inspects the last
character of `txt` and sets `heaven` to 0 when the column
name ends in `-`. The `&aux` clause runs after argument
binding but before the body, so `i` is in scope for the
setf. We follow the same pattern for `make-cols` later. |#

    (defun make-num (&rest args
                     &aux (i (apply #'%make-num args)))
      (setf $heaven (if (eq #\- (ch $txt -1)) 0 1))
      i)

#| ## Operations on atoms (sibling pairs)
Every column supports four verbs: `add`, `mid`, `spread`,
`norm`. We define each verb twice, once per kind, and rely
on CLOS dispatch to pick the right method. The pairs are
shown side by side so the symmetry is visible.

`add` updates the summary by one observation. For `sym` it
either bumps an existing cell or pushes a fresh one. The
weight `w` defaults to 1; passing -1 reverses an earlier
add. Missing values (the symbol `?`) are skipped: every
later step assumes a column's `n` reflects only seen
values. |#

    (defmethod add ((i sym) v &optional (w 1))
      (unless (eql v '?)
        (incf $n w)
        (let ((cell (assoc v $has :test #'equal)))
          (if cell
              (incf (cdr cell) w)
              (push (cons v w) $has))))
      v)

#| The `num` add applies Welford's two-line update. `mu`
moves towards the new value at rate `1/n`. `m2` then uses
the *new* `mu` for its own increment; that is not a bug,
it is the identity that lets sd be recovered later. The
`(minusp w) (<= $n 2)` clause resets to zero when the
caller asks to subtract from a near-empty summary; the
on-line variance update is unstable below n=2. |#

    (defmethod add ((i num) v &optional (w 1))
      (unless (eq v '?)
        (cond ((and (minusp w) (<= $n 2))
               (setf $n 0 $mu 0 $m2 0))
              (t (incf $n w)
                 (let ((delta (- v $mu)))
                   (incf $mu (float (/ (* w delta) $n)))
                   (incf $m2 (float
                              (* w delta (- v $mu))))))))
      v)

#| `mid` returns a central tendency. For symbols this is the
mode: the value with the largest count. For numbers it is
just the running mean. The two methods have the same name
and the same role; only the body changes. |#

    (defmethod mid ((i sym))
      (labels ((most (a b)
                 (if (> (cdr a) (cdr b)) a b)))
        (car (reduce #'most $has))))

    (defmethod mid ((i num)) $mu)

#| `spread` returns a width. For symbols this is the Shannon
entropy of `has`, in bits. For numbers it is the standard
deviation, recovered from `m2` and `n`. Both report zero on
under-populated summaries. The fact that `mid`/`spread`
take the same argument shape regardless of column type is
what lets `dist` (below) loop over a heterogeneous list of
columns without caring which is which. |#

    (defmethod spread ((i sym))
      (loop for (_ . v) in $has
            for p = (/ v $n)
            sum (- p (log p 2))))

    (defmethod spread ((i num))
      (if (< $n 2) 0
          (sqrt (/ (max 0 $m2) (1- $n)))))

#| `norm` maps a value to [0,1]. The symbol method is the
identity; categorical values do not have a natural ordering
to normalise against. The number method computes a clamped
z-score then squashes it through a logistic with slope 1.7,
which is the standard approximation to the gaussian CDF.
The output reads as "fraction of column at or below v". |#

    (defmethod norm ((i sym) v) v)

    (defmethod norm ((i num) v)
      (if (eq v '?) v
          (let ((z (/ (- v $mu) (+ (spread i) 1e-32))))
            (/ 1 (+ 1 (exp (* -1.7
                              (max -3 (min 3 z)))))))))

#| Two folds round out the atom layer. `adds` walks a list
and folds each value into a summary via `add`; the default
summary is a fresh `num`, so `(adds '(1 2 3))` returns a
populated num. `sub` is the symmetric remove: weight -1.
Together they make summaries reversible, which the active
learner uses to shuffle rows between best and rest piles
without recomputing means from scratch. |#

    (defun adds (lst &optional (summary (make-num)))
      "Fold LST into SUMMARY via `add`. Returns SUMMARY."
      (dolist (v lst summary)
        (add summary v)))

    (defun sub (it v)
      "Remove V from IT (symmetric with `add`)."
      (add it v -1))

#| ## Containers
A `cols` object is a triple of column lists carved from one
header row: `all` is every column; `x` is the input
features; `y` is the goals. `names` keeps the original
header strings for cloning. Skipped columns appear in `all`
but not in `x` or `y`. |#

    (defstruct (cols (:constructor %make-cols))
      names x y all)

#| A `data` object is a list of `rows` plus the `cols` that
summarise them. The `mid` slot caches the centroid (one
`mid` per column) so repeated lookups are free; mutation
via `add` invalidates it by setting it to nil. |#

    (defstruct (data (:constructor %make-data))
      rows cols mid)

#| `make-cols` walks the header strings and instantiates the
right kind of column for each. The local function `ako`
("a-kind-of") chooses `make-num` when the name starts with
an uppercase letter, else `make-sym`. The suffix character
then routes the column into `y` (when it ends in `!`, `-`,
or `+`), into `x` (otherwise), or drops it from both (when
it ends in `X`, the skip marker). |#

    (defun make-cols (txts
                      &aux (i (%make-cols :names txts))
                           (n -1))
      (labels ((ako (txt)
                 (if (upper-case-p (ch txt 0))
                     #'make-num #'make-sym))
               (one (txt)
                 (funcall (ako txt) :txt txt :at (incf n)))
               (end (col) (ch (? col txt) -1)))
        (setf $all (mapcar #'one txts))
        (dolist (col $all i)
          (unless (eql (end col) #\X)
            (if (find (end col) "!-+")
                (push col $y)
                (push col $x))))))

#| `make-data` assumes the first row of its input is the
header. It builds the `cols`, then folds the remaining rows
into the data via `adds`. The result is a fully populated
`data` object: every column summary is up to date and every
data row is in `rows`. |#

    (defun make-data (&optional rows)
      (adds (cdr rows)
            (%make-data :cols (make-cols (car rows)))))

#| `add` extends to containers by delegation. The `cols`
method maps `add` across each column with the matching cell
of `row`. The `data` method threads through `cols`, then
either pushes the row (positive weight) or removes it
(negative weight), invalidating the centroid cache. The
shared verb `add` now works uniformly on all four kinds:
sym, num, cols, data. |#

    (defmethod add ((i cols) row &optional (w 1))
      (mapcar (lambda (col v) (add col v w)) $all row)
      row)

    (defmethod add ((i data) row &optional (w 1))
      (setf $mid nil)
      (add $cols row w)
      (if (plusp w)
          (push row $rows)
          (setf $rows (remove row $rows
                              :test #'equal :count 1)))
      row)

#| `mid` for `data` returns the centroid: a list with one
`mid` per column. The result is memoised in `$mid`; later
mutations clear it. Many distance calls compare a row
against the centroid, so caching pays off quickly. |#

    (defmethod mid ((i data))
      (or $mid
          (setf $mid
                (mapcar #'mid (? i cols all)))))

#| ## Distance
`dist` rolls up a list of per-cell costs into one scalar
using a Minkowski norm. The exponent `p` is read from the
config, so `p=1` gives manhattan, `p=2` euclidean, `p>2`
biases toward the largest cell. The function `f` returns
the cost for one element; the caller supplies the
per-column logic. |#

    (defun dist (lst f &aux (d 0))
      (dolist (v lst
               (expt (/ d (length lst)) (/ 1 !p)))
        (incf d (expt (funcall f v) !p))))

#| `disty` is the goal distance: how far a row sits from the
ideal goal vector. For each y-column we take the absolute
gap between its normalised value and `heaven`. Minkowski
collapses those gaps into one number, used everywhere as
the row's "y-score". Lower is better. |#

    (defun disty (data row
                  &optional (cols (? data cols y)))
      (dist cols
            (lambda (i)
              (abs (- (norm i (elt row $at)) $heaven)))))

#| `distx` is the feature-space distance between two rows,
again Minkowski-rolled. The dispatcher loops over x-columns
and asks each cell pair for its cost; missing-on-both costs
1 (a worst case, so unknowns do not lie). The two leaf
methods then handle the actual comparison: symbols compare
by equality, numbers by absolute difference of normalised
values. When only one side is missing, the unknown is
pushed to the extreme furthest from the known: a pessimistic
imputation that keeps the dimension paying its weight. |#

    (defmethod distx ((i data) r1 r2)
      (dist (? i cols x)
            (lambda (i)
              (let ((a (elt r1 $at))
                    (b (elt r2 $at)))
                (if (and (eq a '?) (eq b '?))
                    1 (distx i a b))))))

    (defmethod distx ((i sym) a b)
      (if (equal a b) 0 1))

    (defmethod distx ((i num) a b
                      &aux (a (norm i a)) (b (norm i b)))
      (cond ((eq a '?)
             (abs (- b (if (> b 0.5) 0 1))))
            ((eq b '?)
             (abs (- a (if (> a 0.5) 0 1))))
            (t (abs (- a b)))))

#| `extremum-by` returns argmin or argmax in one O(n) pass.
The comparator decides direction: `#'<` gives argmin under
the key. Pulling this out lets later code stay readable
("the row with the smallest disty") without sorting the
whole list when it only needs the head. |#

    (defun extremum-by (lst key cmp)
      "Argmin or argmax of LST under KEY, ordered by CMP."
      (let* ((best (car lst)) (m (funcall key best)))
        (dolist (x (cdr lst) best)
          (let ((k (funcall key x)))
            (when (funcall cmp k m)
              (setf best x m k))))))

#| ## Trees
A `tree` node carries a cloned `data` (so summaries are
local to the node), a `ynum` summarising the disty of its
rows, the chosen split column `col` and `cut`, the two
children, and a display string `hdr`. Leaves have null
`left` and `right`. |#

    (defstruct (tree (:copier nil))
      d ynum col cut left right hdr)

#| Splits dispatch on column kind through two tiny generic
functions. `branch?` says whether a value goes "left" given
a cut: equality for symbols, `<=` for numbers. `%cuts`
returns the candidate cut values: every distinct symbol,
or just the median for numbers. The median-only choice for
numbers is regularisation; with ~50 labels, trying every
threshold would overfit. |#

    (defmethod branch? ((c sym) v cut) (equal v cut))
    (defmethod branch? ((c num) v cut) (<= v cut))

    (defmethod %cuts ((c sym) vs)
      (remove-duplicates vs :test #'equal))

    (defmethod %cuts ((c num) vs)
      (list (elt (sort vs #'<) (floor (length vs) 2))))

#| `new-tree` builds a node before any splitting decision is
made: clone the data, summarise the disty of its rows into
`ynum`, attach a header string. Later, if `tree-grow` finds
a useful split, it fills in `col`, `cut`, and the children;
otherwise the node stays a leaf. |#

    (defun new-tree (d rs &optional (hdr ""))
      "Tree node: cloned data, a num of distys, header."
      (make-tree
        :d (make-data (cons (? d cols names) rs))
        :ynum (adds (mapcar (lambda (r) (disty d r)) rs)
                    (make-num))
        :hdr hdr))

#| `go-left?` is the routing decision for one row at one
node. Missing values fall left by convention; routing has
to be deterministic so that prediction matches training. |#

    (defun go-left? (c v cut)
      "Does V go left under (C,CUT)? Missing goes left."
      (or (eq v '?) (branch? c v cut)))

#| `tree-cuts` extracts the non-missing values of column `c`
across rows `rs`, then asks `%cuts` for candidate splits.
The intermediate list is short-lived; only the cuts matter
to the caller. |#

    (defun tree-cuts (c rs)
      "Cut candidates for column C over RS."
      (let ((vs (loop for r in rs
                      for v = (elt r (? c at))
                      unless (eq v '?) collect v)))
        (and vs (%cuts c vs))))

#| `tree-split` partitions `rs` by one (column, cut) and
scores the partition. The score is `n * spread` summed over
sides: lower means cleaner separation on the y-distance.
Returning the row partitions alongside the score lets
`tree-grow` reuse them without re-walking. |#

    (defun tree-split (d c cut rs)
      "Score one split. Returns (score c cut left right)."
      (let (l r (ln (make-num)) (rn (make-num)))
        (dolist (row rs)
          (if (go-left? c (elt row (? c at)) cut)
              (progn (push row l) (add ln (disty d row)))
              (progn (push row r) (add rn (disty d row)))))
        (list (+ (* (? ln n) (spread ln))
                 (* (? rn n) (spread rn)))
              c cut l r)))

#| `tree-splits` enumerates every (column, cut) pair on the
x-columns and keeps those whose minority side has at least
`leaf` rows. The `leaf` floor stops cuts that would create
near-empty children. |#

    (defun tree-splits (d rs leaf)
      "Splits with both sides >= LEAF rows."
      (loop for c in (? d cols x)
            append (loop for cut in (tree-cuts c rs)
                         for s = (tree-split d c cut rs)
                         when (>= (min (length (fourth s))
                                       (length (fifth s)))
                                  leaf)
                         collect s)))

#| `kid-hdr` formats one branch's header string. It picks
`==`/`!=` for symbols and `<=`/`>` for numbers, matching
the routing rule in `branch?`. The header is a presentation
detail, but pulling it out keeps `tree-grow` focused on
structure. |#

    (defun kid-hdr (c cut left?)
      "Display header for a child branch."
      (format nil "~a ~a ~a" (? c txt)
              (if left?
                  (if (typep c 'sym) "==" "<=")
                  (if (typep c 'sym) "!=" ">"))
              cut))

#| `tree-grow` is the recursion. Build a node. If too few
rows remain, stop. Otherwise score every legal split, take
the lowest, and recurse on each side. The base case keeps
the recursion finite without an explicit depth bound; the
data shrinks with every level. |#

    (defun tree-grow (d rs &optional (leaf 3) (hdr ""))
      "Recursively grow tree; stop at < 2*LEAF rows."
      (let ((t0 (new-tree d rs hdr)))
        (when (>= (length rs) (* 2 leaf))
          (let ((splits (tree-splits d rs leaf)))
            (when splits
              (let* ((best (extremum-by splits
                                        #'first #'<))
                     (c (second best)) (cut (third best)))
                (setf (? t0 col) c (? t0 cut) cut
                      (? t0 left)
                       (tree-grow d (fourth best) leaf
                                  (kid-hdr c cut t))
                      (? t0 right)
                       (tree-grow d (fifth best) leaf
                                  (kid-hdr c cut nil)))))))
        t0))

#| `tree-leaf` routes a single row through a built tree to
its terminal node. `tree-show` walks the tree depth-first,
printing each node on one line: mu and n on the left,
indent and condition on the right. Both are tiny because
the structure of `tree` carries the work. |#

    (defun tree-leaf (t0 r)
      "Route R to its leaf."
      (if (null (? t0 left)) t0
          (tree-leaf
            (if (go-left?
                  (? t0 col)
                  (elt r (? (? t0 col) at))
                  (? t0 cut))
                (? t0 left) (? t0 right))
            r)))

    (defun tree-show (t0 &optional (lvl 0))
      "Pretty-print: mu, n on LHS; indent + cond on RHS."
      (format t "~&~4,2f (~3d)   ~{~A~}~a~%"
              (mid (? t0 ynum)) (? (? t0 ynum) n)
              (loop repeat (max 0 (1- lvl))
                    collect "|.. ")
              (? t0 hdr))
      (when (? t0 left)
        (tree-show (? t0 left)  (1+ lvl))
        (tree-show (? t0 right) (1+ lvl))))

#| ## Active learning
`wins` returns a closure that maps a row to a 0..100 score
against a "god" data set: 100 means as close to the best
known disty as possible, 50 means around the median. Used
only for evaluation; the learner never sees these numbers
during training. |#

    (defun wins (god)
      "Score a row 0..100 against the god truth."
      (let* ((ys (mapcar (lambda (r) (disty god r))
                         (? god rows)))
             (ds (sort ys #'<))
             (lo (first ds))
             (md (elt ds (floor (length ds) 2))))
        (lambda (r)
          (floor (* 100
                    (- 1 (/ (- (disty god r) lo)
                            (+ (- md lo) 1e-32))))))))

#| `rebalance` keeps the `best` pile small. Whenever it
exceeds sqrt(|labels|), the worst-disty row in `best` is
moved to `rest`. The sqrt cap is empirically the sweet
spot: small enough that the centroid stays discriminating,
big enough that one noisy row cannot poison it. |#

    (defun rebalance (best rest lab)
      "Cap best at sqrt(|lab|): move worst row to rest."
      (when (> (length (? best rows))
               (sqrt (length (? lab rows))))
        (let ((bad (extremum-by (? best rows)
                                (lambda (r)
                                  (disty lab r))
                                #'>)))
          (sub best bad) (add rest bad))))

#| `closer?` is the acquisition test: would labelling row
`r` push it nearer the centroid of `best` than that of
`rest`? If yes, label it. Centroid distance is the
cheapest signal that still uses everything we know about
the two piles. |#

    (defun closer? (lab best rest r)
      "Is R closer to best's centroid than rest's?"
      (< (distx lab r (mid best))
         (distx lab r (mid rest))))

#| `warm-start` pays for the first four labels, then splits
them into best (top sqrt by disty) and rest (the others).
This bootstraps `best` and `rest` with enough mass for
`closer?` to mean something on the next row. |#

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

#| `active` is the main loop. Shuffle the pool, warm-start,
then walk capped pool: for each row, if the acquisition
test fires, pay for the label, fold it into `best`, and
rebalance. Stop when `ys` (the num counting label spend)
hits the budget. Returns three values: the best pile, the
full label set, and the count of labels actually paid. |#

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

#| `validate` is the holdout evaluator. Half the data trains
via `active`. A tree is grown on the labelled rows. Test
rows are ranked by their leaf's mean disty, and the top
`check` are inspected against the god truth. The function
returns three numbers: the score of the best train row, the
score of the best test pick, and the total label spend. |#

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
                 (train-best (extremum-by (? lab rows)
                                          y-god #'<))
                 (tr (tree-grow lab (? lab rows)))
                 (ranked
                  (sort (copy-list test) #'<
                        :key (lambda (r)
                               (mid (? (tree-leaf tr r)
                                       ynum)))))
                 (top (subseq ranked 0
                              (min check (length ranked))))
                 (pick (extremum-by top y-god #'<)))
            (values (funcall w train-best)
                    (funcall w pick)
                    (+ labels check))))))

#| ## Examples
Each `eg--` is a runnable demonstration of one concept
introduced above. They print to stdout and own their own
state. Names map to CLI flags through the lib dispatcher:
`--rows` runs `eg--rows`, and so on. |#

    (defun %stride (rows key &optional (n 30))
      "Sort ROWS by KEY then print every Nth row."
      (loop for x in (sort rows #'< :key key)
            by (lambda (l) (nthcdr n l))
            do (print x)))

    (defun eg-s (&optional (seed !seed))
      "Set the random seed (both *seed* and !seed)."
      (setf *seed* seed  !seed  seed))

    (defun eg--the (_)
      "Print the current config."
      (print *the*))

    (defun eg--rows (&optional (file !file))
      "Read CSV; print its tail."
      (let ((rows (read-csv file)))
        (print (subseq rows 380))))

    (defun eg--data (&optional (file !file))
      "Build a Data; print its y-columns."
      (let ((i (make-data (read-csv file))))
        (print (? i cols y))))

    (defun eg--stats (&optional (file !file))
      "Print mid and spread for every column."
      (let ((i (make-data (read-csv file))))
        (dolist (c (? i cols all))
          (format t "~&~12a  mid=~a  spread=~a~%"
                  (? c txt) (mid c) (spread c)))))

    (defun eg--norm (&optional (file !file))
      "Show raw vs normalized y-values for one row."
      (let* ((i (make-data (read-csv file)))
             (r (car $rows)))
        (dolist (c (? i cols y))
          (let ((v (elt r (? c at))))
            (format t "~&~8a  raw=~a  norm=~a~%"
                    (? c txt) v (norm c v))))))

    (defun eg--ydata (&optional (file !file))
      "Sort by disty (goal distance); stride-print."
      (let ((i (make-data (read-csv file))))
        (%stride $rows (lambda (r) (disty i r)))))

    (defun eg--xdata (&optional (file !file))
      "Sort by distx from row 0; stride-print."
      (let* ((i (make-data (read-csv file)))
             (r0 (car $rows)))
        (%stride $rows
                 (lambda (r) (distx i r0 r)))))

    (defun eg--tree (&optional (file !file))
      "Active-label, grow a tree, pretty-print it."
      (let ((rs (read-csv file)))
        (multiple-value-bind (best lab labels)
            (active rs)
          (declare (ignore best))
          (format t "~&;; labels used: ~d~%" labels)
          (tree-show (tree-grow lab (? lab rows))))))

    (defun eg--active (&optional (file !file))
      "20 train/test splits; print: train hold labels."
      (let* ((rs (read-csv file))
             (god (make-data rs))
             (w (wins god)))
        (loop repeat 20 do
              (multiple-value-bind (train hold labels)
                  (validate rs god w)
                (format t "~&~3d ~3d ~3d~%"
                        train hold labels)))))

    (defun eg--all (&optional (arg !file))
      "Run every eg--* function except eg--all itself."
      (do-symbols (s *package*)
        (unless (eq s 'eg--all) (run s arg))))

#| The cli dispatcher walks `(args)` and either runs the
matching `eg--*` or rewrites the matching slot in `*the*`.
This is the last line of the file, so loading then exiting
runs whatever flag the user passed. |#

    (cli *the*)

#| ## Tutorial appendix
This appendix covers the Common Lisp constructs the body
above actually uses. Readers fluent in C, Python, or Java
recognise most of the structure; the items below are the
spots where Lisp diverges and a fresh reader is most likely
to stumble. Each entry cites the body stanza where the
construct first appears. |#

#| ### quote vs sharp-quote: 'x vs #'x
`'x` is data: it stops evaluation and hands back the symbol
or list literally. `#'x` is the function value of the
symbol `x`, used wherever a function is passed as an
argument. So `'foo` and `#'foo` are different things; one
is a name, the other is the function bound to that name.
First seen in `extremum-by` where comparators arrive as
`#'<`, and in the `*the*` quoted a-list. |#

    'foo       ; the symbol foo (data)
    #'<        ; the function bound to <

#| ### nil is empty list and false
There is no separate boolean type. The empty list `'()`,
the symbol `nil`, and false are the same value. Anything
else is true. So `(if (null xs) ...)` and `(if xs ...)`
read naturally on lists. Used throughout. |#

#| ### let, let*, and double-paren bindings
`let` binds variables in parallel; `let*` binds
sequentially. The bindings live inside an extra pair of
parens: `(let ((x 1) (y 2)) ...)`. The inner parens
surround each `(name value)`. `let*` shows up wherever a
later binding refers to an earlier one (e.g. `validate`). |#

    (let  ((a 1) (b 2)) (+ a b))     ; parallel
    (let* ((a 1) (b (* a 2))) b)     ; b sees a

#| ### setf as the universal place setter
`(setf place value)` writes `value` into `place`. The
"place" can be a variable, a slot, an array cell, an a-list
entry, even a user-defined accessor. `(setf $mu 0)` is the
slot write inside `add (num)`; `(setf (cdr cell) v)` is the
a-list write inside `add (sym)`. |#

#| ### lambda lists: &optional, &key, &rest, &aux
A function's parameter list can carry markers that change
how arguments bind. `&optional` introduces defaults
(`(w 1)` in `add`). `&key` introduces named arguments
(`make-num :txt "x"`). `&rest` collects extras into a list
(`make-num`). `&aux` declares locals initialised after the
parameters but before the body, used in `make-num` to
inspect `txt`. |#

    (defun f (x &optional (y 0) &aux (z (* x 2)))
      (+ x y z))

#| ### multiple values: values and multiple-value-bind
A function returns more than one value with `values`, and
the caller destructures with `multiple-value-bind`. Used
by `active` to return three things at once (best, lab,
label-count) and by `validate` to read them. |#

    (defun two () (values 1 2))
    (multiple-value-bind (a b) (two) (list a b))

#| ### defstruct and slot access
`defstruct` declares a record type and generates a
constructor, accessors, and a type predicate. The
`(:constructor %make-num)` clause renames the auto
constructor so we can wrap it in our own `make-num`.
`(slot-value i 'mu)` reads the `mu` slot of `i`; the
reader macro `$mu` from lib.lisp expands to exactly that
when `i` is in scope. |#

#| ### defmethod and CLOS dispatch
`defmethod name ((arg type) ...)` defines one method of a
generic function. The runtime picks the method whose
argument types match. The body's atoms have parallel
methods (`add`, `mid`, `spread`, `norm`) for `sym` and
`num`; the dispatcher chooses without an explicit type
test in the caller. |#

    (defmethod area ((s circle))    ...)
    (defmethod area ((s rectangle)) ...)

#| ### loop: a mini-language inside Lisp
`loop` accepts a small DSL of clauses: `for`, `in`,
`collect`, `sum`, `when`, `do`. Every loop in the body
uses some subset; the most exotic is `tree-show`'s
`loop repeat n collect "|.. "` for indenting. |#

    (loop for x in '(1 2 3) collect (* x x))   ; (1 4 9)

#| ### labels for local recursion
`labels` is `let` for functions: it introduces local
defuns, possibly mutually recursive. `make-cols` uses it
to define `ako`, `one`, and `end` without polluting the
top level. |#

    (labels ((sq (x) (* x x))) (sq 5))         ; 25

#| ### dolist and the implicit return value
`(dolist (var list result) body)` walks `list` binding
`var`. The optional `result` form is evaluated and
returned at the end. `make-cols` uses this to return `i`
after pushing each column into its bucket. |#

    (dolist (x '(1 2 3) 'done) (print x))      ; done

#| ### cons, list, append, push, pop
`cons` adds one head to a list; `list` builds a fresh
list from arguments; `append` glues lists end to end.
`push` and `pop` are macros that mutate the variable
holding the list. `(push x xs)` is `(setf xs (cons x xs))`.
Used everywhere; especially `add (sym)`. |#

#| ### macro vs function
A macro receives unevaluated source forms and returns new
source forms; the compiler then evaluates the result. The
reader macros `!` and `$` and `?` from lib.lisp are
macros expanding to `(second (assoc ...))` and
`(slot-value ...)` calls. They look like variables in the
source but compile to ordinary code. |#

#| ### reader macros: #|, #\, #'
`#|...|#` opens a block comment. `#\X` is a character
literal. `#'name` is sharp-quote (function value). The
prose in this file lives entirely inside `#|...|#`. |#

#| ### multiple-value gotcha: declare ignore
When a `multiple-value-bind` binds a value the body does
not use, declare it ignored to silence the compiler:
`(declare (ignore best))` in `validate`. |#

#| End. The file above loads top to bottom into SBCL. Each
stanza is one concept. Each concept is a small step on a
trail that runs from a single column summary to a tree of
rules learned from a fixed label budget. Read it once for
the shape; read it again for the seams. |#
