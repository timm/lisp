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
;
; OO HELPERS
;   (ren name [doc] opts slots (m args body)...)
;     defstruct + defmethod each method on `i`. The
;     pseudo-method (:make ARGS BODY) emits make-NAME
;     and adds (:constructor %make-NAME) to OPTS.
;   (new cls k v ...)   -> (make-cls k v ...)

;## Part 1 : Config & Macros -------------------------
(defparameter *the*
  '((p      2            "-p"  "minkowski exponent")
    (file   "auto93.csv" "-f"  "data file")
    (seed   1            "-s"  "seed")
    (budget 50           "-b"  "max labels")
    (few    128          "-u"  "max unlabelled pool")))

;## simplify debugging
#+sbcl (declaim (sb-ext:muffle-conditions
                  warning style-warning))
#+sbcl (setf sb-ext:*invoke-debugger-hook*
             (lambda (c h) (declare (ignore h))
               (format *error-output* "~&[ERROR] ~a~%" c)
               (sb-ext:exit :code 1)))

;## Macros
(defmacro defread (name (stream) &body body)
  "Install BODY as reader-macro for char NAME."
  `(set-macro-character
     ,(character (symbol-name name))
     (lambda (,stream c) (declare (ignore c)) ,@body)
     t))

(defread !(s)
  `(second (assoc ',(read s t nil t) *the*)))

(defread $(s)
  `(slot-value i ',(read s t nil t)))

(defmacro ? (x &rest at)
  "Nested slot access: (? x a b)
   = (slot-value (slot-value x 'a) 'b)."
  (if at `(? (slot-value ,x ',(car at)) ,@(cdr at))
         x))

(defmacro aif (test then &optional else)
  "Anaphoric if: bind `it` to TEST in THEN/ELSE."
  `(let ((it ,test)) (if it ,then ,else)))

(defmacro fn   (&body b) `(lambda (_)         ,@b))
(defmacro fnn  (&body b) `(lambda (_ __)      ,@b))
(defmacro fnnn (&body b) `(lambda (_ __ ___)  ,@b))

;### OO helpers
(defun acc (c s) (intern (format nil "~A-~A" c s)))

(defmacro new (cls &rest kvs)
  "(new num :goal 0) -> (make-num :goal 0)."
  `(,(intern (format nil "MAKE-~A" cls)) ,@kvs))

(defmacro ren+ (name &body methods)
  "Add methods to an existing struct NAME.

   Each (mname (args...) body...) becomes
     (defmethod mname ((i NAME) args...) body...).

   :make is NOT accepted here -- the constructor is a
   struct-time concern. Use REN to define a struct with
   its make-NAME, then REN+ to add more methods later."
  (when (find :make methods :key #'car)
    (error "REN+ does not accept :make. Use REN."))
  `(progn
     ,@(loop for (m args . body) in methods collect
             `(defmethod ,m ((i ,name) ,@args)
                ,@body))
     ',name))

(defmacro ren (name &rest rest)
  "Defstruct + methods on NAME.

   (ren NAME [DOC] SLOTS METHODS...)

   DOC (optional string) becomes the defstruct docstring.
   SLOTS is the slot list.
   Pseudo-methods:
     (:make ARGS BODY...)     body of make-NAME factory.
     (:opts ((opt ...) ...))  extra defstruct options.
   The macro always adds (:constructor %make-NAME) so the
   private form is available; if no :make is given, a thin
   wrapper (defun make-NAME (&rest args)
            (apply #'%make-NAME args)) is emitted.

   The method list (excluding :make) is forwarded to REN+."
  (let* ((doc       (when (stringp (car rest)) (pop rest)))
         (slots     (pop rest))
         (methods   rest)
         (extra     (find :opts methods :key #'car))
         (methods   (remove :opts methods :key #'car))
         (make-spec (find :make methods :key #'car))
         (methods   (remove :make methods :key #'car))
         (priv      (intern (format nil "%MAKE-~A" name)))
         (pub       (intern (format nil  "MAKE-~A" name)))
         (make-defun
           (if make-spec
               `(defun ,pub ,(cadr make-spec) ,@(cddr make-spec))
               `(defun ,pub (&rest args) (apply #',priv args))))
         (opts (cons `(:constructor ,priv)
                     (and extra (cadr extra)))))
    `(progn
       (defstruct (,name ,@opts)
         ,@(when doc (list doc))
         ,@slots)
       ,make-defun
       (ren+ ,name ,@methods)
       ',name)))

(defmacro for/ (expr &rest cs)
  "List comprehension. Nil results are skipped.

   (for/ EXPR for var in lst [if TEST] ...)

   E.g.  (for/ (* x x) for x in '(1 2 3 4 5) if (oddp x))
                 ==> (1 9 25)"
  (labels ((walk (cs)
             (cond ((null cs)
                    `(let ((v ,expr)) (if v (list v) nil)))
                   ((eq (car cs) 'for)
                    `(loop for ,(cadr cs) in ,(cadddr cs)
                           append ,(walk (cddddr cs))))
                   ((eq (car cs) 'if)
                    `(if ,(cadr cs) ,(walk (cddr cs)) nil)))))
    (walk cs)))

(ren sym "Symbolic column: count + frequency table."
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

  (mid ()
    "Mode: most-frequent value."
    (labels ((most (a b) (if (> (cdr a) (cdr b)) a b)))
      (car (reduce #'most $has))))

  (spread ()
    "Entropy of the frequency distribution."
    (labels ((fn (p) (- p (log p 2))))
      (loop for (_ . v) in $has sum (fn (/ v $n)))))

  (norm (v)
    "Symbols are not normalized; return V."
    v)

  (distx (a b)
    "0 if A=B else 1 (Hamming-style)."
    (if (equal a b) 0 1))

  (go-left? (v cut)
    "Branch left if V is missing or matches CUT."
    (or (eq v '?) (equal v cut)))

  (%cuts (rs)
    "Candidate cuts for sym = each known value."
    (declare (ignore rs))
    (mapcar #'car $has)))

(ren num "Numeric column: Welford count, mean, m2, goal."
    ((at 0) (txt " ") (n 0) (mu 0) (m2 0) (goal 1))

  (:make (&rest args
          &aux (i (apply #'%make-num args)))
    "Build a num; goal=0 if txt ends in '-', else 1."
    (setf $goal (if (eq #\- (ch $txt -1)) 0 1))
    i)

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

  (mid ()
    "Running mean."
    $mu)

  (spread ()
    "Running standard deviation."
    (if (< $n 2) 0
        (sqrt (/ (max 0 $m2) (1- $n)))))

  (norm (v)
    "Logistic z-score in [0,1] using current mu+sd."
    (if (eq v '?) v
        (let ((z (/ (- v $mu) (+ (spread i) 1e-32))))
          (/ 1 (+ 1 (exp (* -1.7 (max -3 (min 3 z)))))))))

  (distx (a b)
    "Normalized abs diff with missing-value handling."
    (let ((a (norm i a)) (b (norm i b)))
      (cond ((eq a '?) (abs (- b (if (> b 0.5) 0 1))))
            ((eq b '?) (abs (- a (if (> a 0.5) 0 1))))
            (t         (abs (- a b))))))

  (go-left? (v cut)
    "Branch left if V is missing or <= CUT."
    (or (eq v '?) (<= v cut)))

  (%cuts (rs)
    "Single candidate cut: median of the column."
    (let ((vs (loop for r in rs for v = (elt r $at)
                    unless (eq v '?) collect v)))
      (and vs
           (list (elt (sort vs #'<)
                      (floor (length vs) 2)))))))

(ren cols "Header-derived collection: names, x-cols, y-cols, all."
    (names x y all)

  (:make (txts
          &aux (i (%make-cols :names txts)) (n -1))
    "Parse header TXTS; partition columns into x/y/skip."
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

  (add (row &optional (w 1))
    "Fold ROW (weight W) into every column."
    (mapcar (fnn (add _ __ w)) $all row)
    row))

(ren data "Table: rows + their parsed columns + cached row-mid."
    (rows cols mid)

  (:make (&optional rows)
    "Build data from ROWS list (first row = header)."
    (adds (cdr rows)
          (%make-data :cols (make-cols (car rows)))))

  (add (row &optional (w 1))
    "Add ROW (weight W) to data; W=-1 removes."
    (setf $mid nil)
    (add $cols row w)
    (if (plusp w)
        (push row $rows)
        (setf $rows
              (remove row $rows :test #'equal :count 1)))
    row)

  (mid ()
    "Cached centroid: per-column mid."
    (or $mid
        (setf $mid (mapcar #'mid (? i cols all)))))

  (distx (r1 r2)
    "Minkowski feature-distance between rows R1 and R2."
    (dist (? i cols x)
       (lambda (i &aux (a (elt r1 $at)) (b (elt r2 $at)))
         (if (and (eq a '?) (eq b '?))
           1
           (distx i a b)))))

  (disty (row &optional (cols (? i cols y)))
    "Goal-distance: 0 = perfect, 1 = worst."
    (dist cols
       (lambda (i) (abs (- (norm i (elt row $at)) $goal)))))

  (node (rs &optional (hdr ""))
    "Build a tree node: cloned data + ynum summary + header."
    (%make-tree
      :d    (make-data (cons (? i cols names) rs))
      :ynum (adds (mapcar (fn (disty i _)) rs))
      :hdr  hdr))

  (split (c cut rs)
    "Score one candidate split: (score c cut left-rs right-rs)."
    (let (l r (ln (make-num)) (rn (make-num)))
      (dolist (row rs)
        (if (go-left? c (elt row (? c at)) cut)
            (progn (push row l) (add ln (disty i row)))
            (progn (push row r) (add rn (disty i row)))))
      (list (+ (* (? ln n) (spread ln))
               (* (? rn n) (spread rn)))
            c cut l r)))

  (splits (rs leaf)
    "All splits where both sides have at least LEAF rows."
    (loop for c in (? i cols x)
          append (loop for cut in (%cuts c rs)
                       for s = (split i c cut rs)
                       when (>= (min (length (fourth s))
                                     (length (fifth s)))
                                leaf)
                       collect s)))

  (grow (rs &optional (leaf 3) (hdr ""))
    "Recursively grow tree; stop when fewer than 2*LEAF rows."
    (let ((t0 (node i rs hdr)))
      (when (>= (length rs) (* 2 leaf))
        (let ((cands (splits i rs leaf)))
          (when cands
            (let* ((best (extremum-by cands #'first #'<))
                   (c (second best)) (cut (third best)))
              (setf
                (? t0 col) c
                (? t0 cut) cut
                (? t0 left)
                     (grow i (fourth best) leaf
                                  (kid-hdr c cut t))
                (? t0 right)
                      (grow i (fifth best) leaf
                                   (kid-hdr c cut nil)))))))
      t0)))

; ## Part 7 : Trees (grow) ----------------------------
(ren tree "Decision-tree node: data, ynum, split col+cut, kids, hdr."
    (d ynum col cut left right hdr)

  (:opts ((:copier nil)))

  (leaf (r)
    "Route R to its leaf node."
    (if (null $left) i
        (leaf
          (if (go-left? $col (elt r (? i col at)) $cut)
              $left $right)
          r)))

  (show (&optional (lvl 0))
    "Pretty-print: mu, n on LHS; indent + condition on RHS."
    (format t "~&~4,2f (~3d)   ~{~A~}~a~%"
            (mid $ynum) (? i ynum n)
            (loop repeat (max 0 (1- lvl))
                  collect "|.. ")
            $hdr)
    (when $left
      (show $left  (1+ lvl))
      (show $right (1+ lvl)))))

(defun kid-hdr (c cut left?)
  "Display header for a child branch."
  (format nil "~a ~a ~a" (? c txt)
          (if left?
              (if (typep c 'sym) "==" "<=")
              (if (typep c 'sym) "!=" ">"))
          cut))

; ## Part 8 : ACTIVE LEARNING -------------------------
(defun wins (god)
  "Score 0..100 against god truth: 100=best, 50=med."
  (let* ((ys (mapcar (fn (disty god _)) (? god rows)))
         (ds (sort ys #'<))
         (lo (first ds))
         (md (elt ds (floor (length ds) 2))))
    (fn (floor (* 100 (- 1 (/ (- (disty god _) lo)
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
                 (fn (disty lab _))
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
                       :key (fn (disty lab _))))
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
      (let* ((y-god (fn (disty god _)))
             (train-best
              (extremum-by (? lab rows) y-god #'<))
             (tr (grow lab (? lab rows)))
             (ranked
              (sort (copy-list test) #'<
                    :key (fn (mid (? (leaf tr _)
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
    by (fn (nthcdr n _)) do (print x)))

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
    (%stride $rows (fn (disty i _)))))

(defun eg--xdata (&optional (file !file))
  "Sort rows by distx from row 0; stride-print."
  (let* ((i (make-data (read-csv file)))
         (r0 (car $rows)))
    (%stride $rows (fn (distx i r0 _)))))

(defun eg--tree (&optional (file !file))
  "Active-label, grow tree, pretty-print it."
  (let ((rs (read-csv file)))
    (multiple-value-bind (best lab labels) (active rs)
      (declare (ignore best))
      (format t "~&;; labels used: ~d~%" labels)
      (show (grow lab (? lab rows))))))

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

; ## Utilities : ---------------------------------------
; ### Stats helpers
(defun adds (lst &optional (summary (make-num)))
  "Fold LST into SUMMARY via `add`. Returns SUMMARY."
  (dolist (v lst summary)
    (add summary v)))

(defun sub (it v)
  "Remove V from IT (symmetric with `add`)."
  (add it v -1))

(defun dist (lst f &aux (d 0))
  "Minkowski distance: !p-norm of (funcall f v) for v in LST."
  (dolist (v lst (expt (/ d (length lst)) (/ 1 !p)))
    (incf d (expt (funcall f v) !p))))

; ### Random
(defvar *seed* !seed)

(defun rand (&optional (n 1))
  "Reproducible float in [0,n). Advances *seed*."
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 100) &aux (base 1E10))
  "Reproducible integer in [0,n)."
  (floor (* n (/ (rand base) base))))

; ## lists
(defun shuffle (lst &aux (v (coerce lst 'vector)))
  "Fisher-Yates shuffle of LST. Seeded via *seed*."
  (loop for i from (1- (length v)) downto 1 do
    (rotatef (aref v i) (aref v (rint (1+ i)))))
  (coerce v 'list))

; ## characters
(defun ch (s n)
  "Char at N of S. Negative N counts from end."
  (char s (if (minusp n) (+ (length s) n) n)))

; ## strings
(defun cells (s sep)
  "Split S on character SEP into substring list."
  (loop for start = 0 then (1+ pos)
    for pos = (position sep s :start start)
    collect (subseq s start (or pos (length s)))
    while pos))

(defun thing (str
              &aux (v (ignore-errors
                        (read-from-string str))))
  "Coerce STR to number; '? for \"?\"; else string."
  (cond ((numberp v) v)
        ((string= str "?") '?)
        (t str)))

(defun read-csv (file)
  "Read FILE as CSV; coerce cells via `thing`."
  (with-open-file (s file)
    (loop for line = (read-line s nil) while line
      collect (mapcar #'thing (cells line #\,)))))

; ## Tests
(defun run (it &optional arg)
  "Dispatch --flag to EG-FLAG function."
  (let* ((f (if (symbolp it) it
                (intern (format nil
                                "EG~:@(~a~)" it))))
         (n (symbol-name f)))
    (when (and (fboundp f)
               (> (length n) 3)
               (string= n "EG-" :end1 3))
      (setf *seed* !seed)
      (if arg (funcall f arg) (funcall f))
      t)))

; ## os
(defun args ()
  "Argv as list of strings (SBCL/CLISP portable)."
  #+sbcl (cdr sb-ext:*posix-argv*)
  #+clisp ext:*args*)

(defun cli (lsts)
  "Walk argv (flag arg) pairs: dispatch or update."
  (loop for (flag arg) on (args) by #'cddr do
    (unless (run flag (thing arg))
      (aif (find flag lsts
                 :key #'third :test #'equalp)
           (setf (second it) (thing arg))))))

; ## Last : -------------------------------------------
(cli *the*)
