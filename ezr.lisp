; ezr.lisp -- options and examples, built on lib.lisp.
; (c) 2026 Tim Menzies, timm@ieee.org, MIT license.
; compile-file does not run a plain toplevel `load`, so
; `check` would call all of lib.lisp undefined.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "lib"))

(defun defaults ()
  "Settings: (name flag doc value).  `list`: cli mutates."
  (list (list 'seed   "-s" "random seed" 1234567891)
        (list 'p      "-p" "distance exponent" 2)
        (list 'budget "-b" "labelling budget" 50)
        (list 'check  "-c" "top picks to label" 5)
        (list 'repeats "-R" "holdout: how many repeats" 20)
        (list 'leaf   "-l" "tree: min rows in a leaf" 4)
        (list 'start  "-S" "goods: random labels first" 4)
        (list 'few    "-F" "goods: max rows to consider" 128)
        (list 'file   "-f" "data set file"
              "~/gits/moot/optimize/misc/auto93.csv")))

(setf *settings* (defaults))

;;;; structs
(defstruct num (at 0) (txt "") (n 0) (goal 1) sd
  (mu 0d0 :type double-float) (m2 0d0 :type double-float))
(defstruct sym (at 0) (txt "") (n 0) seen)
(defstruct tbl (n 0) rows cols mids)
(defstruct cols names x y klass all)

(defun num (&optional txt at)
  (make-num :txt txt :at at 
            :goal (if (eql #\- (chars txt -1)) 0 1)))

(defun sym (&optional txt at) (make-sym :txt txt :at at))

(defun tbl (rows)
  (adds (cdr rows) (make-tbl :cols (cols (car rows)))))

(defun col (txt at)
  (if (upper-case-p (chars txt 0)) (num txt at) (sym txt at)))

(defun cols (names &aux (i (make-cols :names names)))
  "Header NAMES.  Last letter: + - goal, ! klass, X skip."
  (setf $all (loop for txt across names for at from 0
                   collect (col txt at)))
  (dolist (col $all i)
    (case (chars (? col txt) -1)
      (#\X)                         ; summarized, not modelled
      ((#\+ #\-) (addend col $y))
      (#\!       (setf $klass col) (addend col $y))
      (t         (addend col $x)))))

(defmethod clone ((i tbl)) (tbl (list (? i cols names))))
(defmethod clone ((i num)) (num $txt $at))
(defmethod clone ((i sym)) (sym $txt $at))

;;;; -----------------------------------------------------------
(defun subs (lst &optional (i (num)))
  "Take every item of LST back out of I (any sequence)."
  (map nil (-> (setf i (sub i %1))) lst)
  i)

(defun adds (lst &optional (i (num)))
  "Add every item of LST to I (any sequence)."
  (map nil (-> (setf i (add i %1))) lst)
  i)

(defun sub (i x) "Take X back out of I." (add i x :inc -1))

(defun add (i x &key (inc 1))
  "Add X to I; a negative INC takes it away again."
  (unless (eql x '?)
    (incf $n inc)
    (if (< $n 1)
      (setf i (clone i)) ; start over 
      (_add i x inc)))
  i)

(defmethod _add ((i sym) x inc)
  "Count X."
  (incf (has x $seen) inc))

(defmethod _add ((i num) x inc)
  "Welford, forwards or backwards; see `div`."
  (let* ((x (float x 1d0)) (d (- x $mu)))
    (incf $mu (* inc (/ d $n)))
    (setf $m2 (max 0d0 (+ $m2 (* inc (* d (- x $mu)))))
          $sd (if (< $n 2) 0d0
                (sqrt (/ $m2 (1- $n)))))))

(defmethod _add ((i cols) row inc)
  "Show each cell of ROW to the column that owns it."
  (setf $all (loop for col in $all
               collect (add col (svref row (? col at))
                                :inc inc))))

(defmethod _add ((i tbl) row inc)
  "Keep ROW, and summarize it in my cols."
  (setf $mids nil)             ; my summary just went stale
  (_add $cols row inc)   ; _add: cols keeps no count
  (if (plusp inc)
    (push row $rows)
    (setf $rows (delete row $rows :test #'eq :count 1))))

;;;; -----------------------------------------------------------
(defmethod mids ((i tbl))
  "Every column's mid, in header order.  Cached till `add`."
  (or $mids (setf $mids (map 'vector #'mid (? $cols all)))))

(defmethod mid ((i num)) "Central tendency: the mean." $mu)

(defmethod mid ((i sym))
  "Central tendency: the most common symbol."
  (car (reduce (-> (if (> (cdr %1) (cdr %2)) %1 %2)) $seen)))

(defmethod div ((i num))
  "Diversity: standard deviation, kept fresh by `_add`."
  (or $sd 0d0))

(defmethod div ((i sym))
  "Diversity: entropy, in bits."
  (- (loop for (nil . n) in $seen
           sum (* (/ n $n) (log (/ n $n) 2)))))

(defmethod norm ((i sym) x) "Nothing to scale." x)

(defmethod norm ((i num) x)
  "X to 0..1, via 1/(1+exp(-1.7 z)); z clamped to +-3."
  (let ((z (max -3 (min 3 (/ (- x $mu) (+ 1d-32 (div i)))))))
    (/ 1 (+ 1 (exp (* -1.7 z))))))

;;;; -----------------------------------------------------------
(defmethod dist ((i sym) a b)
  "0 if the same, else 1.  An unknown is never the same."
  (if (and (eql a '?) (eql b '?)) 1 (if (equal a b) 0 1)))

(defmethod dist ((i num) a b)
  "Gap between norms; an unknown goes to the far pole."
  (if (and (eql a '?) (eql b '?))
    1
    (let ((x (if (eql a '?) nil (norm i a)))
          (y (if (eql b '?) nil (norm i b))))
      (if (null x) (setf x (if (> y 0.5) 0 1)))
      (if (null y) (setf y (if (> x 0.5) 0 1)))
      (abs (- x y)))))

(defun minkowski (seq f &aux (d 0) (n 1d-32) (p (my p)))
  "Sum F over SEQ, p-norm style.  A nil from F is skipped."
  (map nil (-> (let ((v (funcall f %1)))
                 (when v (incf n) (incf d (expt v p)))))
       seq)
  (expt (/ d n) (/ 1d0 p)))

(defun distx (i row1 row2)
  "Minkowski gap between two rows, over the x columns."
  (minkowski (? i cols x) (-> (dist %1 (svref row1 (? %1 at))
                                    (svref row2 (? %1 at))))))

(defun disty (i row)
  "How far ROW's goals sit from the best they could be."
  (minkowski (? i cols y)
             (-> (when (num-p %1)
                   (abs (- (norm %1 (svref row (? %1 at)))
                           (? %1 goal)))))))

;;;; -----------------------------------------------------------
(defun mean (xs) "Mean of XS." (mid (adds xs (num))))

(defun cohen (xs ys &optional (d 0.35) (eps 0))
  "Are the means within D pooled standard deviations?"
  (let* ((a (adds xs (num))) (b (adds ys (num)))
         (pool (/ (+ (* (1- (? a n)) (expt (div a) 2))
                     (* (1- (? b n)) (expt (div b) 2)))
                  (- (+ (? a n) (? b n)) 2))))
    (<= (abs (- (mid a) (mid b)))
        (max eps (* d (sqrt pool))))))

(defun cliffs (xs ys &optional (d 0.197))
  "Sorted in.  Is the rank imbalance small enough?"
  (let ((gt 0) (lt 0) (j 0) (k 0)
        (n (length xs)) (m (length ys)))
    (loop for x across xs do
      (loop while (and (< j m) (< (aref ys j) x))
        do (incf j) (setf k j))
      (loop while (and (< k m) (<= (aref ys k) x)) do (incf k))
      (incf gt j)
      (incf lt (- m k)))
    (<= (/ (abs (- gt lt)) (* n m)) d)))

(defun ks (xs ys &optional (a 1.36))
  "Sorted in.  95% Kolmogorov-Smirnov."
  (let ((n (length xs)) (m (length ys)) (i 0) (j 0) (d 0))
    (loop while (and (< i n) (< j m)) do
      (let ((v (min (aref xs i) (aref ys j))))
        (loop while (and (< i n) (<= (aref xs i) v)) do (incf i))
        (loop while (and (< j m) (<= (aref ys j) v)) do (incf j))
        (setf d (max d (abs (- (/ i n) (/ j m)))))))
    (<= d (* a (sqrt (/ (+ n m) (* n m)))))))

(defun same (xs ys &optional (eps 0))
  "Indistinguishable by all three tests."
  (let ((xs (sort (coerce xs 'vector) #'<))
        (ys (sort (coerce ys 'vector) #'<)))
    (and (cliffs xs ys) (ks xs ys) (cohen xs ys 0.35 eps))))

;;;; -----------------------------------------------------------
(defun good (i best rest row)
  "Score ROW: near BEST's middle, far from REST's."
  (- (distx i row (mids rest)) (distx i row (mids best))))

(defun good-enough (both best rest row lab)
  "Label ROW; keep BEST near sqrt(n), worst goes to REST."
  (let ((r (funcall lab row)))
    (add both r)
    (add best r)
    (when (> (tbl-n best) (sqrt (+ 1 (tbl-n best) (tbl-n rest))))
      (let ((w (reduce 
                 (-> (if (> (disty both %1) (disty both %2))
                             %1 %2))
                 (tbl-rows best))))
        (sub best w)
        (add rest w)))))

(defun goods (i &key (lab (-> %1)) (score #'good)
                (start (my start)) (few (my few))
                (budget (- (my budget) (my check))))
  "Label by SCORE till budget gone.  Best rows first."
  (let* ((rows (shuffle (tbl-rows i)))
         (todo (subseq rows 0 (min few (length rows))))
         (best (clone i))
         (rest (clone i))
         (both (clone i)))
    (dotimes (_ start)
      (good-enough both best rest (pop todo) lab))
    (loop while (and todo (< (tbl-n both) budget))
      do (setf todo
           (sort todo #'>
             :key (-> (funcall score i best rest %1))))
      (good-enough both best rest (pop todo) lab))
    (sort (tbl-rows both) #'< :key (-> (disty both %1)))))

;;;; -----------------------------------------------------------
(defun cuts-keeper (enough &aux sharp)
  "Closure: feed it (l r col v); no args = the sharpest.
   Cuts leaving under ENOUGH rows either side are ignored."
  (labels
    ((xpect (a b)                    ; diversity, by size
       (/ (+ (* (div a) (? a n)) (* (div b) (? b n)))
          (+ (? a n) (? b n) 1d-32))))
    (lambda (&optional l r col v &aux s)
      (when (and l (>= (min (? l n) (? r n)) enough))
        (setf s (xpect l r))
        (if (or (null sharp) (< s (first sharp)))
          (setf sharp (list s col v))))
      sharp)))

(defun cuts (i &optional (rows (? i rows))
                   (y (-> (disty i %1))))
  "Sharpest (score col v) split of ROWS, over the x cols."
  (let ((keep (cuts-keeper (sqrt (length rows))))
        (ys   (->> (funcall y %1) rows)))       ; y once, not
    (dolist (col (? i cols x) (funcall keep))   ; once per col
      (cuts-loop col (loop for r in rows for v in ys
                       unless (eql '? (svref r (? col at)))
                       collect (cons (svref r (? col at)) v))
                 keep))))

(defmethod cuts-loop ((col num) xy keep &aux (lhs (num)) rhs)
  "One sweep: RHS shrinks as LHS grows.  Offer each edge."
  (setf xy  (sort xy #'< :key #'car)
        rhs (adds (mapcar #'cdr xy) (num)))
  (loop for (a b) on xy do
    (add lhs (cdr a))
    (setf rhs (sub rhs (cdr a)))
    (if (and b (/= (car a) (car b)))
      (funcall keep lhs rhs col (car a)))))

(defmethod cuts-loop ((col sym) xy keep &aux (all (num))
                 (bins (make-hash-table :test #'equal)))
  "One pass to bin the ys, then one cut per symbol."
  (dolist (p xy)
    (add all (cdr p))
    (push (cdr p) (gethash (car p) bins)))
  (maphash
    (-> (funcall keep (adds %2 (num))          ; this symbol
                      (subs %2 (copy-num all)) ; all others
                      col %1))
    bins))

;;;; -----------------------------------------------------------
(defstruct node rows col v kids mu ys)

(defun selects (col v row &aux (x (svref row (? col at))))
  "Does ROW fall on the left of this cut?  `?` goes both."
  (or (eql x '?) (if (num-p col) (<= x v) (equal x v))))

(defun split (col v rows)
  "ROWS in two: those this cut selects, and the others."
  (loop for r in rows
    if (selects col v r) collect r into yes
    else collect r into no
    finally (return (values yes no))))

(defun grow (i rows &optional (y (-> (disty i %1))) &aux
             (z  (and (> (length rows) (my leaf))
                      (cuts i rows y)))
             (me (make-node :rows rows :mu (ymu i rows)
                            :ys (ymids i rows))))
  (multiple-value-bind (yes no)
      (if z (split (second z) (third z) rows))
    (when (and yes no)
      (setf (? me col)  (second z)
            (? me v)    (third z)
            (? me kids) (list (grow i yes y) (grow i no y))))
    me))

(defun leaves (z)
  "Every childless node under Z."
  (if (? z kids) (mapcan #'leaves (? z kids)) (list z)))

(defun ymu (i rows)
  "Mean d2h of ROWS."
  (mid (adds (->> (disty i %1) rows) (num))))

(defun ymids (i rows)
  "Mid of each y column, over ROWS."
  (->> (mid (adds (loop for r in rows collect(svref r (? %1 at)))
                  (num)))
       (? i cols y)))

(defun leaf (z row)
  "Walk ROW down the tree to its leaf."
  (if (? z kids)
    (leaf (if (selects (? z col) (? z v) row)
            (first (? z kids))
            (second (? z kids)))
          row)
    z))

(defun wins (i &aux
             (ys (sort (->> (disty i %1) (? i rows)) #'<))
             (lo (first ys))
             (b4 (mid (adds ys (num)))))
  "A scorer: 100 at the best row, 0 at the mean row."
  (-> (max -100
           (min 100 (* 100 (- 1 (/ (- (disty i %1) lo)
                                   (+ 1d-32 (- b4 lo)))))))))

(defun grabs (i &aux (rows (shuffle (? i rows))))
  "Budget-less-check rows, taken at random.  The straw man."
  (subseq rows 0 (min (length rows)
                      (- (my budget) (my check)))))

(defun holdout (i &key (pick #'goods) &aux
                (rows (shuffle (? i rows))))
  "Train on half; of CHECK guesses on the rest, pick best."
  (let* ((n     (floor (length rows) 2))
         (train (subseq rows 0 (min n (my few))))
         (test  (subseq rows n))
         (tr    (adds train (clone i)))
         (lab   (adds (funcall pick tr) (clone i)))
         (tt    (grow lab (? lab rows)))
         (top   (subseq (sort test #'<
                          :key (-> (? (leaf tt %1) mu)))
                        0 (my check))))
    (first (sort top #'< :key (-> (disty tr %1))))))

(defun show (i z &optional pre (edge "")
               (ls (sort (leaves z) #'< :key (-> (? %1 mu)))))
  "Tree left; y mids, d2h, n right.  Tick best, cross worst."
  (unless pre
    (format t "~&  ~34a~{~7@a~}~7@a~5@a~%" ""
      (mapcar (-> (? %1 txt)) (? i cols y)) "d2h" "n"))
  (format t "~&~a ~34a~{~7d~}~7d~5d~%"
    (cond ((eq z (first ls))      (code-char 10003))
          ((eq z (car (last ls))) (code-char 10007))
          (t #\Space))
    (concatenate 'string (or pre "") edge)
    (mapcar #'round (? z ys))
    (round (* 100 (? z mu))) (length (? z rows)))
  (loop for k in (? z kids)
    for op in (if (num-p (? z col)) '("<=" ">") '("=" "!="))
    do (show i k (if pre (concatenate 'string pre "|  ") "")
             (format nil "~a ~a ~a" (? (? z col) txt) op
               (? z v))
             ls)))

;;;; -----------------------------------------------------------
(defun similar (x y &optional (eps 1e-5))
  "Do X and Y match?  Numbers, within a relative EPS."
  (if (numberp x)
    (< (abs (- x y)) (* eps (max 1 (abs y))))
    (equal x y)))

(defun gauss (m sd)
  "One sample from a normal curve, via Box-Muller."
  (+ m (* sd (sqrt (* -2 (log (rand 1.0))))
            (cos (* 2 pi (rand 1.0))))))

;;;; -----------------------------------------------------------
(defun eg--the ()
  (dolist (o *settings*)
    (destructuring-bind (key flag doc val) o
      (format t "~&  ~8a ~(~a~)=~a~28t~a~%" flag key val doc)))
  (assert (numberp (my seed))))

(defun eg--rand ()
  (let ((a (rand-int 100)) (b (rand-int 100)))
    (kv :rand-ints (list a b) :seed (my seed))
    (assert (and (<= 0 a 99) (<= 0 b 99)))))

(defun eg--macros ()
  (let (c)
    (dolist (x '(a b a)) (incf (has x c)))
    (kv :has c :arrow (funcall (-> (+ %1 1)) 1))
    (assert (equal c '((b . 1) (a . 2))))
    (assert (eql 2 (funcall (-> (+ %1 1)) 1)))))

(defun eg--settings () 
  (format t"~&~a ~a~%" (my p) *settings*))

(defun eg--csv () 
  (let ((lst (csv (my file)))) (print (last lst))))

(defun eg--tbl () 
  (print (? (tbl (csv (my file))) cols x)))

(defun eg--stats ()
  "Mid and div, per column."
  (dolist (c (? (? (tbl (csv (my file))) cols) all))
    (format t "~&  ~10a mid=~9,2f div=~8,3f~%"
      (? c txt) (float (mid c)) (float (div c)))))

(defun eg--gauss ()
  "Sampling a normal curve reproduces its mid and div."
  (let ((n (num)))
    (dotimes (i 1000) (add n (gauss 10 2)))
    (kv :mid (mid n) :div (div n))
    (assert (similar (mid n) 10 0.05))
    (assert (similar (div n) 2  0.25))))

(defun eg--sub ()
  "Mids noted halfway, then halfway back, must agree."
  (let* ((rows (csv (my file)))
         (i    (tbl (list (pop rows))))  ; header off the front
         (half (floor (length rows) 2))
         (was  nil))
    (loop for r in rows for k from 1 do
      (add i r)
      (if (= k half) (print (setf was (mids i)))))
    (dolist (r (reverse (nthcdr half rows)))
      (sub i r))          ; newest first: cheap
    (->> (assert (similar %1 %2)) (print (mids i)) was)))

(defun eg--acq ()
  "Goods spends its budget, and beats a random draw."
  (let* ((i    (tbl (csv (my file))))
         (rows (tbl-rows i))
         (got  (goods i))
         (any  (loop repeat (length got)
                     minimize
                     (disty i (nth (rand-int (length rows))
                                   rows)))))
    (kv :labelled (length got) :goods (disty i (first got))
        :random any)
    (assert (= (length got) (- (my budget) (my check))))
    (assert (<= (disty i (first got)) any))))

(defun eg--cuts ()
  "The sharpest cut lowers diversity below the whole."
  (let* ((i    (tbl (csv (my file))))
         (rows (tbl-rows i))
         (all  (adds (->> (disty i %1) rows) (num))))
    (destructuring-bind (s c v) (cuts i)
      (multiple-value-bind (yes no) (split c v rows)
        (kv :col (? c txt) :at v :was (div all) :now s)
        (assert (and yes no))
        (assert (< s (div all)))))))

(defun eg--tree ()
  "Grow a tree on labelled rows; its best leaf beats root."
  (let* ((i  (tbl (csv (my file))))
         (lab (adds (goods i) (clone i)))    ; what we bought
         (z   (grow lab (? lab rows)))
         (ls (sort (leaves z) #'< :key (-> (? %1 mu)))))
    (show i z)
    (assert (> (length ls) 1))
    (assert (< (? (first ls) mu) (? z mu)))
    (assert (> (? (car (last ls)) mu) (? z mu)))))

(defun winsn (i &aux (w (wins i)))
  "REPEATS holdouts, scored and sorted."
  (sort (loop repeat (my repeats)
              collect (round (funcall w (holdout i))))
        #'<))

(defun eg--wins ()
  "Every holdout score, sorted."
  (format t "~&~{~4d~}~%" (winsn (tbl (csv (my file))))))

(defun eg--holdout ()
  "Mean holdout score, then the data set name."
  (let* ((ws (winsn (tbl (csv (my file)))))
         (mu (round (/ (reduce #'+ ws) (length ws)))))
    (format t "~&~4d ~a~%" mu (file-namestring (my file)))
    (assert (> mu 0))))

(defun eg--same ()
  "The stats tell noise from signal."
  (let ((x (loop repeat 40 collect (gauss 10 1)))
        (y (loop repeat 40 collect (gauss 10 1)))
        (z (loop repeat 40 collect (gauss 11 1))))
    (kv :same (same x y) :differ (not (same x z)))
    (assert (same x y))
    (assert (not (same x z)))
    (assert (same x x))))

(defun eg--vs ()
  "Goods versus random picks, same budget. If 'same', delta=0."
  (let* ((i (tbl (csv (my file))))
         (w (wins i))
         (a (loop repeat (my repeats)
              collect (funcall w (holdout i :pick #'goods))))
         (b (loop repeat (my repeats)
              collect (funcall w (holdout i :pick #'grabs)))))
    (format t "~&~4d ~4d ~4d ~a~%"
      (round (mean a)) (round (mean b))
      (if (same a b) 0 (round (- (mean a) (mean b))))
      (file-namestring (my file)))))

(defun eg--all (&aux (fails 0) egs)
  (do-symbols (s *package*)
    (if (and (fboundp s) (eql 0 (search "EG--" (symbol-name s)))
             (not (eql s 'eg--all)))
      (pushnew s egs)))
  (dolist (f (sort egs #'string< :key #'symbol-name))
    (setf fails (cli-run f fails)))
  (assert (zerop fails))
  fails)

;;;; -----------------------------------------------------------
;;;; main
(cli #'defaults)
