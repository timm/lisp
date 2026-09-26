; ezr.lisp -- options and examples, built on lib.lisp.
; (c) 2026 Tim Menzies, timm@ieee.org, MIT license.
;
;   lisp ezr -s 42 --all
;
; -s 42 sets an option; --foo runs (eg--foo) with a fresh
; seed. Exit code = failure count.

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
        (list 'start  "-S" "goods: random labels first" 4)
        (list 'few    "-F" "goods: max rows to consider" 256)
        (list 'file   "-f" "data set file"
              "~/gits/moot/optimize/misc/auto93.csv")))

(setf *settings* (defaults))

;;;; structs
(defstruct num (at 0) (txt "") (n 0) (mu 0) (m2 0) (goal 1))
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
  "Take every item of LST back out of I."
  (dolist (x lst i) (setf i (sub i x))))

(defun adds (lst &optional (i (num)))
  "Add every item of LST to I, keeping what `add` returns."
  (dolist (x lst i) (setf i (add i x))))

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
  (let ((d (- x $mu)))
    (incf $mu (* inc (/ d $n)))
    (incf $m2 (* inc (* d (- x $mu))))))

(defmethod _add ((i cols) row inc)
  "Show each cell of ROW to the column that owns it."
  (setf $all (loop for col in $all
               collect (add col (elt row (? col at)) :inc inc))))

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
  "Diversity: standard deviation, out of Welford's M2."
  (if (< $n 2) 0 (sqrt (/ $m2 (1- $n)))))

(defmethod div ((i sym))
  "Diversity: entropy, in bits."
  (- (loop for (nil . n) in $seen
           sum (* (/ n $n) (log (/ n $n) 2)))))

(defmethod norm ((i sym) x) "Nothing to scale." x)

(defmethod norm ((i num) x)
  "X to 0..1, via 1/(1+exp(-1.702 z)): no lo/hi needed."
  (let ((sd (div i)))
    (if (zerop sd) 0.5 
      (/ 1 (+ 1 (exp (* -1.7 (/ (- x $mu) sd))))))))

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

(defun xdist (i row1 row2 &aux (d 0) (n 0) (p (my p)))
  "Minkowski gap between two rows, over the x columns."
  (dolist (col (? i cols x))
    (incf n)
    (incf d (expt (dist col (elt row1 (? col at))
                            (elt row2 (? col at)))
                  p)))
  (expt (/ d n) (/ 1 p)))

(defun ydist (i row &aux (d 0) (n 0) (p (my p)))
  "How far ROW's goals sit from the best they could be."
  (dolist (col (? i cols y))
    (when (num-p col)
      (incf n)
      (incf d (expt (abs (- (norm col (elt row (? col at)))
                            (? col goal)))
                    p))))
  (expt (/ d n) (/ 1 p)))

;;;; -----------------------------------------------------------
(defun good (i best rest row)
  "Score ROW: near BEST's middle, far from REST's."
  (- (xdist i row (mids rest)) (xdist i row (mids best))))

(defun good-enough (both best rest row lab)
  "Label ROW; keep BEST near sqrt(n), worst goes to REST."
  (let ((r (funcall lab row)))
    (add both r)
    (add best r)
    (when (> (tbl-n best) (sqrt (+ 1 (tbl-n best) (tbl-n rest))))
      (let ((w (reduce 
                 (-> (if (> (ydist both %1) (ydist both %2))
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
    (sort (tbl-rows both) #'< :key (-> (ydist both %1)))))

;;;; -----------------------------------------------------------
(defun cut (enough &aux sharp)
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

(defmethod chop ((col num) xy keep &aux (lhs (num)) rhs)
  "One sweep: RHS shrinks as LHS grows.  Offer each edge."
  (setf xy  (sort xy #'< :key #'car)
        rhs (adds (mapcar #'cdr xy) (num)))
  (loop for (a b) on xy do
    (add lhs (cdr a))
    (setf rhs (sub rhs (cdr a)))
    (if (and b (/= (car a) (car b)))
      (funcall keep lhs rhs col (car a)))))

(defmethod chop ((col sym) xy keep &aux (all (num))
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

(defun cuts (i &optional (rows (? i rows))
                         (y (-> (ydist i %1))))
  "Sharpest (score col v) split of ROWS, over the x cols."
  (let ((keep (cut (sqrt (length rows))))
        (ys   (->> (funcall y %1) rows)))       ; y once, not
    (dolist (col (? i cols x) (funcall keep))   ; once per col
      (chop col
            (loop for r in rows for v in ys
                  unless (eql '? (elt r (? col at)))
                  collect (cons (elt r (? col at)) v))
            keep))))

(defun same (x y &optional (eps 1e-5))
  "Do X and Y match?  Numbers, within a relative EPS."
  (if (numberp x)
    (< (abs (- x y)) (* eps (max 1 (abs y))))
    (equal x y)))

(defun gauss (m sd)
  "One sample from a normal curve, via Box-Muller."
  (+ m (* sd (sqrt (* -2 (log (rand 1.0))))
            (cos (* 2 pi (rand 1.0))))))

;;;; egs

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
    (assert (same (mid n) 10 0.05))
    (assert (same (div n) 2  0.25))))

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
    (->> (assert (same %1 %2)) (print (mids i)) was)))

(defun eg--acq ()
  "Goods spends its budget, and beats a random draw."
  (let* ((i    (tbl (csv (my file))))
         (rows (tbl-rows i))
         (got  (goods i))
         (any  (loop repeat (length got)
                     minimize
                     (ydist i (nth (rand-int (length rows))
                                   rows)))))
    (kv :labelled (length got) :goods (ydist i (first got))
        :random any)
    (assert (= (length got) (- (my budget) (my check))))
    (assert (<= (ydist i (first got)) any))))

(defun eg--all (&aux (fails 0) egs)
  (do-symbols (s *package*)
    (if (and (fboundp s) (eql 0 (search "EG--" (symbol-name s)))
             (not (eql s 'eg--all)))
      (pushnew s egs)))
  (dolist (f (sort egs #'string< :key #'symbol-name) fails)
    (setf fails (cli-run f fails))))

;;;; main file

(cli #'defaults)
