;;;; ezr1plus.lisp : CL port of ezr.py, built on plus.lisp.

(load "plus.lisp")

(setf *the*
  '((budget 50) (check 5) (start 4) (few 128)
    (leaf 3) (p 2) (seed 1) (show 30) (decimals 2)
    (bm 2) (bk 1)
    (cliffs 0.195) (eps 0.35) (ksconf 1.36)
    (file "data.csv")))
(setf *seed* @seed)

(defmacro has (x lst)
  "Alist counter: (incf (has v xs))."
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x 0) ,lst))))))

(defun read-csv (file)
  "CSV reader: strip # comments, skip blanks, coerce cells."
  (with-open-file (s file)
    (loop for line = (read-line s nil) while line
          for clean = (subseq line 0
                        (or (position #\# line) (length line)))
          when (loop for c across clean
                     thereis (not (member c '(#\Space #\Tab))))
          collect (mapcar #'thing (cells clean #\,)))))

;;; ## Structs ---------------------------------------------

(plus col ((txt "") (at 0) (n 0)))

(plus num ((mu 0d0) (m2 0d0) (sd 0d0) (heaven 1))
  (:opts ((:include col)))
  (make (&optional (txt "") (at 0)
         &aux (i (%make-num :txt txt :at at)))
    (setf $heaven
          (if (and (plusp (length txt))
                   (eql (ch txt -1) #\-)) 0 1))))

(plus sym ((has nil)) (:opts ((:include col)))
  (make (&optional (txt "") (at 0)
         &aux (i (%make-sym :txt txt :at at)))))

(plus cols ((xs nil) (ys nil) (all nil) (names nil) (klass nil))
  (make (names &aux (i (%make-cols :names names)))
    (loop for s in names for k from 1
          for c = (if (upper-case-p (ch s 0))
                      (make-num s k) (make-sym s k))
          do (push c $all)
             (unless (eql (ch s -1) #\X)
               (when (eql (ch s -1) #\!) (setf $klass c))
               (if (find (ch s -1) "+-!" :test #'eql)
                   (push c $ys) (push c $xs))))))

(plus data ((rows nil) (cols nil) (centroid nil))
  (make (src &aux (i (%make-data)))
    (loop for row in (if (stringp src) (read-csv src) src)
          do (add i row))))

(plus tnode ((score nil) (y nil) (mids nil) (col nil)
             (cut nil) (at nil) (left nil) (right nil)))

;;; ## Num/Sym methods -------------------------------------

(plus+ num
  (add (v &optional (w 1))
    (unless (eql v '?)
      (cond ((and (minusp w) (<= $n 2))
             (setf $n 0 $mu 0d0 $m2 0d0 $sd 0d0))
            (t (incf $n w)
               (let ((err (- v $mu)))
                 (incf $mu (/ (* w err) $n))
                 (incf $m2 (* w err (- v $mu))))
               (setf $sd (if (> $n 1)
                             (sqrt (max 0 (/ $m2 (1- $n)))) 0)))))
    v)
  (mid () $mu)
  (spread () $sd)
  (norm (v)
    (if (eql v '?) v
        (/ 1 (1+ (exp (* -1.7 (max -3 (min 3
                       (/ (- v $mu) (+ $sd 1e-32))))))))))
  (like (v prior) (declare (ignore prior))
    (let ((z (* 2 (+ $sd 1e-32) (+ $sd 1e-32))))
      (/ (exp (- (/ (expt (- v $mu) 2) z))) (sqrt (* pi z)))))
  (pick (&optional v)
    (let* ((base (if (or (null v) (eql v '?)) $mu v))
           (lo (- $mu (* 3 $sd))) (hi (+ $mu (* 3 $sd)))
           (new (+ base (* $sd 2 (- (+ (rand) (rand) (rand)) 1.5)))))
      (+ lo (mod (- new lo) (+ (- hi lo) 1e-32)))))
  (splits (rows score)
    (let* ((vs (sort (loop for r in rows
                           for v = (nth (1- $at) r)
                           unless (eql v '?) collect v)
                     #'<))
           (mu (and (>= (length vs) 2)
                    (nth (floor (length vs) 2) vs))))
      (when mu
        (let ((cut (split-rows i rows score mu (f+ (<= _ mu)))))
          (and cut (list cut)))))))

(plus+ sym
  (add (v &optional (w 1))
    (unless (eql v '?)
      (incf $n w)
      (incf (has v $has) w))
    v)
  (mid ()
    (loop for (k . v) in $has
          with best = -1 with chosen = nil
          when (> v best) do (setf best v chosen k)
          finally (return chosen)))
  (spread ()
    (- (loop for (_ . v) in $has
             sum (* (/ v $n) (log (/ v $n) 2)))))
  (like (v prior)
    (/ (+ (or (cdr (assoc v $has :test #'equal)) 0) (* @bk prior))
       (+ $n @bk)))
  (pick (&optional v) (declare (ignore v))
    (let ((n (* (rand) $n)))
      (loop for (k . cnt) in $has do
        (decf n cnt) (when (<= n 0) (return k)))))
  (splits (rows score)
    (let ((seen (make-hash-table :test 'equal)) out)
      (loop for r in rows
            for v = (nth (1- $at) r)
            unless (or (eql v '?) (gethash v seen))
            do (setf (gethash v seen) t)
               (let ((cut (split-rows i rows score v (f+ (equal _ v)))))
                 (when cut (push cut out))))
      out)))

(defun mode (dct) (caar (sort (copy-list dct) #'> :key #'cdr)))

(defun entropy (dct &aux (n (loop for (_ . v) in dct sum v)))
  (- (loop for (_ . v) in dct sum (* (/ v n) (log (/ v n) 2)))))

;;; ## Data ops --------------------------------------------

(defun aha (c u v)
  "Distance between two values on col c."
  (cond ((and (eql u '?) (eql v '?)) 1)
        ((typep c 'sym) (if (equal u v) 0 1))
        (t (let ((u2 (norm c u)) (v2 (norm c v)))
             (when (eql u2 '?) (setf u2 (if (> v2 0.5) 0 1)))
             (when (eql v2 '?) (setf v2 (if (> u2 0.5) 0 1)))
             (abs (- u2 v2))))))

(plus+ data
  (add (row &optional (w 1))
    (setf $centroid nil)
    (cond ((null $cols) (setf $cols (make-cols row)))
          (t (loop for c in (? $cols all) do
               (add c (nth (1- (? c at)) row) w))
             (when (plusp w) (push row $rows)))))
  (clone (&optional rs &aux (i2 (%make-data)))
    (add i2 (? $cols names))
    (loop for r in rs do (add i2 r))
    i2)
  (mids ()
    (or $centroid
        (setf $centroid
              (loop for c in (? $cols all) collect (mid c)))))
  (disty (row)
    (let ((ys (? $cols ys)))
      (expt (/ (loop for c in ys
                     sum (expt (abs (- (norm c (nth (1- (? c at)) row))
                                       (? c heaven))) @p))
               (length ys))
            (/ 1 @p))))
  (distx (r1 r2)
    (let ((xs (? $cols xs)))
      (expt (/ (loop for c in xs
                     sum (expt (aha c (nth (1- (? c at)) r1)
                                      (nth (1- (? c at)) r2))
                               @p))
               (length xs))
            (/ 1 @p))))
  (nearest (row &optional (rs $rows))
    (let (best (bd most-positive-double-float))
      (loop for r in rs
            for d = (distx i row r)
            when (< d bd) do (setf best r bd d))
      best))
  (likes (row nr nk
          &aux (prior (/ (+ (length $rows) @bm)
                         (+ nr (* @bm nk)))))
    (+ (log prior)
       (loop for c in (? $cols xs)
             for v = (nth (1- (? c at)) row)
             for l = (and (not (eql v '?)) (like c v prior))
             when (and l (> l 0)) sum (log l))))
  (picks (row &optional (n 1))
    (let* ((xs (? $cols xs))
           (s  (copy-list row))
           (k  (min n (length xs))))
      (dolist (c (subseq (shuffle xs) 0 k) s)
        (setf (nth (1- (? c at)) s)
              (pick c (nth (1- (? c at)) s)))))))

(defun extrapolate (cols a b c &optional (F 0.5)
                    &aux (out (copy-list a)))
  "DE blend: new = a + F*(b-c). Clamp Num. Sym: pick b prob F."
  (dolist (col cols out)
    (let* ((k  (1- (? col at)))
           (va (nth k a)) (vb (nth k b)) (vc (nth k c)))
      (setf (nth k out)
            (cond ((eql va '?) '?)
                  ((typep col 'num)
                   (if (or (eql vb '?) (eql vc '?)) va
                       (let* ((v (+ va (* F (- vb vc))))
                              (lo (- (? col mu) (* 4 (? col sd))))
                              (hi (+ (? col mu) (* 4 (? col sd)))))
                         (max lo (min hi v)))))
                  (t (if (and (not (eql vb '?)) (< (rand) F))
                         vb va)))))))

;;; ## Tree ------------------------------------------------

(defun split-rows (col rows score cut test)
  (let ((lhs (make-num)) (rhs (make-num))
        (left nil) (right nil))
    (loop for row in rows
          for v = (nth (1- (? col at)) row)
          for ok = (or (eql v '?) (! test v))
          do (if ok (push row left) (push row right))
             (add (if ok lhs rhs) (! score row)))
    (when (and (>= (length left)  @leaf)
               (>= (length right) @leaf))
      (list :col col :cut cut :left left :right right
            :lhs lhs :rhs rhs))))

(defun best-split (d rows score)
  (let (best (bw most-positive-double-float))
    (loop for c in (? d cols xs) do
      (loop for cut in (splits c rows score)
            for lhs = (getf cut :lhs)
            for rhs = (getf cut :rhs)
            for w = (+ (* (? lhs n) (spread lhs))
                       (* (? rhs n) (spread rhs)))
            when (< w bw) do (setf best cut bw w)))
    best))

(defun tree-build (score d rows)
  (let ((i (make-tnode :score score)))
    (setf $y (adds (mapcar score rows))
          $mids (mids (clone d rows)))
    (when (>= (length rows) (* 2 @leaf))
      (let ((b (best-split d rows score)))
        (when b
          (setf $col   (getf b :col)
                $cut   (getf b :cut)
                $at    (? $col at)
                $left  (tree-build score d (getf b :left))
                $right (tree-build score d (getf b :right))))))
    i))

(defun tree-leaf (i row)
  (if (null $col) i
      (let ((v (nth (1- $at) row)))
        (tree-leaf
          (cond ((eql v '?) $left)
                ((typep $col 'num) (if (<= v $cut) $left $right))
                (t (if (equal v $cut) $left $right)))
          row))))

(defun tree-nodes (i fn &optional (lvl 0) (pre ""))
  (! fn i lvl pre)
  (when $col
    (let* ((nu (typep $col 'num))
           (sy (if nu "<=" "==")) (sn (if nu ">" "!=")))
      (loop for (kid op) in (sort (list (list $left  sy)
                                        (list $right sn))
                                  #'< :key (f+ (mid (? (car _) y))))
            do (tree-nodes kid fn (1+ lvl)
                 (format nil "~A ~A ~A" (? $col txt) op $cut))))))

(defun fmt-cell (x)
  (cond ((floatp x)   (format nil "~,vF" @decimals x))
        ((integerp x) (format nil "~D"   x))
        (t            (format nil "~A"   x))))

(defun tree-show (i)
  (tree-nodes i
    (lambda (i lvl pre)
      (let ((pad (if (plusp lvl)
                     (concatenate 'string
                       (apply #'concatenate 'string
                         (loop repeat (1- lvl) collect "|   "))
                       pre) "")))
        (format t "~vA, ~5,2F ,(~3D), {~{~A~^, ~}}~%"
                @show pad (mid $y) (? $y n)
                (mapcar #'fmt-cell $mids))))))

;;; ## Format helpers --------------------------------------

(defun o (x)
  "Recursive format. Floats use @decimals. Lists in {}."
  (cond ((floatp x) (format nil "~,vF" @decimals x))
        ((listp x)  (format nil "{~{~A~^, ~}}" (mapcar #'o x)))
        (t          (format nil "~A" x))))

(defun table (rows &optional (w 10))
  "Print list of alists as aligned table."
  (when rows
    (let ((ks (loop for (k . _) in (first rows) collect k)))
      (format t "~{~vA~}~%" (loop for k in ks append (list w k)))
      (format t "~A~%"
              (make-string (* w (length ks)) :initial-element #\-))
      (dolist (r rows)
        (format t "~{~vA~}~%"
                (loop for k in ks append
                      (list w (or (cdr (assoc k r :test #'equal))
                                  ""))))))))

;;; ## Stats helpers ---------------------------------------

(defun bisect (xs x)
  (let ((lo 0) (hi (length xs)))
    (loop while (< lo hi)
          for m = (floor (+ lo hi) 2)
          do (if (<= (elt xs m) x) (setf lo (1+ m))
                                   (setf hi m)))
    lo))

(defun weibull (k lam)
  (* lam (expt (- (log (- 1 (random 1.0)))) (/ 1 k))))

(defun wins (i)
  "Score row by closeness to d2h low; clamp via decile sd."
  (let* ((ys (sort (mapcar (f+ (disty i _)) $rows) #'<))
         (ten (max 1 (floor (length ys) 10)))
         (lo (first ys))
         (md (nth (* 5 ten) ys))
         (sd (/ (- (nth (min (1- (length ys)) (* 9 ten)) ys)
                   (nth ten ys)) 2.56)))
    (f+ (let ((x (disty i _)))
          (when (< x (+ lo (* 0.35 sd))) (setf x lo))
          (max -100 (floor (* 100 (- 1 (/ (- x lo)
                                          (+ (- md lo) 1e-32))))))))))

(defun ks (xs ys &aux (n (length xs)) (m (length ys)) (d 0))
  "Kolmogorov-Smirnov: max gap between empirical CDFs <= conf."
  (loop for v in (append xs ys) do
    (setf d (max d (abs (- (/ (bisect xs v) n)
                           (/ (bisect ys v) m))))))
  (<= d (* @ksconf (sqrt (/ (+ n m) (* n m))))))

(defun cliffs (xs ys &aux (n (length xs)) (m (length ys))
                          (gt 0) (lt 0))
  "Cliff's delta: |#gt - #lt| / nm <= threshold."
  (loop for v in xs do
    (incf gt (bisect ys v))
    (incf lt (- m (bisect ys (+ v 1e-32)))))
  (<= (/ (abs (- gt lt)) (* n m)) @cliffs))

(defun same (xs ys eps)
  "Two samples similar: medians close OR cliffs+KS pass."
  (let ((xs (sort (copy-list xs) #'<))
        (ys (sort (copy-list ys) #'<)))
    (or (<= (abs (- (nth (floor (length xs) 2) xs)
                    (nth (floor (length ys) 2) ys))) eps)
        (and (cliffs xs ys) (ks xs ys)))))

(defun best-ranks (dict)
  (let* ((names (sort (loop for k being the hash-keys of dict
                            collect k)
                      #'<
                      :key (f+ (mid (adds (gethash _ dict))))))
         (top (gethash (first names) dict))
         (eps (* (spread (adds top)) @eps))
         (out (make-hash-table :test 'equal)))
    (setf (gethash (first names) out)
          (adds top (make-num (first names) 0)))
    (loop for n in (rest names)
          while (same top (gethash n dict) eps)
          do (setf (gethash n out)
                   (adds top (make-num n 0))))
    out))

;;; ## Examples --------------------------------------------

(defun eg--the (&optional f) (declare (ignore f))
  (format t "~A~%" *the*))

(defun eg--csv (&optional (f @file))
  (loop for r in (read-csv f) for k from 0
        when (zerop (mod k 30)) do (format t "~A~%" r)))

(defeg eg--data "Show y col mids."
  (loop for c in (? i cols ys)
        do (format t "~A~T~A~%" (? c txt) (mid c))))

(defeg eg--tree "Build + show tree."
  (let* ((rs2 (subseq (shuffle rs) 0 (min @budget (length rs))))
         (d2  (clone i rs2))
         (sc  (f+ (disty d2 _))))
    (tree-show (tree-build sc d2 (? d2 rows)))))

(defeg eg--distx "First row vs nearest."
  (let* ((r0 (first (? i rows)))
         (rn (nearest i r0 (rest (? i rows)))))
    (format t "row0: ~A~%near: ~A~%dist: ~,3F~%" r0 rn (distx i r0 rn))))

(defeg eg--likes "Log-likelihood of first 5 rows."
  (loop for r in (subseq (? i rows) 0 (min 5 (length (? i rows))))
        do (format t "~,3F  ~A~%" (likes i r (length (? i rows)) 1) r)))

(defeg eg--pick "Pick a mutated copy of first row."
  (let ((r (first (? i rows))))
    (format t "orig: ~A~%pick: ~A~%" r (picks i r 2))))

(defeg eg--de "DE extrapolate over 3 random rows."
  (let* ((rs (subseq (shuffle (? i rows)) 0 3))
         (out (extrapolate (? i cols xs)
                           (first rs) (second rs) (third rs))))
    (format t "a:   ~A~%b:   ~A~%c:   ~A~%new: ~A~%"
            (first rs) (second rs) (third rs) out)))

(defun eg--ranks (&optional f) (declare (ignore f))
  (let ((dict (make-hash-table :test 'equal)))
    (loop for n from 1 to 20 do
      (let ((name (format nil "t~D" n))
            (k    (if (<= n 5) 2 1))
            (lam  (if (<= n 5) 10 20)))
        (setf (gethash name dict)
              (loop repeat 50 collect (weibull k lam)))))
    (format t "~%Top Tier Treatments:~%")
    (loop for nm in (sort (loop for v being the hash-values
                                of (best-ranks dict) collect v)
                          #'< :key (f+ (? _ mu)))
          do (format t "~5A median: ~5,2F~%"
                     (? nm txt) (mid nm)))))

(defeg eg--test "Tree-rank predict win."
  (when (? i cols)
    (let ((stats (make-num "win" 0)) (fn (wins i)))
      (loop repeat 20 do
        (let* ((rs2 (shuffle (? i rows)))
               (half (floor (length rs2) 2))
               (train (subseq rs2 0 (min half @budget)))
               (rest  (subseq rs2 half))
               (d2 (clone i train))
               (sc (f+ (disty d2 _)))
               (tr (tree-build sc d2 (? d2 rows)))
               (rk (sort rest #'<
                     :key (f+ (mid (? (tree-leaf tr _) y)))))
               (top (sort (subseq rk 0
                            (min @check (length rk)))
                          #'< :key (f+ (disty d2 _)))))
          (add stats (! fn (first top)))))
      (format t "~D~%" (floor (mid stats))))))

;;; ## Main ------------------------------------------------

(defun main () (cli *the*))
