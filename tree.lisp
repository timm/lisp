;; tree.lisp -- regression trees, in as little code as I can manage.
;; (c) 2026 Tim Menzies, timm@ieee.org, MIT license.
;;
;; Stands alone: no lib.lisp, no structs, no CLOS, no macros.
;;
;; A missing value is nil, not `?`, so every test for one is
;; just a test for truth.
;;
;; In: a csv whose header names the columns.  An upper case first
;; letter means numeric.  A last letter of `+` or `-` marks a goal
;; to maximize or minimize, `X` marks a column to ignore.
;;
;; Out: the tree, as nested lists.  Every node carries the test
;; that reached it, so printing is a four line walk and `write`
;; is all the model persistence anyone needs.
;;
;;   node = (test n d2h mids . kids)
;;   test = (at name op v)        ; nil at the root
;;
;; Whoever calls `grow` decides which rows were worth labelling.
;; Growing on a random sample needs no code here beyond a shuffle.

(defparameter *leaf* 4 "Stop splitting at this many rows.")
(defparameter *p*    2 "Distance exponent.")
(defparameter *cuts* 8 "Splits to try, per numeric column.")

;;;; -----------------------------------------------------------
;;;; csv

(defun thing (s)
  "Coerce S to a number, or `?`, or leave it a string."
  (let* ((*read-eval* nil)
         (v (handler-case (read-from-string s nil "") (error () ""))))
    (if (numberp v) v (if (string= s "?") nil s))))

(defun cells (s &optional (lo 0) (hi (position #\, s :start lo)))
  "Split S on commas, coercing each cell."
  (cons (thing (subseq s lo hi)) (if hi (cells s (1+ hi)))))

(defun csv (file)
  "FILE as a list of lists: the header, then the rows."
  (with-open-file (s (truename file))
    (loop for line = (read-line s nil) while line
          collect (cells (string-right-trim '(#\Return) line)))))

;;;; -----------------------------------------------------------
;;;; columns

(defun lastc (name) "NAME's last character." (char name (1- (length name))))

(defun x-cols (names)
  "The (at name numeric?) of every column we may split on."
  (loop for name in names for at from 0
        unless (find (lastc name) "+-!X")
        collect (list at name (upper-case-p (char name 0)))))

(defun y-cols (names)
  "The (at name heaven) of every goal column."
  (loop for name in names for at from 0
        when (find (lastc name) "+-")
        collect (list at name (if (eql #\+ (lastc name)) 1 0))))

(defun cells-at (at rows)
  "Column AT of ROWS, with the unknowns dropped."
  (loop for r in rows for x = (nth at r) when x collect x))

;;;; -----------------------------------------------------------
;;;; one number per row

(defun mu-sd (xs)
  "Mean and sample sd of XS, in one pass.  No Welford, no take-backs."
  (let ((n 0) (sum 0d0) (sum2 0d0))
    (dolist (x xs) (incf n) (incf sum x) (incf sum2 (* x x)))
    (values (/ sum (max 1 n))
            (if (< n 2) 0d0
              (sqrt (max 0d0 (/ (- sum2 (/ (* sum sum) n)) (1- n))))))))

(defun norm (x mu sd)
  "X to 0..1, via 1/(1+exp(-1.7 z)); z clamped to +-3."
  (let ((z (max -3 (min 3 (/ (- x mu) (+ 1d-32 sd))))))
    (/ 1 (+ 1 (exp (* -1.7 z))))))

(defun d2h (cols stats row)
  "Root mean square gap from ROW's goals to their heaven."
  (let ((d 0d0) (n 1d-32))
    (loop for (at nil heaven) in cols for (mu sd) in stats
          for x = (nth at row) when x do
      (incf n)
      (incf d (expt (abs (- (norm x mu sd) heaven)) *p*)))
    (expt (/ d n) (/ 1d0 *p*))))

(defun d2hs (cols rows)
  "Every row's distance to heaven, in row order.  Zero is best."
  (let ((stats (loop for (at) in cols
                     collect (multiple-value-list
                               (mu-sd (cells-at at rows))))))
    (loop for r in rows collect (d2h cols stats r))))

(defun mean (xs)
  "Mean of XS, as a float, so printed trees stay readable."
  (float (/ (reduce #'+ xs) (max 1 (length xs))) 1d0))

(defun mids (cols rows)
  "The mean of each goal column, over ROWS."
  (loop for (at) in cols collect (mean (cells-at at rows))))

;;;; -----------------------------------------------------------
;;;; cuts


(defun score (a b)
  "Spread either side of a cut, weighted by how many fell there."
  (/ (+ (* (length a) (nth-value 1 (mu-sd a)))
        (* (length b) (nth-value 1 (mu-sd b))))
     (+ (length a) (length b) 1d-32)))

(defun candidates (at numeric? rows)
  "What to try: one cut per symbol, or *cuts* numbers by rank."
  (if (not numeric?)
    (loop for x in (remove-duplicates (cells-at at rows)
                                 :test #'equal :from-end t)
          collect (list x '=))
    (let* ((xs (coerce (sort (cells-at at rows) #'<) 'vector))
           (n  (length xs))
           (k  (max 1 (floor n *cuts*))))   ; by rank: these are dense
      (loop for i from (1- k) below (1- n) by k
            collect (list (aref xs i) '<=)))))

(defun sharpest (cols rows ys)
  "The (at name op v) whose two sides have the tightest ys."
  (let (out (least 1d30))
    (dolist (c cols out)
      (destructuring-bind (at name numeric?) c
        (loop for (v op) in (candidates at numeric? rows) do
          (let ((test (list at name op v)) a b)
            (loop for r in rows for y in ys    ; one pass per cut,
              do (if (selects test r)          ; so no take-backs
                   (push y a)
                   (push y b)))
            (when (and (>= (length a) *leaf*) (>= (length b) *leaf*))
              (let ((s (score a b)))
                (when (< s least) (setf out test least s))))))))))
;;;; -----------------------------------------------------------
;;;; tree

(defun selects (test row &aux (x (nth (first test) row))
                             (v (fourth test)))
  "Does ROW pass TEST?  A missing value passes anything."
  (or (null x)
      (case (third test)
        (<= (<= x v)) (> (> x v)) (= (equal x v))
        (t (not (equal x v))))))

(defun flip (test)
  "TEST's complement, for the other branch."
  (list (first test) (second test)
        (if (eq (third test) '<=) '> '/=) (fourth test)))

(defun grow (cols ys-cols rows ys &optional test)
  "A node for ROWS, plus two below it if a cut is worth making."
  (let ((z (and (> (length rows) *leaf*) (sharpest cols rows ys)))
        kids)
    (when z
      (let ((yes z) rs1 ys1 rs2 ys2)
        (loop for r in rows for y in ys do
          (cond ((selects yes r) (push r rs1) (push y ys1))
                (t               (push r rs2) (push y ys2))))
        (when (and rs1 rs2)
          (setf kids (list (grow cols ys-cols rs1 ys1 yes)
                           (grow cols ys-cols rs2 ys2 (flip yes)))))))
    (list* test (length rows) (mean ys) (mids ys-cols rows) kids)))

(defun kids   (z) "Z's children, if any."    (cddddr z))
(defun leaves (z) "Every childless node under Z."
  (if (kids z) (mapcan #'leaves (kids z)) (list z)))

(defun leaf (z row)
  "Walk ROW down to its leaf.  Each kid knows its own test."
  (if (kids z)
    (leaf (if (selects (first (first (kids z))) row)
            (first (kids z))
            (second (kids z)))
          row)
    z))

;;;; -----------------------------------------------------------
;;;; show.  Or skip it: `(write z)` is the tree, verbatim.

(defun show (names z &optional (pre ""))
  "Tree on the left; goal mids, d2h and n on the right."
  (when (string= pre "")
    (format t "~&~34a~{~9@a~}~9@a~7@a~%" ""
      (loop for (nil name) in (y-cols names) collect name) "d2h" "n"))
  (destructuring-bind (test n mu mids . kids) z
    (format t "~&~34a~{~9,2f~}~9,2f~7d~%"
      (concatenate 'string pre
        (if test
          (format nil "~a ~a ~a" (second test) (third test)
            (if (floatp (fourth test))
              (format nil "~,2f" (fourth test))
              (fourth test)))
          ""))
      mids mu n)
    (dolist (k kids)
      (show names k (concatenate 'string pre "|  ")))))

;;;; -----------------------------------------------------------
;;;; main

(defun tree (file &aux (data (csv file)) (names (car data))
                       (rows (cdr data)) (cols (y-cols names)))
  "Read FILE, score its rows, grow a tree over all of them."
  (let ((z (grow (x-cols names) cols rows (d2hs cols rows))))
    (show names z)
    z))
