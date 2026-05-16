#!/usr/bin/env -S sbcl --script
; vim: set ft=lisp ts=2 sw=2 et lw+=defmethods,defeg,let+ :

;; nb.lisp -- naive bayes classifier (online).
;; (c) 2026 Tim Menzies, timm@ieee.org, MIT license.

;; Usage:
;;   sbcl --script nb.lisp --cmd [arg] [-flag value]...

;; Commands:
;;   --the         print config *the*
;;   --testSym     Sym counts assertion
;;   --testNum     Num n+mu assertion
;;   --testLike    SYM_like in (0,1] assertion
;;   --sym         print example Sym
;;   --num         print example Num
;;   --col         print Num+Sym constructor examples
;;   --cols        print y-cols from a sample header
;;   --data        print first y column from CSV
;;   --like        Gaussian + smoothed-symbolic example
;;   --likes       likes(data, first row, n, 2)
;;   --nb          run NB on -f file
;;   --soybean     run NB on soybean.csv
;;   --diabetes    run NB on diabetes.csv
;;   --experiment  rank k/m variants by G-score on critical class
;;   --check       run all assertions; exit 1 on fail

;; Options:
;;   -f F  CSV file               (default auto93.csv)
;;   -k N  symbol smoothing       (default 1)
;;   -m N  prior smoothing        (default 2)
;;   -w N  burn-in rows           (default 5)
;;   -s N  random seed            (default 1)

;; CSV: header names cols; suffix sets role.
;;   [A-Z]* numeric   [a-z]* symbolic
;;   *!     class (y) *X     skip
;;   else   x         ?      missing

(load "lib")

;; =================================================================
;; Config
;; =================================================================

(defparameter *the*
  (list (list 'file "auto93.csv" "-f" "csv input")
        (list 'k    1            "-k" "symbol smoothing")
        (list 'm    2            "-m" "prior smoothing")
        (list 'wait 5            "-w" "burn-in rows")
        (list 'seed 1            "-s" "random seed")))

(defun the-of (k) (second (assoc k *the*)))

(setf *seed* @seed)

;; =================================================================
;; Sym -- symbolic column: count + frequency table.
;; =================================================================

(defstruct sym (at 0) (txt " ") (n 0) has)

(defmethod mid ((i sym))
  "Mode (most common value)."
  (car (reduce (ff+ (if (> (cdr _) (cdr __)) _ __))
               $has)))

(defmethod add ((i sym) v)
  "Increment count of V in $has."
  (unless (eq v '?)
    (incf $n)
    (let ((cell (assoc v $has :test #'equal)))
      (if cell (incf (cdr cell))
          (push (cons v 1) $has))))
  v)

(defmethod like ((i sym) v prior)
  "Smoothed P(v|class): (count + k*prior) / (n + k)."
  (max 1e-32
       (/ (+ (aget v $has) (* @k prior))
          (+ $n @k 1e-32))))

;; =================================================================
;; Num -- numeric column: Welford count, mu, m2, sd.
;; =================================================================

(defstruct (num (:constructor %make-num))
  (at 0) (txt " ") (n 0) (mu 0) (m2 0) (sd 0))

(defun make-num (&rest args) (apply #'%make-num args))

(defmethod mid    ((i num)) $mu)
(defmethod spread ((i num)) $sd)

(defmethod add ((i num) v)
  "Welford update of mu, m2, sd."
  (unless (eq v '?)
    (incf $n)
    (let ((d (- v $mu)))
      (incf $mu (/ d $n))
      (incf $m2 (* d (- v $mu)))
      (setf $sd (if (< $n 2) 0
                    (sqrt (/ $m2 (1- $n)))))))
  v)

(defmethod like ((i num) v prior)
  "Gaussian P(v|class)."
  (declare (ignore prior))
  (let* ((var (+ (* $sd $sd) 1e-32)))
    (* (/ 1 (sqrt (* 2 pi var)))
       (exp (- (/ (expt (- v $mu) 2)
                  (* 2 var)))))))

;; =================================================================
;; Cols -- header-derived collection.
;;   y: name ends in !
;;   x: name does NOT end in ! or X
;; =================================================================

(defstruct (cols (:constructor %make-cols))
  names x y all)

(defun make-cols (txts &aux (i (%make-cols :names txts))
                            (n -1))
  "Parse header TXTS; partition into x/y."
  (let+ ((one (txt) (! (if (upper-case-p (ch txt 0))
                           #'make-num #'make-sym)
                        :txt txt :at (incf n))))
    (setf $all (mapcar #'one txts))
    (dolist (c $all)
      (let ((last (ch (? c txt) -1)))
        (unless (find last "!X")
          (push c $x))
        (when (eq last #\!)
          (push c $y)))))
  i)

(defmethod add ((i cols) row)
  (mapcar (ff+ (add _ __)) $all row)
  row)

;; =================================================================
;; Data -- table: rows + parsed columns + name.
;;   First add sets cols (header). Subsequent adds push rows.
;; =================================================================

(defstruct (data (:constructor %make-data))
  rows cols (txt ""))

(defun make-data (&optional rows
                  &aux (i (%make-data)))
  "Build empty data; if ROWS, first is header, rest data."
  (when rows
    (dolist (row rows) (add i row)))
  i)

(defmethod add ((i data) row)
  "If no cols yet, ROW is header; else push to rows."
  (if (null $cols)
      (setf $cols (make-cols row))
      (progn (push row $rows)
             (add $cols row)))
  row)

(defun clone-data (data &optional rows (txt ""))
  "Empty Data with same cols. ROWS optional seed."
  (let ((d (make-data)))
    (setf (data-txt d) txt)
    (add d (? data cols names))
    (when rows (dolist (r rows) (add d r)))
    d))

;; =================================================================
;; Bayes
;; =================================================================

(defun likes (data row n-all n-h)
  "Log-likelihood of ROW under DATA's class model."
  (let* ((b (/ (+ (length (? data rows)) @m)
               (+ n-all (* @m n-h))))
         (s (loop for c in (? data cols x)
                  for v = (elt row (? c at))
                  unless (eq v '?)
                  sum (log (max 1e-32 (like c v b))))))
    (+ (log b) s)))

(defun cm-incf (cm want got)
  "Increment confusion-matrix cell cm[want][got]."
  (let* ((row (assoc want cm :test #'equal))
         (cell (assoc got (cdr row) :test #'equal)))
    (if cell (incf (cdr cell))
        (rplacd row (acons got 1 (cdr row))))))

(defun classify (klasses row n nk)
  "Return class name with max log-likelihood, or nil."
  (let ((best (argm klasses
                    (f+ (likes (cdr _) row n nk))
                    #'>)))
    (when best (car best))))

(defun nb (data &aux (klasses nil) (cm nil)
                     (n 0) (nk 0)
                     (kat (? (car (? data cols y)) at)))
  "Online NB: classify-then-train. Returns alist cm."
  (dolist (row (reverse (? data rows)) cm)
    (let ((want (elt row kat)))
      (unless (assoc want klasses :test #'equal)
        (incf nk)
        (push (cons want (clone-data data nil
                                     (format nil "~a" want)))
              klasses)
        (push (cons want nil) cm))
      (when (> n @wait)
        (let ((got (classify klasses row n nk)))
          (when got (cm-incf cm want got))))
      (incf n)
      (add (aget want klasses nil) row))))

;; =================================================================
;; Stats
;; =================================================================

(defun pct (x y)
  (floor (+ (/ (* 100 x) (+ y 1e-32)) 0.5)))

(defun cm-total (cm)
  "Total cell count across confusion matrix."
  (loop for (_ . gots) in cm
        sum (loop for (_ . c) in gots sum c)))

(defun cm-fnfp (cm pos)
  "For class POS, return (values fn fp) over CM."
  (let ((fn 0) (fp 0))
    (loop for (w . wgots) in cm do
      (loop for (g . c) in wgots do
        (cond ((and (equal w pos) (not (equal g pos)))
               (incf fn c))
              ((and (not (equal w pos)) (equal g pos))
               (incf fp c)))))
    (values fn fp)))

(defun metrics (pos n tp fn fp)
  "Per-class stats plist from raw counts."
  (let* ((tn  (- n tp fp fn))
         (rec (/ tp (+ tp fn 1e-32)))
         (spc (/ tn (+ tn fp 1e-32)))
         (g   (/ (* 2 rec spc) (+ rec spc 1e-32))))
    (list :class (format nil "~a" pos)
          :n n :tn tn :fn fn :fp fp :tp tp
          :pd   (pct tp (+ tp fn))
          :pf   (pct fp (+ fp tn))
          :prec (pct tp (+ tp fp))
          :acc  (pct (+ tp tn) n)
          :g    (floor (+ (* 100 g) 0.5)))))

(defun stats (cm)
  "Per-class stats rows from confusion matrix."
  (let ((n (cm-total cm)))
    (loop for (pos . gots) in cm
          collect (multiple-value-bind (fn fp)
                      (cm-fnfp cm pos)
                    (metrics pos n (aget pos gots)
                             fn fp)))))

(defparameter *hdr-fmt*
  "~&~5a ~5a ~5a ~5a ~5a   ~4a ~4a ~4a ~4a ~4a   ~a~%")
(defparameter *row-fmt*
  "~&~5d ~5d ~5d ~5d ~5d   ~4d ~4d ~4d ~4d ~4d   ~a~%")

(defun print-stats (rows)
  (format t *hdr-fmt*
          "n" "tn" "fn" "fp" "tp"
          "pd" "pf" "prec" "acc" "g" "class")
  (dolist (r rows)
    (format t *row-fmt*
            (getf r :n) (getf r :tn) (getf r :fn)
            (getf r :fp) (getf r :tp)
            (getf r :pd) (getf r :pf) (getf r :prec)
            (getf r :acc) (getf r :g)
            (getf r :class))))

(defun bisect (xs x)
  "Rightmost i where (elt xs i) <= x. 0..length."
  (let ((lo 0) (hi (1- (length xs))))
    (loop while (<= lo hi)
          do (let ((m (floor (+ lo hi) 2)))
               (if (<= (elt xs m) x)
                   (setf lo (1+ m))
                   (setf hi (1- m)))))
    lo))

(defun cliffs-delta (xs ys)
  (let ((n (length xs)) (m (length ys))
        (ngt 0) (nlt 0))
    (dolist (v xs)
      (incf ngt (bisect ys v))
      (incf nlt (- m (bisect ys (+ v 1e-32)))))
    (/ (abs (- ngt nlt)) (* n m))))

(defun ks (xs ys)
  (let ((n (length xs)) (m (length ys)) (d 0))
    (flet ((gap (v)
             (abs (- (/ (bisect xs v) n)
                     (/ (bisect ys v) m)))))
      (dolist (v xs) (setf d (max d (gap v))))
      (dolist (v ys) (setf d (max d (gap v))))
      d)))

(defun same (xs ys eps)
  (let* ((xs (sort (copy-list xs) #'<))
         (ys (sort (copy-list ys) #'<))
         (n (length xs)) (m (length ys))
         (mx (elt xs (floor n 2)))
         (my (elt ys (floor m 2))))
    (cond ((<= (abs (- mx my)) eps) t)
          ((> (cliffs-delta xs ys) 0.195) nil)
          (t (<= (ks xs ys)
                 (* 1.36
                    (sqrt (/ (+ n m) (* n m)))))))))

(defun eps-pool (a b)
  "Pooled-sd epsilon: 0.35*sqrt((sd_a²+sd_b²)/2), min 1."
  (max 1 (* 0.35 (sqrt (/ (+ (expt (spread a) 2)
                             (expt (spread b) 2))
                          2)))))

(defun lead-same? (lead entry dict)
  "True if ENTRY's values are stat-equiv to LEAD's."
  (same (aget (car lead)  dict nil)
        (aget (car entry) dict nil)
        (eps-pool (cdr lead) (cdr entry))))

(defun sames (dict)
  "Rank treatments. Returns list of (:name :num :rank)."
  (let* ((nums   (loop for (nm . vs) in dict
                       collect (cons nm (adds vs))))
         (sorted (gt (f+ (mid (cdr _))) nums))
         (lead   (car sorted))
         (rank   1)
         (out    nil))
    (dolist (entry sorted (nreverse out))
      (when (and (not (eq entry lead))
                 (not (lead-same? lead entry dict)))
        (incf rank)
        (setf lead entry))
      (push (list :name (car entry)
                  :num  (cdr entry)
                  :rank rank)
            out))))

(defun rank-print (dict)
  (format t "~&  ~4a  ~12a  ~6a  ~6a~%"
          "rank" "treatment" "mu" "sd")
  (let ((prev 0))
    (dolist (r (sames dict))
      (when (and (/= (getf r :rank) prev) (> prev 0))
        (format t "~%"))
      (setf prev (getf r :rank))
      (format t "~&  ~4d  ~12a  ~6,2f  ~6,2f~%"
              (getf r :rank) (getf r :name)
              (mid (getf r :num)) (spread (getf r :num))))))

;; =================================================================
;; Assertion helpers
;; =================================================================

(defvar *fails* 0)
(defvar *asserts* 0)

(defun ok (cond label)
  (format t "~&~a ~a~%" (if cond "PASS" "FAIL") label)
  (if cond (incf *asserts*) (incf *fails*)))

(defun eq= (a b msg)
  (ok (equalp a b)
      (format nil "~a (want ~a got ~a)" msg b a)))

;; =================================================================
;; Examples
;; =================================================================

(defun eg--the (&optional _)
  (declare (ignore _)) (print *the*))

(defun eg--testSym (&optional _)
  "Sym n + count."
  (declare (ignore _))
  (let ((s (make-sym)))
    (dolist (v '(a a a b b c)) (add s v))
    (eq= (? s n) 6 "Sym n")
    (eq= (cdr (assoc 'a (? s has))) 3 "Sym a count")))

(defun eg--testNum (&optional _)
  "Num n + mu."
  (declare (ignore _))
  (let ((nm (adds '(1 2 3 4 5))))
    (eq= (? nm n) 5 "Num n")
    (eq= (mid nm) 3 "Num mu")))

(defun eg--testLike (&optional _)
  "SYM_like in (0,1]."
  (declare (ignore _))
  (let ((s (make-sym)))
    (dolist (v '(a a a b c)) (add s v))
    (let ((p (like s 'a 0.5)))
      (ok (and (> p 0) (<= p 1))
          "SYM_like in (0,1]"))))

(defun nb-min-g (file)
  "Run NB on FILE; return min G across classes."
  (loop for r in (stats (nb (make-data (read-csv file))))
        minimize (getf r :g)))

(defun eg--testDiabetes (&optional _)
  "NB on diabetes: min G >= 50."
  (declare (ignore _))
  (let ((g (nb-min-g
             "~/gits/timm/moot/classify/diabetes.csv")))
    (ok (>= g 50)
        (format nil "diabetes min G=~d >= 50" g))))

(defun eg--testSoybean (&optional _)
  "NB on soybean: min G >= 30."
  (declare (ignore _))
  (let ((g (nb-min-g
             "~/gits/timm/moot/classify/soybean.csv")))
    (ok (>= g 30)
        (format nil "soybean min G=~d >= 30" g))))

(defun eg--sym (&optional _)
  "Print example Sym."
  (declare (ignore _))
  (let ((s (make-sym)))
    (dolist (v '(a a a b c)) (add s v))
    (print s)))

(defun eg--num (&optional _)
  "Print example Num."
  (declare (ignore _))
  (print (adds '(10 20 30 40))))

(defun eg--col (&optional _)
  "Print Num+Sym examples."
  (declare (ignore _))
  (print (make-num :at 1 :txt "Age"))
  (print (make-sym :at 2 :txt "name")))

(defun eg--cols (&optional _)
  "y-cols from sample header."
  (declare (ignore _))
  (let ((c (make-cols
             '("Name" "Age" "Weight-" "Class!"))))
    (print (? c y))))

(defeg eg--data "Show first y column."
  (print (car (? i cols y))))

(defun eg--like (&optional _)
  "NUM + SYM likelihood example."
  (declare (ignore _))
  (let ((nm (adds '(10 20 30 40 50)))
        (s  (make-sym)))
    (dolist (v '(a a a b c)) (add s v))
    (format t "~&NUM_like(30)=~a  SYM_like(a)=~a~%"
            (like nm 30 nil) (like s 'a 0.5))))

(defeg eg--likes "likes(data, row0, n, 2)."
  (print (likes i (car (? i rows))
                (length (? i rows)) 2)))

(defeg eg--nb "Run NB on -f file."
  (print-stats (stats (nb i))))

(defun eg--soybean (&optional _)
  "NB on soybean.csv."
  (declare (ignore _))
  (print-stats
    (stats (nb (make-data (read-csv "soybean.csv"))))))

(defun eg--diabetes (&optional _)
  "NB on diabetes.csv."
  (declare (ignore _))
  (print-stats
    (stats (nb (make-data (read-csv "diabetes.csv"))))))

(defeg eg--experiment "Rank k/m variants by G-score."
  (let* ((all-rows (reverse (? i rows)))
         (n-all (length all-rows))
         (target (cond ((search "soybean" @file)
                        "phytophthora-rot")
                       ((search "diabetes" @file)
                        "tested_positive")
                       (t nil)))
         (dict nil))
    (dolist (n '(50 100 200))
      (let ((n (min n n-all)))
        (loop for k from 0 to 2 do
          (loop for m from 0 to 2 do
            (let ((nm (format nil "n=~3d k=~d m=~d"
                              n k m))
                  (vals nil))
              (dotimes (_ 20)
                (setf (second (assoc 'k *the*)) k
                      (second (assoc 'm *the*)) m)
                (let* ((rs2 (subseq
                              (shuffle
                                (copy-list all-rows))
                              0 n))
                       (d (clone-data i rs2))
                       (srows (stats (nb d)))
                       (row (find target srows
                                  :key (f+ (getf _ :class))
                                  :test #'equal)))
                  (push (if row (getf row :g) 0) vals)))
              (push (cons nm vals) dict))))))
    (format t "~&~%G-score on '~a' (rank 1=top):~%"
            target)
    (rank-print dict)))

(defun eg--check (&optional _)
  "Run all assertions; exit 1 on fail."
  (declare (ignore _))
  (setf *fails* 0 *asserts* 0)
  (eg--testSym) (eg--testNum) (eg--testLike)
  (eg--testDiabetes) (eg--testSoybean)
  (format t "~&;; asserts passed: ~d  failed: ~d~%"
          *asserts* *fails*)
  (when (plusp *fails*) (sb-ext:exit :code 1)))

(cli *the*)
