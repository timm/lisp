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
  "My settings, as (name flag doc value)."
  (list (list 'seed "-s" "random seed" 1234567891)
        (list 'p    "-p" "distance exponent" 2)
        (list 'file "-f" "data set file"
              "~/gits/moot/optimize/misc/auto93.csv")))

;;;; structs
(defstruct num (at 0) (txt "") (n 0) (mu 0) (m2 0) (goal 1))
(defstruct sym (at 0) (txt "") (n 0) seen)
(defstruct tbl rows cols mids)
(defstruct cols names x y klass all)

(defun num (&optional txt at)
  (make-num :txt txt :at at 
            :goal (if (eql #\- (chars txt -1)) 0 1)))

(defun sym (&optional txt at) (make-sym :txt txt :at at))

(defun col (txt at)
  (if (upper-case-p (chars txt 0)) (num txt at) (sym txt at)))

(defun cols (names &aux (i (make-cols :names names)) (at -1))
  (dolist (col (setf $all (->> (col %1 (incf at)) names)) i)
    (if (member (chars (? col txt) -1) '(#\+ #\-))
      (push col $y)
      (push col $x))
    (if (eql #\! (chars (? col txt) -1)) (setf $klass col))))

(defmethod add ((i sym) x)
  "Count X."
  (incf $n)
  (incf (has x $seen))
  x)

(defmethod add ((i num) x)
  "Welford: one pass, updating N, MU and M2 together, so
   `sd` needs no second visit to the data."
  (incf $n)
  (let ((d (- x $mu)))
    (incf $mu (/ d $n))
    (incf $m2 (* d (- x $mu))))
  x)

(defmethod add ((i cols) (row cons))
  (add i (coerce row 'vector)))

(defmethod add ((i cols) row)
  "Show each cell of ROW to the column that owns it.
   `?` means unknown, so it is shown to no one."
  (dolist (col $all row)
    (let ((x (elt row (? col at))))
      (unless (eql x '?) (add col x)))))

(defmethod add ((i tbl) (row cons))
  "Coerce here too, so what I store is a vector."
  (add i (coerce row 'vector)))

(defmethod add ((i tbl) row)
  "Keep ROW, and summarize it in my cols."
  (push row $rows)
  (add $cols row)
  row)

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

(defun near (x y &optional (eps 0.01))
  "True if X and Y agree to within EPS."
  (< (abs (- x y)) eps))

(defun gauss (m sd)
  "One sample from a normal curve, via Box-Muller."
  (+ m (* sd (sqrt (* -2 (log (rand 1.0))))
            (cos (* 2 pi (rand 1.0))))))

(defun tbl (rows)
  (adds (cdr rows) (make-tbl :cols (cols (car rows)))))

(defun adds (lst &optional (i (num)))
  (dolist (x lst i) (add i x)))

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
  (dolist (c (reverse (? (? (tbl (csv (my file))) cols) all)))
    (format t "~&  ~10a mid=~9,2f div=~8,3f~%"
            (? c txt) (float (mid c)) (float (div c)))))

(defun eg--gauss ()
  "Sampling a normal curve reproduces its mid and div."
  (let ((n (num)))
    (dotimes (i 1000) (add n (gauss 10 2)))
    (kv :mid (mid n) :div (div n))
    (assert (near (mid n) 10 0.5))
    (assert (near (div n) 2  0.5))))

(defun eg--all (&aux (fails 0) egs)
  (do-symbols (s *package*)
    (if (and (fboundp s) (eql 0 (search "EG--" (symbol-name s)))
             (not (eql s 'eg--all)))
      (pushnew s egs)))
  (dolist (f (sort egs #'string< :key #'symbol-name) fails)
    (setf fails (cli-run f fails))))

;;;; main file

(cli (defaults))
