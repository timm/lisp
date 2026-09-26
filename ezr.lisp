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
  "My settings, as (name flag doc value).  Built with
   `list`: `cli` mutates this, and a quoted literal is
   the same object on every call (and read-only at that)."
  (list (list 'seed "-s" "random seed" 1234567891)
        (list 'p    "-p" "distance exponent" 2)
        (list 'file "-f" "data set file"
              "~/gits/moot/optimize/misc/auto93.csv")))

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
  "Header NAMES, a vector.  A name's last letter is magic:
   + or - is a goal, ! is the klass (a goal too), and X is
   summarized like the rest but kept out of x and y."
  (setf $all (loop for txt across names for at from 0
                   collect (col txt at)))
  (dolist (col $all i)
    (case (chars (? col txt) -1)
      (#\X)                         ; summarized, not modelled
      ((#\+ #\-) (push col $y))
      (#\!       (setf $klass col) (push col $y))
      (t         (push col $x)))))

(defmethod clone ((i tbl)) (tbl (list (? i cols names))))
(defmethod clone ((i num)) (num $txt $at))
(defmethod clone ((i sym)) (sym $txt $at))

;;;; -----------------------------------------------------------
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
  "Every column's mid, cached until the next `add`."
  (or $mids (setf $mids (->> (mid %1) (cols-all $cols)))))

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
      (setf i (add i r))
      (if (= k half) (print (setf was (mids i)))))
    (dolist (r (reverse (nthcdr half rows)))
      (setf i (sub i r)))          ; newest first: cheap
    (->> (assert (same %1 %2)) (print (mids i)) was)))

(defun eg--all (&aux (fails 0) egs)
  (do-symbols (s *package*)
    (if (and (fboundp s) (eql 0 (search "EG--" (symbol-name s)))
             (not (eql s 'eg--all)))
      (pushnew s egs)))
  (dolist (f (sort egs #'string< :key #'symbol-name) fails)
    (setf fails (cli-run f fails))))

;;;; main file

(cli #'defaults)
