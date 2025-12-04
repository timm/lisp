#!/usr/local/bin/sbcl --script
;;; binr.lisp : build rules via stochastic incremental XAI
;;; (c) 2025, mit-license.org
(defun eg--help ()
  (format t "
Options:
  -e  era=10     Number of rows in an era
  -b  bins=7     Number of bins for discretization
  -B  Budget=30  Max rows to eval
  -r  repeats=20 Number of experimental repeats
  -s  seed=42    Random number seed
  -f  file=../data/auto93.csv~%"))

(defvar *the* (list :era 10 :bins 7 :budget 30 :repeats 20 :seed 42
                    :file "../data/auto93.csv"))

(defun opt (k) (getf *the* k))

(defun o (x) (format t "~a" x)) 

;;; Lib ----------------------------------------------------------------------
(defun s2n (s) 
  (let ((n (read-from-string s nil))) (if (numberp n) n s)))

(defun split (s) 
  (loop for i = 0 then (1+ j) 
        as j = (position #\, s :start i)
        collect (s2n (subseq s i j)) while j))

(defun randf () (random 1.0))

(defun box-muller (mu sd)
  (+ mu (* sd (sqrt (* -2 (log (randf)))) (cos (* 2 pi (randf))))))

(defun shuffle (s)
  (loop for i from (length s) downto 2
        do (rotatef (elt s (1- i)) (elt s (random i)))) s)

;;; Structs ------------------------------------------------------------------
(defstruct (col) at txt (n 0) (bins (make-hash-table)))
(defstruct (num (:include col)) (mu 0d0) (m2 0d0) (sd 0d0) (goal 1))
(defstruct (sym (:include col)) (has (make-hash-table :test 'equal)))
(defstruct (cols) names all x y)
(defstruct (data) rows cols)

;;; Methods ------------------------------------------------------------------
(defmethod add ((i sym) x)
  (unless (eq x '?) 
    (with-slots (n has) i 
      (incf n) 
      (incf (gethash x has 0)))))

(defmethod add ((i num) x)
  (unless (eq x '?) (with-slots (n mu m2 sd) i
    (let ((d (- x mu))) 
      (incf n) 
      (incf mu (/ d n)) 
      (incf m2 (* d (- x mu)))
      (setf sd (if (< n 2) 0d0 (sqrt (/ (max 0 m2) (1- n)))))))))

(defun make-col (n s)
  (if (upper-case-p (char s 0))
      (make-num :at n :txt s :goal (if (find #\- s) 0 1))
      (make-sym :at n :txt s)))

(defun new-cols (row &aux (all (loop for s in row for n from 0
                                     collect (make-col n s))))
  (make-cols :names row :all all
             :x (remove-if (lambda (c) (find #\X (col-txt c))) all)
             :y (remove-if-not (lambda (c) (some (lambda (x) (find x "+-"))
                                                 (col-txt c))) all)))

(defmethod add ((d data) row)
  (with-slots (cols rows) d
    (if cols (progn (loop for c in (cols-all cols)
                          do (add c (elt row (col-at c))))
                    (push row rows))
        (setf cols (new-cols row)))))

(defun load-d (src &optional (d (make-data)))
  (with-open-file (s src)
    (loop for l = (read-line s nil) 
          while l do (add d (split l)))) 
  d)

(defun clone (d)
  (let ((d2 (make-data))) 
    (add d2 (cols-names (data-cols d))) 
    d2))

;;; Logic --------------------------------------------------------------------
(defmethod norm ((n num) x)
  (if (eq x '?) x
      (/ 1 (1+ (exp (* -1.702 (/ (- x (num-mu n)) (+ (num-sd n) 1e-32))))))))

(defmethod bin ((n num) x)
  (if (or (eq x '?) (zerop (num-mu n))) 
    x
    (floor (* (opt :bins) (norm n x)))))

(defun disty (d row)
  (sqrt (/ (loop for c in (cols-y (data-cols d))
                 sum (expt (- (norm c (elt row (col-at c))) (num-goal c)) 2))
           (length (cols-y (data-cols d))))))

(defun score-get (d row)
  (loop for c in (cols-x (data-cols d)) for b = (bin c (elt row (col-at c)))
        sum (if (and (not (eq b '?)) (gethash b (col-bins c)))
                (num-mu (gethash b (col-bins c))) 0)))

(defun score-put (d row score)
  (loop for c in (cols-x (data-cols d)) for b = (bin c (elt row (col-at c)))
        unless (eq b '?) do
        (add (or (gethash b (col-bins c))
                 (setf (gethash b (col-bins c)) (make-num))) score)))

(defun scores-seen (d)
  (let* ((lst (sort (mapcar (lambda (r) (disty d r)) (data-rows d)) #'<))
         (m (floor (length lst) 10)))
    (format t "~&~{~5,2f~^, ~}; eps= ~5,2f"
            (mapcar (lambda (p) (nth (* p m) lst)) '(1 3 5 7 9))
            (values (* 0.35 (/ (- (nth (* 9 m) lst) (nth m lst)) 2.56))))))

(defun best-guess (d rows)
  (cdr (first (sort (loop for r in rows collect (cons (score-get d r) r))
                    #'> :key #'car))))

(defun score (d eps &aux (besty 1d32) best (labeled (clone d)))
  (loop for i from 0 below (min (length (data-rows d)) (opt :budget))
        for row = (elt (data-rows d) i) do
        (add labeled row) (score-put labeled row (disty labeled row))
        (when (and (> i 0) (zerop (mod i (opt :era))))
          (let* ((g (best-guess labeled (data-rows labeled)))
                 (y (disty labeled g)))
            (when (< y (- besty eps)) (setf besty y best g)))))
  (values best (disty d best)))

;;; Examples -----------------------------------------------------------------
(defun eg--csv ()
  (with-open-file (s (opt :file))
    (loop for l = (read-line s nil) while l do (o (split l)))))

(defun eg--num (&aux (n (make-num)))
  (loop repeat 1000 do (add n (box-muller 10 5)))
  (format t "~&mu=~5,3f sd=~5,3f" (num-mu n) (num-sd n)))

(defun eg--data ()
  (loop for c in (cols-x (data-cols (load-d (opt :file)))) do (print c)))

(defun eg--disty ()
  (let ((d (load-d (opt :file))))
    (o (sort (loop for r in (data-rows d) collect (disty d r)) #'<))))

(defun eg--score ()
  (multiple-value-bind (eps) (scores-seen (load-d (opt :file)))
    (o (sort (loop repeat (opt :repeats) collect
                   (floor (* 100 (nth-value 1 (score (load-d (opt :file))
                                                     eps))))) 
             #'<))))

;;; Main ---------------------------------------------------------------------
(setf *random-state* (make-random-state t))
(loop for arg in (cdr sb-ext:*posix-argv*) do
  (let ((fun (find-symbol (string-upcase (format nil "eg~a" arg)))))
    (if (fboundp fun) (funcall fun) (eg--help))))
