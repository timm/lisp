#!/usr/bin/env sbcl --script
#+sbcl (declaim (sb-ext:muffle-conditions cl:style-warning))

(defvar *the* '(seed 42 
                file "auto93.csv" 
                bins 7 
                Budget 50
                leaf 4 
                p 2 
                Show 30 
                Check 5))

;;;; macros ----------------------------------------------------------
(defmacro ? (x) `(getf *the* ',x))

;;;; structs ---------------------------------------------------------
(defstruct sym (n 0) (at 0) (txt " ") (has (make-hash-table :test #'equal)))
(defstruct num (n 0) (at 0) (txt " ") (mu 0) (m2 0) (goal 1))
(defstruct cols x y all names)
(defstruct data rows cols)
(defstruct node col cut lo hi y)

;;;; lib -------------------------------------------------------------
(defun chr (s i &aux (s1 (string s)))
  (char s1 (if (minusp i) (+ (length s1) i) i)))

(defun chrs (s i &rest lst)
  (member (chr s i) lst :test #'equal))

(defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s)))
  (let ((x (let ((*read-eval* nil)) (read-from-string s1 nil ""))))
    (if (or (numberp x) (member x '(t nil ?))) x s1)))

(defun things (s &optional (sep #\,) (here 0))
  (let ((there (position sep s :start here)))
    (cons (thing (subseq s here there))
          (if there (things s sep (1+ there))))))

(defun mapcsv (fun file)
  (with-open-file (s file)
    (loop (funcall fun (things (or (read-line s nil) (return)))))))

(defun shuffle (lst &aux (a (coerce lst 'vector)))
  (loop for i from (1- (length a)) downto 1
        do (rotatef (aref a i) (aref a (random (1+ i)))))
  (coerce a 'list))

(defun bars (n)
  (with-output-to-string (s) (dotimes (_ n) (write-string "|.. " s))))

;;;; create ----------------------------------------------------------
(defmethod new (x) x)

(defmethod new ((self num))
  (when (eql #\- (chr (num-txt self) -1))
    (setf (num-goal self) 0))
  self)

(defmethod new ((self cols) &aux (at -1))
  (labels
    ((what  (s) (if (upper-case-p (chr s 0)) #'make-num #'make-sym))
     (one   (s) (new (funcall (what s) :txt s :at (incf at))))
     (skipc (c) (chrs (slot-value c 'txt) -1 #\- #\+ #\! #\X))
     (goalc (c) (chrs (slot-value c 'txt) -1 #\- #\+ #\!)))
    (setf (cols-all self)   (mapcar          #'one   (cols-names self))
          (cols-x self)     (remove-if       #'skipc (cols-all self))
          (cols-y self)     (remove-if-not   #'goalc (cols-all self)))
    self))

;;;; update ----------------------------------------------------------
(defun add (col v &optional (inc 1))
  (unless (eq v '?)
    (incf (slot-value col 'n) inc)
    (_add col v inc))
  v)

(defmethod _add ((self sym) v inc)
  (incf (gethash v (sym-has self) 0) inc))

(defmethod _add ((self num) v inc)
  (let ((d (- v (num-mu self))))
    (incf (num-mu self) (* inc (/ d (num-n self))))
    (incf (num-m2 self) (* inc d (- v (num-mu self))))))

(defun adds (lst &optional (col (make-num)))
  (dolist (x lst col) (add col x)))

;;;; data ------------------------------------------------------------
(defun data (&optional src)
  (let ((d (make-data)))
    (etypecase src
      (string (mapcsv (lambda (row) (data+ d row)) src))
      (list   (dolist (row src) (data+ d row)))
      (null   d))
    d))

(defun data+ (d row)
  (if (data-cols d)
    (progn (mapc (lambda (c v) (add c v)) (cols-all (data-cols d)) row)
           (push row (data-rows d)))
    (setf (data-cols d) (new (make-cols :names row))))
  d)

(defun clone (d &optional rows)
  (data (cons (cols-names (data-cols d)) rows)))

;;;; query -----------------------------------------------------------
(defun div (num)
  (if (< (num-n num) 2) 0
      (sqrt (/ (max 0 (num-m2 num)) (1- (num-n num))))))

(defun z (num v)
  (max -3 (min 3 (/ (- v (num-mu num)) (+ (div num) 1e-32)))))

(defun norm (num v)
  (/ 1.0 (1+ (exp (* -1.7 (z num v))))))

(defun disty (d row)
  (minkowski (mapcar (lambda (y)
                       (- (norm y (elt row (num-at y))) (num-goal y)))
                     (cols-y (data-cols d)))))

(defun minkowski (items &aux (n 0) (d 0))
  (dolist (x items (if (zerop n) 0 (expt (/ d n) (/ 1.0 (? p)))))
    (incf n)
    (incf d (expt x (? p)))))

;;;; tree ------------------------------------------------------------
(defun selects (rows at fn &aux lo hi)
  (dolist (r rows)
    (let ((v (elt r at)))
      (unless (eq v '?) (if (funcall fn v) (push r lo) (push r hi)))))
  (if (and (>= (length lo) (? leaf)) (>= (length hi) (? leaf)))
    (values lo hi) (values nil nil)))

(defun splits (col rows &aux (at (slot-value col 'at)))
  (if (sym-p col)
    (loop for v in (remove-duplicates
                     (remove '? (mapcar (lambda (r) (elt r at)) rows)))
          nconc (multiple-value-bind (lo hi) (selects rows at (lambda (x) (equal x v)))
                  (when lo (list (list v lo hi)))))
    (let ((vals (sort (remove '? (mapcar (lambda (r) (elt r at)) rows)) #'<)))
      (when (>= (length vals) 2)
        (let ((med (elt vals (floor (length vals) 2))))
          (multiple-value-bind (lo hi) (selects rows at (lambda (x) (<= x med)))
            (when lo (list (list med lo hi)))))))))

(defun w* (d rows)
  (* (length rows) (div (adds (mapcar (lambda (r) (disty d r)) rows)))))

(defun tree (d rows)
  (if (< (length rows) (* 2 (? leaf)))
    (make-node :y (adds (mapcar (lambda (r) (disty d r)) rows)))
    (let ((best nil) (bscore most-positive-double-float))
      (dolist (col (cols-x (data-cols d)))
        (dolist (split (splits col rows))
          (destructuring-bind (cut lo hi) split
            (let ((s (+ (w* d lo) (w* d hi))))
              (when (< s bscore)
                (setf bscore s best (list col cut lo hi)))))))
      (if best
        (destructuring-bind (col cut lo hi) best
          (make-node :col col :cut cut
                     :lo (tree d lo) :hi (tree d hi)))
        (make-node :y (adds (mapcar (lambda (r) (disty d r)) rows)))))))

(defun tree-leaf (nd row)
  (let ((col (node-col nd)))
    (if (null col) nd
      (let ((v (elt row (slot-value col 'at))))
        (tree-leaf
          (if (eq v '?) (node-lo nd)
            (if (if (sym-p col) (equal v (node-cut nd)) (<= v (node-cut nd)))
              (node-lo nd) (node-hi nd)))
          row)))))

(defun tree-show (nd &optional (lvl 0) (pre ""))
  (let ((s (if (string= pre "") ""
               (concatenate 'string (bars (1- lvl)) pre))))
    (if (node-col nd)
      (let* ((col (node-col nd))
             (txt (slot-value col 'txt))
             (sym? (sym-p col)))
        (when (string/= pre "") (format t "~a~%" s))
        (tree-show (node-lo nd) (1+ lvl)
           (format nil "~a ~a ~a" txt (if sym? "==" "<=") (node-cut nd)))
        (tree-show (node-hi nd) (1+ lvl)
           (format nil "~a ~a ~a" txt (if sym? "!=" ">") (node-cut nd))))
      (format t "~va ~6,2f (~d)~%" (? Show) s
              (num-mu (node-y nd)) (num-n (node-y nd))))))

;;;; main ------------------------------------------------------------
(setf *random-state* (sb-ext:seed-random-state (? seed)))

(let* ((d  (data (? file)))
       (rs (shuffle (data-rows d)))
       (d1 (clone d (subseq rs 0 (min (? Budget) (length rs))))))
  (tree-show (tree d1 (data-rows d1))))
