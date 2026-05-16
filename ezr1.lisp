;;;; ezr.lisp : CL port of ezr.at (sketch).
;;;; Uses LOOP + 4 custom macros (->, has, $, o).
;;;; Zero external libs.

(defpackage :ezr (:use :cl))
(in-package :ezr)

;;; ## Macros ----------------------------------------------

;; Short lambda. (-> x (* x 2)) = (lambda (x) (* x 2))
(defmacro -> (args &body body)
  `(lambda ,(if (listp args) args (list args))
     ,@body))

;; Auto-counter on alist `lst` for key `x`.
;; (incf (has v counts)) bumps or inserts.
(defmacro has (x lst)
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst
                       (cons (cons ,x 0) ,lst))))))

;; $foo => (slot-value self 'foo)
(set-macro-character #\$
  (-> (stream char)
    `(slot-value self
                 ',(read stream t nil t))))

;; Nested slot path. (o x a b c) = x.a.b.c
(defmacro o (x f &rest fs)
  (if fs
      `(o (slot-value ,x ',f) . ,fs)
      `(slot-value ,x ',f)))

;;; ## Config ----------------------------------------------

(defparameter *the*
  '(:budget 50 :check 5 :cliffs 0.195
    :eps 0.35 :ksconf 1.36 :leaf 3 :p 2
    :seed 1 :show 30))

(defun cfg (k) (getf *the* k))

;;; ## Classes ---------------------------------------------

(defclass col ()
  ((txt :initarg :txt :initform "")
   (at  :initarg :at  :initform 0)
   (n   :initform 0)))

(defclass num (col)
  ((mu :initform 0d0)
   (m2 :initform 0d0)
   (heaven :initform 1)))

(defclass sym (col)
  ((has :initform nil)))

(defclass cols ()
  ((xs :initform nil) (ys :initform nil)
   (all :initform nil) (names :initarg :names)))

(defclass data ()
  ((rows :initform nil) (cols :initform nil)))

(defclass tree ()
  ((score :initarg :score) (y) (mids)
   (col :initform nil)
   (cut :initform nil)
   (at  :initform nil)
   (left :initform nil)
   (right :initform nil)))

;;; ## Constructors ----------------------------------------

(defun new-num (txt at)
  (let ((o (make-instance 'num :txt txt :at at)))
    (setf (slot-value o 'heaven)
          (if (and (> (length txt) 0)
                   (eql (aref txt (1- (length txt)))
                        #\-))
              0 1))
    o))

(defun new-sym (txt at)
  (make-instance 'sym :txt txt :at at))

;;; ## Update ----------------------------------------------

(defgeneric add (self v &optional w))

(defmethod add ((self num) v &optional (w 1))
  (unless (equal v "?")
    (incf $n w)
    (if (and (minusp w) (<= $n 1))
        (setf $n 0 $mu 0d0 $m2 0d0)
        (let ((err (- v $mu)))
          (incf $mu (/ (* w err) $n))
          (incf $m2 (* w err (- v $mu))))))
  v)

(defmethod add ((self sym) v &optional (w 1))
  (unless (equal v "?")
    (incf $n w)
    (incf (has v $has) w))
  v)

(defun adds (vs &optional (it (make-instance 'num)))
  (loop for v in vs do (add it v))
  it)

(defun sub (col v) (add col v -1))

;;; ## Query -----------------------------------------------

(defgeneric mid (self))
(defmethod mid ((self num)) $mu)
(defmethod mid ((self sym))
  (loop for (k . v) in $has
        with best = -1 with chosen = nil
        when (> v best)
          do (setf best v chosen k)
        finally (return chosen)))

(defgeneric spread (self))
(defmethod spread ((self num))
  (if (> $n 1)
      (sqrt (max 0 (/ $m2 (1- $n))))
      0))
(defmethod spread ((self sym))
  (- (loop for (_ . v) in $has
           sum (* (/ v $n) (log (/ v $n) 2)))))

(defun norm-val (self v)
  (if (equal v "?") v
      (/ 1
         (1+ (exp (* -1.7
                     (max -3
                       (min 3
                         (/ (- v (slot-value self 'mu))
                            (+ (spread self)
                               1e-32))))))))))

;;; ## Cols + Data -----------------------------------------

(defun new-cols (names)
  (let ((c (make-instance 'cols :names names)))
    (loop for s in names for i from 1
          for col = (if (upper-case-p (aref s 0))
                        (new-num s i)
                        (new-sym s i))
          do (push col (slot-value c 'all))
             (unless (eql (aref s (1- (length s)))
                          #\X)
               (if (find (aref s (1- (length s)))
                         "+-!" :test #'eql)
                   (push col (slot-value c 'ys))
                   (push col (slot-value c 'xs)))))
    c))

(defun data-add (d row &optional (w 1))
  (cond ((null (slot-value d 'cols))
         (setf (slot-value d 'cols)
               (new-cols row)))
        (t (loop for c in (o d cols all) do
             (add c (nth (1- (slot-value c 'at))
                         row)
                  w))
           (when (plusp w)
             (push row (slot-value d 'rows))))))

(defun clone (d &optional rows)
  (let ((d2 (make-instance 'data)))
    (data-add d2 (slot-value (o d cols) 'names))
    (loop for r in rows do (data-add d2 r))
    d2))

(defun mid-row (d)
  (loop for c in (o d cols all)
        collect (mid c)))

;;; ## Distance + score ------------------------------------

(defun disty (d row)
  (let* ((ys (o d cols ys))
         (p  (cfg :p))
         (s  (loop for c in ys
                   sum (expt
                         (abs
                           (- (norm-val c
                                (nth (1- (slot-value c 'at))
                                     row))
                              (slot-value c 'heaven)))
                         p))))
    (expt (/ s (length ys)) (/ 1 p))))

;;; ## Tree ------------------------------------------------

(defun split-rows (col rows score cut test)
  ;; partition rows on test; gather lhs/rhs Num stats.
  (let ((lhs (make-instance 'num))
        (rhs (make-instance 'num))
        (left nil) (right nil))
    (loop for row in rows
          for v = (nth (1- (slot-value col 'at)) row)
          for ok = (or (equal v "?")
                       (funcall test v))
          do (if ok (push row left)
                    (push row right))
             (add (if ok lhs rhs)
                  (funcall score row)))
    (when (and (>= (length left)  (cfg :leaf))
               (>= (length right) (cfg :leaf)))
      (list :col col :cut cut
            :left left :right right
            :lhs lhs :rhs rhs))))

(defgeneric splits (self rows score))

(defmethod splits ((self num) rows score)
  (let* ((vs (sort
               (loop for r in rows
                     for v = (nth (1- $at) r)
                     unless (equal v "?") collect v)
               #'<))
         (mu (and (>= (length vs) 2)
                  (nth (floor (length vs) 2) vs))))
    (if mu
        (let ((c (split-rows self rows score mu
                             (lambda (v) (<= v mu)))))
          (and c (list c)))
        '())))

(defmethod splits ((self sym) rows score)
  (let ((seen (make-hash-table :test 'equal))
        (out  nil))
    (loop for r in rows
          for v = (nth (1- $at) r)
          unless (or (equal v "?")
                     (gethash v seen))
          do (setf (gethash v seen) t)
             (let ((c (split-rows self rows score v
                        (lambda (x) (equal x v)))))
               (when c (push c out))))
    out))

(defun best-split (d rows score)
  (let ((best nil) (bw most-positive-double-float))
    (loop for col in (o d cols xs) do
      (loop for cut in (splits col rows score)
            for lhs = (getf cut :lhs)
            for rhs = (getf cut :rhs)
            for w = (+ (* (slot-value lhs 'n)
                          (spread lhs))
                       (* (slot-value rhs 'n)
                          (spread rhs)))
            when (< w bw)
              do (setf best cut bw w)))
    best))

(defun tree-build (score d rows)
  (let ((tr (make-instance 'tree :score score)))
    (setf (slot-value tr 'y)
          (adds (mapcar score rows)))
    (setf (slot-value tr 'mids) (mid-row (clone d rows)))
    (when (>= (length rows) (* 2 (cfg :leaf)))
      (let ((best (best-split d rows score)))
        (when best
          (setf (slot-value tr 'col)  (getf best :col)
                (slot-value tr 'cut)  (getf best :cut)
                (slot-value tr 'at)
                  (slot-value (getf best :col) 'at)
                (slot-value tr 'left)
                  (tree-build score d (getf best :left))
                (slot-value tr 'right)
                  (tree-build score d
                              (getf best :right))))))
    tr))

(defun tree-leaf (tr row)
  (if (null (slot-value tr 'col)) tr
      (let* ((at  (slot-value tr 'at))
             (col (slot-value tr 'col))
             (cut (slot-value tr 'cut))
             (v   (nth (1- at) row)))
        (cond
          ((equal v "?")
           (tree-leaf (slot-value tr 'left) row))
          (t (tree-leaf
              (if (typep col 'num)
                  (if (<= v cut)
                      (slot-value tr 'left)
                      (slot-value tr 'right))
                  (if (equal v cut)
                      (slot-value tr 'left)
                      (slot-value tr 'right)))
              row))))))

(defun tree-nodes (tr fn &optional (lvl 0) (pre ""))
  (funcall fn tr lvl pre)
  (when (slot-value tr 'col)
    (let* ((col (slot-value tr 'col))
           (numeric (typep col 'num))
           (sy  (if numeric "<=" "=="))
           (sn  (if numeric ">"  "!="))
           (kids
            (sort
             (list
              (list (slot-value tr 'left)  sy)
              (list (slot-value tr 'right) sn))
             #'<
             :key (lambda (k)
                    (mid (slot-value (car k) 'y))))))
      (loop for (kid op) in kids do
        (tree-nodes kid fn (1+ lvl)
          (format nil "~A ~A ~A"
                  (slot-value col 'txt)
                  op
                  (slot-value tr 'cut)))))))

(defun fmt-cell (x)
  (cond ((floatp x) (format nil "~,2F" x))
        ((integerp x) (format nil "~D" x))
        (t (format nil "~A" x))))

(defun fmt-mids (xs)
  (format nil "{~{~A~^, ~}}"
          (mapcar #'fmt-cell xs)))

(defun tree-show (tr)
  (tree-nodes tr
    (lambda (node lvl pre)
      (let ((indent
              (if (plusp lvl)
                  (concatenate 'string
                    (apply #'concatenate 'string
                      (loop repeat (1- lvl)
                            collect "|   "))
                    pre)
                  "")))
        (format t "~vA, ~5,2F ,(~3D), ~A~%"
                (cfg :show) indent
                (mid (slot-value node 'y))
                (slot-value (slot-value node 'y) 'n)
                (fmt-mids (slot-value node 'mids)))))))

;;; ## CSV reader ------------------------------------------

(defun split-comma (s)
  (loop for i = 0 then (1+ j)
        for j = (position #\, s :start i)
        collect (string-trim " "
                  (subseq s i j))
        while j))

(defun coerce-cell (s)
  (cond ((equal s "true") t)
        ((equal s "false") nil)
        (t (multiple-value-bind (n end)
               (parse-integer s :junk-allowed t)
             (if (and n (= end (length s)))
                 n
                 (let ((f (read-from-string s nil nil)))
                   (if (numberp f) f s)))))))

(defun read-csv (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
          while line
          collect (mapcar #'coerce-cell
                          (split-comma line)))))

(defun new-data (path-or-rows)
  (let ((d (make-instance 'data)))
    (loop for row in
          (if (stringp path-or-rows)
              (read-csv path-or-rows)
              path-or-rows)
          do (data-add d row))
    d))

;;; ## Stats helpers ---------------------------------------

(defun bisect (xs x)
  ;; count of elts <= x in sorted xs.
  (let ((lo 0) (hi (length xs)))
    (loop while (< lo hi)
          for m = (floor (+ lo hi) 2)
          do (if (<= (elt xs m) x)
                 (setf lo (1+ m))
                 (setf hi m)))
    lo))

(defun weibull (k lam)
  (* lam (expt (- (log (- 1 (random 1.0))))
               (/ 1 k))))

(defun wins (d)
  (let* ((ys (sort
              (mapcar (lambda (r) (disty d r))
                      (slot-value d 'rows))
              #'<))
         (lo    (first ys))
         (n-mid (nth (floor (length ys) 2) ys)))
    (lambda (row)
      (floor (* 100
                (- 1 (/ (- (disty d row) lo)
                        (+ (- n-mid lo) 1e-32))))))))

(defun same (xs ys eps)
  (let* ((xs (sort (copy-list xs) #'<))
         (ys (sort (copy-list ys) #'<))
         (n (length xs)) (m (length ys)))
    (cond
      ((<= (abs (- (nth (floor n 2) xs)
                   (nth (floor m 2) ys)))
           eps)
       t)
      (t
       (let ((ngt 0) (nlt 0))
         (loop for v in xs do
           (incf ngt (bisect ys v))
           (incf nlt (- m (bisect ys (+ v 1e-32)))))
         (cond
           ((> (/ (abs (- ngt nlt)) (* n m))
               (cfg :cliffs))
            nil)
           (t
            (let ((ks 0))
              (loop for v in (append xs ys) do
                (setf ks
                  (max ks
                       (abs (- (/ (bisect xs v) n)
                               (/ (bisect ys v) m))))))
              (<= ks
                  (* (cfg :ksconf)
                     (sqrt (/ (+ n m)
                              (* n m)))))))))))))

(defun best-ranks (dict)
  ;; dict :: hash-table  name -> list of samples.
  (let* ((names
           (sort
             (loop for k being the hash-keys of dict
                   collect k)
             #'<
             :key (lambda (k)
                    (mid (adds (gethash k dict))))))
         (top  (gethash (first names) dict))
         (eps  (* (spread (adds top)) (cfg :eps)))
         (out  (make-hash-table :test 'equal)))
    (setf (gethash (first names) out)
          (adds top (new-num (first names) 0)))
    (loop for n in (rest names)
          while (same top (gethash n dict) eps)
          do (setf (gethash n out)
                   (adds top (new-num n 0))))
    out))

;;; ## Examples --------------------------------------------

(defun eg-data (path)
  (let ((d (new-data path)))
    (loop for c in (o d cols ys) do
      (format t "~A~T~A~%"
              (slot-value c 'txt) (mid c)))))

(defun eg-csv (path)
  (loop for r in (read-csv path)
        for i from 0
        when (zerop (mod i 30))
          do (format t "~A~%" r)))

(defun shuffle (xs)
  (let ((v (coerce xs 'vector)))
    (loop for i from (1- (length v)) downto 1
          for j = (random (1+ i))
          do (rotatef (aref v i) (aref v j)))
    (coerce v 'list)))

(defun eg-tree (path)
  (let* ((d   (new-data path))
         (raw (slot-value d 'rows))
         (rs  (subseq (shuffle raw)
                      0 (min (cfg :budget) (length raw))))
         (d2  (clone d rs))
         (sc  (lambda (r) (disty d2 r)))
         (tr  (tree-build sc d2 (slot-value d2 'rows))))
    (tree-show tr)))

(defun eg-the (&optional path)
  (declare (ignore path))
  (format t "~A~%" *the*))

(defun eg-ranks (&optional path)
  (declare (ignore path))
  (let ((dict (make-hash-table :test 'equal)))
    (loop for n from 1 to 20 do
      (let ((name (format nil "t~D" n))
            (k    (if (<= n 5) 2 1))
            (lam  (if (<= n 5) 10 20)))
        (setf (gethash name dict)
              (loop repeat 50
                    collect (weibull k lam)))))
    (format t "~%Top Tier Treatments:~%")
    (let* ((br (best-ranks dict))
           (nums (sort
                  (loop for v being the hash-values of br
                        collect v)
                  #'<
                  :key (lambda (n) (slot-value n 'mu)))))
      (loop for num in nums do
        (format t "~5A median: ~5,2F~%"
                (slot-value num 'txt)
                (mid num))))))

(defun eg-test (path)
  (let ((d (new-data path)))
    (when (slot-value d 'cols)
      (let ((stats  (new-num "win" 0))
            (fn-win (wins d)))
        (loop repeat 20 do
          (let* ((rows  (shuffle (slot-value d 'rows)))
                 (half  (floor (length rows) 2))
                 (train (subseq rows 0
                          (min half (cfg :budget))))
                 (rest  (subseq rows half))
                 (d2    (clone d train))
                 (sc    (lambda (r) (disty d2 r)))
                 (node  (tree-build sc d2
                          (slot-value d2 'rows)))
                 (ranked
                   (sort rest #'<
                     :key (lambda (r)
                            (mid (slot-value
                                   (tree-leaf node r)
                                   'y)))))
                 (top
                   (sort
                     (subseq ranked 0
                       (min (cfg :check)
                            (length ranked)))
                     #'< :key (lambda (r) (disty d2 r)))))
            (add stats (funcall fn-win (first top)))))
        (format t "~D~%" (floor (mid stats)))))))

(defun eg-all (path)
  (loop for (name fn)
        in '(("--the"   eg-the)
             ("--csv"   eg-csv)
             ("--data"  eg-data)
             ("--tree"  eg-tree)
             ("--ranks" eg-ranks)
             ("--test"  eg-test))
        do (format t "~%~A~%" name)
           (funcall (symbol-function fn) path)))

;;; ## Main ------------------------------------------------

(defun main (&optional (args
                        #+sbcl (cdr sb-ext:*posix-argv*)
                        #-sbcl nil))
  (cond
    ((equal (car args) "-h")
     (format t "ezr.lisp: explainable MO opt~%"))
    ((equal (car args) "--the")   (eg-the))
    ((equal (car args) "--data")  (eg-data  (cadr args)))
    ((equal (car args) "--csv")   (eg-csv   (cadr args)))
    ((equal (car args) "--tree")  (eg-tree  (cadr args)))
    ((equal (car args) "--ranks") (eg-ranks))
    ((equal (car args) "--test")  (eg-test  (cadr args)))
    ((equal (car args) "--all")   (eg-all   (cadr args)))
    (t (format t "usage: -h | --the | --data | --csv~%~
                  | --tree | --ranks | --test | --all~%"))))


;;; --------------------------------------------------------
;;; Macro recap:
;;;   ->  short lambda    ; @(x) ... end equiv
;;;   has alist counter   ; sym freq tables
;;;   $   self.foo        ; method-body slot access
;;;   o   nested path     ; data.cols.ys etc
;;;
;;; LOOP already does list comprehension:
;;;   (loop for x in xs when (plusp x)
;;;         collect (* x 2))
