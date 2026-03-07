;; stats.lisp: confusion, effect size, top ranks
; vim: set et ts=2 sw=2 lisp:
(load "ez")

(defstruct klass label (tn 0) (fn 0) (fp 0) (tp 0) pd prec pf acc)
(defstruct confuse (klasses (make-hash-table :test 'equal)) (total 0))
(defstruct (sample (:include num)) label vals)

(defun confuse! (cf want got &aux (ks (confuse-klasses cf)))
  (dolist (x (list want got))
    (unless (gethash x ks)
      (setf (gethash x ks)
            (make-klass :label x :tn (confuse-total cf)))))
  (do-hash (k i ks)
    (if (equal $label want)
      (if (equal got want) (incf $tp) (incf $fn))
      (if (equal got $label) (incf $fp) (incf $tn))))
  (incf (confuse-total cf)) got)

(defun confuse-finalize (i &aux (tp $tp) (fn $fn) (fp $fp) (tn $tn))
  (labels ((p (y z) (round (* 100 (/ y (max z 1e-32))))))
    (setf $pd (p tp (+ tp fn))     $prec (p tp (+ fp tp))
          $pf (p fp (+ fp tn))     $acc  (p (+ tp tn) (+ tp fp fn tn)))
    i))

(defun confused (cf &optional summary &aux (ks (confuse-klasses cf)))
  (if summary
    (let ((i (make-klass :label "_OVERALL")))
      (do-hash (k c ks)
        (with-slots (tn fn fp tp) c
          (incf $tn tn) (incf $fn fn) (incf $fp fp) (incf $tp tp)))
      (confuse-finalize i))
    (sort (let (lst)
            (do-hash (k i ks) (push (confuse-finalize i) lst))
            (push (confused cf t) lst) lst)
          #'< :key (lambda (i) (+ $fn $tp)))))

;; --- sample ---
(defun sample! (label lst &aux (i (make-sample :label label)))
  (dolist (x lst) (add i x) (push x $vals))
  (setf $vals (sort $vals #'<))
  i)

(defmethod merge! ((i sample) (j sample))
  (dolist (v (sample-vals j) i) (add i v) (push v $vals)))
(defmethod merge! ((i sample) (j list))
  (dolist (s j i) (merge! i s)))

(defmethod minus ((i sample) (j sample))
  (dolist (v (sample-vals j) i)
    (sub i v) (setf $vals (remove v $vals :count 1))))

;; --- effect size + KS test ---
(defmethod cliffs ((i sample) (j sample) &aux (gt 0) (lt 0))
  (dolist (x $vals
           (/ (abs (- gt lt)) (* $n (num-n j))))
    (dolist (y (sample-vals j))
      (if (> x y) (incf gt)) (if (< x y) (incf lt)))))

(defmethod ks- ((i sample) (j sample) &aux (mx 0))
  (dolist (v (merge 'list (copy-list $vals)
                    (copy-list (sample-vals j)) #'<) mx)
    (let ((fx (/ (count-if (lambda (x) (<= x v)) $vals) $n))
          (fy (/ (count-if (lambda (x) (<= x v))
                           (sample-vals j)) (num-n j))))
      (setf mx (max mx (abs (- fx fy)))))))

(defmethod same ((i sample) (j sample))
  (and (<= (cliffs i j) (? delta))
       (<= (ks- i j)
           (* (? ks) (sqrt (/ (+ $n (num-n j))
                              (* $n (num-n j))))))))

;; --- scott-knott ---
(defun top (rxs &key (by #'<))
  (let* ((its (sort
                (loop for k being the hash-keys of rxs
                      using (hash-value v) when v
                      collect (sample! k v))
                by :key (lambda (i) $mu)))
         (all (merge! (make-sample) its))
         (ep (* (? eps) (spread all)))
         (l (car its)))
    (loop for s in (cdr its) for j from 1
      for diff = (and (> (abs (- (num-mu l) (num-mu s))) ep)
                      (not (same l s)))
      if diff
      return (mapcar #'sample-label (subseq its 0 j))
      else do (merge! l s)
      finally (return (mapcar #'sample-label its)))))

;; --- test ---
(defun weibull (&optional (n 100))
  (let ((shape (+ 0.5 (random 2.5)))
        (scale (+ 1.0 (random 3.0))))
    (loop repeat n collect
      (min 10 (* scale 2.5
                 (expt (- (log (random 1.0))) (/ 1 shape)))))))

(defun eg-cliffs (&optional f) f
  (let ((a (sample! :a '(1 2 3 4 5 6 7 8 9 10)))
        (b (sample! :b '(1 2 3 4 5 6 7 8 9 10)))
        (c (sample! :c '(10 20 30 40 50 60 70 80 90))))
    (format t "same: ~a~%" (< (cliffs a b) 0.2))
    (format t "diff: ~a~%" (> (cliffs a c) 0.8))))

(defun eg-ks (&optional f) f
  (let ((a (sample! :a '(1 2 3 4 5 6 7 8 9 10)))
        (b (sample! :b '(1 2 3 4 5 6 7 8 9 10)))
        (c (sample! :c '(10 20 30 40 50 60 70 80 90))))
    (format t "same: ~a~%" (< (ks- a b) 0.2))
    (format t "diff: ~a~%" (> (ks- a c) 0.8))))

(defun eg-same (&optional f) f
  (let ((a (sample! :a '(1 2 3 4 5 6 7 8 9 10)))
        (b (sample! :b '(1 2 3 4 5 6 7 8 9 10)))
        (c (sample! :c '(2 3 4 5 6 7 8 9 10 11)))
        (d (sample! :d '(10 20 30 40 50 60 70 80 90))))
    (format t "identical:  ~a~%" (same a b))
    (format t "close:      ~a~%" (same a c))
    (format t "far:        ~a~%" (not (same a d)))))

(defun eg-top5 (&optional f) f
  (let ((rxs (make-hash-table)))
    (setf (gethash :lo1 rxs) '(1 2 3 4 5)
          (gethash :lo2 rxs) '(2 3 4 5 6)
          (gethash :lo3 rxs) '(1 2 3 4 5)
          (gethash :hi1 rxs) '(20 30 40 50 60)
          (gethash :hi2 rxs) '(25 35 45 55 65))
    (print (top rxs))))

(defun eg-top2 (&optional f) f
  (let ((rxs (make-hash-table)))
    (setf (gethash :lo1 rxs) '(1 2 3 4 5)
          (gethash :lo3 rxs) '(1 2 3 4 5))
    (print (top rxs))))

(defun nudge (lst &optional (delta 0.1))
  (mapcar (lambda (x) (* delta x)) lst))

(defun eg-nudge (&optional f) f
  (let ((rxs (make-hash-table))
        (base (loop for i from 1 to 100 collect i)))
    (loop for k in '(:a :b :c :d :e) do
      (setf (gethash k rxs) base
            base (nudge base 1.25)))
    (print (top rxs))))

(defun eg-weibulls (&optional f (m 10) (n 20)) f
  (let ((rxs (make-hash-table)))
    (loop for x below m do
      (setf (gethash x rxs) (weibull n)))
    (print (top rxs))))

(main)
