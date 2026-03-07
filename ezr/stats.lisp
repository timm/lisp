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
  (dohash (k i ks)
    (if (equal $label want)
      (if (equal got want) (incf $tp) (incf $fn))
      (if (equal got $label) (incf $fp) (incf $tn))))
  (incf (confuse-total cf)) got)

(defun finalize (i &aux (tp $tp) (fn $fn) (fp $fp) (tn $tn))
  (labels ((p (y z) (round (* 100 (/ y (max z 1e-32))))))
    (setf $pd (p tp (+ tp fn))     $prec (p tp (+ fp tp))
          $pf (p fp (+ fp tn))     $acc  (p (+ tp tn) (+ tp fp fn tn)))
    i))

(defun confused (cf &optional summary &aux (ks (confuse-klasses cf)))
  (if summary
    (let ((i (make-klass :label "_OVERALL")))
      (dohash (k c ks)
        (with-slots (tn fn fp tp) c
          (incf $tn tn) (incf $fn fn) (incf $fp fp) (incf $tp tp)))
      (finalize i))
    (sort (let (lst)
            (dohash (k i ks) (push (finalize i) lst))
            (push (confused cf t) lst) lst)
          #'< :key (lambda (i) (+ $fn $tp)))))

;; --- sample ---
(defun sample! (label lst &aux (i (make-sample :label label)))
  (dolist (x lst i) (add i x) (push x $vals))
  (setf $vals (sort $vals #'<)))

(defmethod add ((i sample) (j sample))
  (dolist (v (sample-vals j) i) (add i v) (push v $vals)))

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
         (l (copy-sample (car its))))
    (loop for r in (cdr its) for j from 1 do
      (unless (same l r)
        (return (mapcar #'sample-label (subseq its 0 j))))
      (add l r)
      finally (return (mapcar #'sample-label its)))))

;; --- test ---
(defun weibull (&optional (n 100))_
  (let ((shape (+ 0.5 (random 2.5)))
        (scale (+ 1.0 (random 3.0))))
    (loop repeat n collect
      (min 10 (* scale 2.5
                 (expt (- (log (random 1.0))) (/ 1 shape)))))))

(defun weibulls (&optional (m 20) (n 20))
  (let ((rxs (make-hash-table)))
    (loop for x below m do
      (setf (gethash x rxs) (weibull n)))
    (top rxs)))
