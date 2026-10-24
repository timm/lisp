;;; arms.lisp: min.lisp's rmeans vs LightGBM, on the cached splits
;;; in ~/tmp/ezrreg (see ezr/etc/rcache.py).  Metric and rig copy
;;; ezr/etc/arms.py: 80:20, 20 seeds, our arm capped at 256 training
;;; rows.  err here is RAW mae = mean|y-yhat|, not normalized.
(load "/Users/timm/gh/lisp/min")

(defparameter *cap*   256)
(defparameter *seeds* 20)
(defparameter *dir*   "/Users/timm/tmp/ezrreg")

(defun lgbms (f &aux out) "LightGBM's predictions, in file order."
  (with-open-file (s f)
    (loop for line = (read-line s nil) while line
          do (push (read-from-string line) out)))
  (nreverse out))

(defm klass (tbl) "The ! column." 
  (find-if (lambda (c) (eql #\! (at (? c txt) -1))) $y))

(defun one (dir seed) "err of ours, and of lgbm, on one split."
  (let* ((tr (tbl (format nil "~a/~a.tr.csv" dir seed)))
         (te (reverse (? (tbl (format nil "~a/~a.te.csv" dir seed)) rows)))
         (k  (klass tr))
         (y  (lambda (r) (elt r (? k at))))
         (all (reverse (? tr rows)))
         (some (subseq (shuffle all) 0 (min *cap* (length all))))
         (m  (train tr some y))
         (e1 0) (e2 0))
    (loop for r in te for p in (lgbms (format nil "~a/~a.lgbm" dir seed))
          do (incf e1 (abs (- (funcall y r) (guess tr m r))))
             (incf e2 (abs (- (funcall y r) p))))
    (values (/ e1 (length te)) (/ e2 (length te)))))

(defun mid (lst) "Median of LST." 
  (let ((v (sort (copy-list lst) #'<))) (elt v (floor (length v) 2))))

(defun datas () "The cached data sets, by name."
  (sort (mapcar (lambda (p) (car (last (pathname-directory p))))
                (directory (format nil "~a/*/" *dir*))) #'string<))

(defun eg-arms (&aux rows (w 0) (l 0))
  "Median err per data set, ours and LightGBM's, sorted by the gap."
  (dolist (data (datas))
    (let ((a ()) (b ()))
      (dotimes (s *seeds*)
        (multiple-value-bind (e1 e2)
            (one (format nil "~a/~a" *dir* data) s)
          (push e1 a) (push e2 b)))
      (push (list data (mid a) (mid b) (- (mid a) (mid b))
                  (if (same (sort (copy-list a) #'<)
                            (sort (copy-list b) #'<))
                    "="
                    (if (< (mid a) (mid b)) "rmeans" "lgbm")))
            rows)))
  (setf rows (sort rows #'< :key #'fourth))   ; sort is destructive
  (format t "~&~22a ~8a ~8a ~9a  ~a~%"
          "data" "rmeans" "lgbm" "diff" "win (= is a tie)")
  (dolist (z rows)
    (cond ((string= (fifth z) "rmeans") (incf w))
          ((string= (fifth z) "lgbm")   (incf l)))
    (format t "~22a ~8,3f ~8,3f ~+9,3f  ~a~%"
            (first z) (second z) (third z) (fourth z) (fifth z)))
  (format t "~&rmeans wins ~a, lgbm wins ~a, = on ~a of ~a~%"
          w l (- (length rows) w l) (length rows))
  (format t "medians ~,3f vs ~,3f~%" (mid (mapcar #'second rows))
          (mid (mapcar #'third rows))))

(defun eg-one (&aux (d (format nil "~a/auto93" *dir*)))
  "One data set, one seed, as a smoke test."
  (multiple-value-bind (a b) (one d 0)
    (format t "~&auto93 seed 0: rmeans=~,4f lgbm=~,4f~%" a b)))

(halt (cli))
