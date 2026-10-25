;;; arms.lisp: min.lisp's rmeans vs LightGBM, on the cached splits
;;; in ~/tmp/ezrreg (see ezr/etc/rcache.py).  Metric and rig copy
;;; ezr/etc/arms.py: 80:20, 20 seeds, our arm capped at 256 training
;;; rows.  err here is RAW mae = mean|y-yhat|, not normalized.
(load "/Users/timm/gh/lisp/min")

(defparameter *cap*   256)
(defparameter *caps* '(25 50 100 200 0))    ; 0 means every row
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
          do (incf e1 (abs (- (funcall y r) (guess m r))))
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
                  (if (same a b)
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

(defun curve (dir seed &aux out)
  "err at each of *caps* training rows, then LightGBM's err."
  (let* ((tr (tbl (format nil "~a/~a.tr.csv" dir seed)))
         (te (reverse (? (tbl (format nil "~a/~a.te.csv" dir seed))
                         rows)))
         (k  (klass tr))
         (y  (lambda (r) (elt r (? k at))))
         (rows (shuffle (reverse (? tr rows))))
         (n  (length rows))
         (e2 0))
    (dolist (cap *caps*)
      (let ((m (train tr (subseq rows 0 (if (zerop cap) n
                                          (min cap n))) y))
            (e1 0))
        (dolist (r te) (incf e1 (abs (- (funcall y r)
                                        (guess m r)))))
        (push (/ e1 (length te)) out)))
    (loop for r in te for p in (lgbms (format nil "~a/~a.lgbm" dir seed))
          do (incf e2 (abs (- (funcall y r) p))))
    (values (nreverse out) (/ e2 (length te)) n)))

(defun eg-learn (&aux rows)
  "Learning curve: raw MAE at 125, 250, 500, 1000, all rows."
  (format t "~&~20a ~6a" "data" "rows")
  (dolist (cap *caps*) (format t " ~10a" (if (zerop cap) "all" cap)))
  (format t " ~10a ~a ~a~%" "lgbm" "50" "all")
  (dolist (data (datas))
    (let ((es (mapcar (lambda (x) (declare (ignore x)) nil) *caps*))
          (gs ()) (n 0))
      (setf es (mapcar (lambda (x) (declare (ignore x)) ()) *caps*))
      (dotimes (s *seeds*)
        (multiple-value-bind (e1s e2 rows1)
            (curve (format nil "~a/~a" *dir* data) s)
          (setf n rows1)
          (setf es (mapcar #'cons e1s es))
          (push e2 gs)))
      (push (list data n (mapcar #'mid es) (mid gs)) rows)
      (format t "~20a ~6a" data n)
      (dolist (e (mapcar #'mid es)) (format t " ~10,4f" e))
      (format t " ~10,4f" (mid gs))
      (dolist (one (list (second es) (car (last es))))
        (format t " ~a" (if (same one gs) "=" 
                          (if (< (mid one) (mid gs)) "<" ">"))))
      (format t "~%")))
  rows)

(defparameter *budget* 50)              ; labels, all in: ezr's Stop
(defparameter *check*   5)              ; of those, saved for test

(defun ranks1 (dir seed &aux out)
  "Percentile of the best of the top *check* guesses, per cap."
  (let* ((tr (tbl (format nil "~a/~a.tr.csv" dir seed)))
         (te (reverse (? (tbl (format nil "~a/~a.te.csv" dir seed))
                         rows)))
         (k  (klass tr))
         (y  (lambda (r) (elt r (? k at))))
         (rows (shuffle (reverse (? tr rows))))
         (n  (length te))
         (ys (sort (mapcar y te) #'<)))
    (dolist (cap (list (- *budget* *check*) 0)
                 (values (nreverse out) n))
      (let* ((m   (train tr (subseq rows 0 (if (zerop cap)
                                             (length rows)
                                             (min cap (length rows))))
                         y))
             (top (subseq (sort (copy-list te) #'<
                                :key (lambda (r) (guess m r)))
                          0 (min *check* n)))
             (best (reduce #'min (mapcar y top))))
        (push (* 100.0 (/ (position best ys) (max 1 (1- n)))) out)))))

(defun eg-rank ()
  "Best of our top 5 guesses: where does it really rank?"
  (format t "~&~20a ~6a ~8a ~8a~%" "data" "test" "r25" "rall")
  (dolist (data (datas))
    (let ((a ()) (b ()) (n 0))
      (dotimes (s *seeds*)
        (multiple-value-bind (two rows)
            (ranks1 (format nil "~a/~a" *dir* data) s)
          (setf n rows)
          (push (first two) a)
          (push (second two) b)))
      (format t "~20a ~6a ~8,2f ~8,2f~%" data n (mid a) (mid b)))))

(defparameter *opt* "/Users/timm/gits/moot/optimize")

(defun opts1 (file &aux out)
  "Top-5 percentile on FILE, y = ydist over every goal."
  (let* ((all (tbl file))
         (rows (shuffle (reverse (? all rows))))
         (cut  (floor (* .8 (length rows))))
         (tr   (clone all (subseq rows 0 cut)))
         (te   (subseq rows cut))
         (y    (lambda (r) (ydist tr r)))
         (n    (length te))
         (ys   (sort (mapcar y te) #'<)))
    (dolist (cap (list (- *budget* *check*) 0)
                 (values (nreverse out) n))
      (let* ((m   (train tr (subseq (? tr rows) 0
                                    (if (zerop cap) cut
                                      (min cap cut))) y))
             (top (subseq (sort (copy-list te) #'<
                                :key (lambda (r) (guess m r)))
                          0 (min *check* n)))
             (best (reduce #'min (mapcar y top))))
        (push (* 100.0 (/ (position best ys) (max 1 (1- n)))) out)))))

(defun optdatas ()
  "Every moot/optimize csv, except the test ones."
  (sort (remove-if (lambda (f) (search "/test/" f))
          (mapcar #'namestring
                  (directory (format nil "~a/*/*.csv" *opt*))))
        #'string<))

(defun eg-opt1 (&aux (a ()) (b ()) (n 0))
  "One file (-file F), so a shell can fan these out."
  (dotimes (s *seeds*)
    (srand (+ 1 s))
    (multiple-value-bind (two rows) (opts1 (@ file))
      (setf n rows)
      (push (first two) a)
      (push (second two) b)))
  (format t "~24a ~6a ~8,2f ~8,2f~%"
          (pathname-name (@ file)) n (mid a) (mid b)))

(defun eg-opt ()
  "Multi-objective: rank by guessed ydist, label the top 5."
  (format t "~&~24a ~6a ~8a ~8a~%" "data" "test" "r25" "rall")
  (dolist (file (optdatas))
    (let ((a ()) (b ()) (n 0))
      (dotimes (s *seeds*)
        (srand (+ 1 s))
        (multiple-value-bind (two rows) (opts1 file)
          (setf n rows)
          (push (first two) a)
          (push (second two) b)))
      (format t "~24a ~6a ~8,2f ~8,2f~%"
              (pathname-name file) n (mid a) (mid b)))))

(defun eg-secs1 (&aux (reps 3) (a 0) (n 0))
  "Seconds for one file: train on the budget, rank every test row."
  (let ((all (tbl (@ file))))
    (dotimes (s reps)
      (srand (+ 1 s))
      (let* ((rows (shuffle (reverse (? all rows))))
             (cut  (floor (* .8 (length rows))))
             (tr   (clone all (subseq rows 0 cut)))
             (te   (subseq rows cut))
             (y    (lambda (r) (ydist tr r))))
        (setf n (length rows))
        (incf a (secs (lambda (&aux (m (train tr (subseq (? tr rows) 0
                              (min (- *budget* *check*) cut)) y)))
                        (dolist (r te) (guess m r)))))))
    (format t "~a ~a ~,4f~%" (pathname-name (@ file)) n (/ a reps))))

(defun eg-mini (&aux (ds (list "auto93" "servo" "housing")))
  "Three small sets, one seed: does the curve look sane?"
  (dolist (d ds)
    (multiple-value-bind (es g n) (curve (format nil "~a/~a" *dir* d) 0)
      (format t "~&~10a rows=~5a caps ~{~7,2f~} lgbm ~7,2f~%"
              d n es g))))

(defparameter *few* '("servo" "housing" "abalone" "cpu.act"
                      "2dplanes"))

(defun secs (f &aux (t0 (get-internal-real-time))) "Seconds to run F."
  (funcall f)
  (/ (- (get-internal-real-time) t0)
     internal-time-units-per-second))

(defun eg-secs (&aux (reps 3))
  "Train+predict seconds, at 50 labels and at every row."
  (format t "~&~12a ~7a ~9a ~9a~%" "data" "rows" "rmeans45" "rmeansAll")
  (dolist (data *few*)
    (let ((dir (format nil "~a/~a" *dir* data)) (a 0) (b 0) (n 0))
      (dotimes (s reps)
        (let* ((tr (tbl (format nil "~a/~a.tr.csv" dir s)))
               (te (reverse (? (tbl (format nil "~a/~a.te.csv" dir s))
                               rows)))
               (k (klass tr))
               (y (lambda (r) (elt r (? k at))))
               (rows (shuffle (reverse (? tr rows)))))
          (setf n (length rows))
          (incf a (secs (lambda (&aux (m (train tr (subseq rows 0
                                  (min (- *budget* *check*) n)) y)))
                          (dolist (r te) (guess m r)))))
          (incf b (secs (lambda (&aux (m (train tr rows y)))
                          (dolist (r te) (guess m r)))))))
      (format t "~12a ~7a ~9,3f ~9,3f~%" data n (/ a reps) (/ b reps))))) 

(defun eg-one (&aux (d (format nil "~a/auto93" *dir*)))
  "One data set, one seed, as a smoke test."
  (multiple-value-bind (a b) (one d 0)
    (format t "~&auto93 seed 0: rmeans=~,4f lgbm=~,4f~%" a b)))

(cli)
