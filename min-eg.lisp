(load "min")
(setf (@ file) "/Users/timm/gits/moot/optimize/misc/auto93.csv")

(defun eg-the () (format t "the: seed=~a p=~a file=~a goods=~a~%"
                         (@ seed) (@ p) (@ file) (@ goods)))

(defun eg-cols (&aux (i (tbl)))
  (format t "rows=~a x=~a y=~a~%" (length (? i rows))
          (mapcar (lambda (c) (? c txt)) (? i x))
          (mapcar (lambda (c) (? c txt)) (? i y)))
  (dolist (c (? i cols))
    (format t "  ~10a ~4a n=~4a mid=~8,2f div=~6,2f~%" (? c txt)
            (if (sym-p c) "sym" "num") (? c n)
            (if (sym-p c) 0 (? c mu)) (div c))))

(defun eg-bins (&aux (i (tbl)) (h (o)))
  (let ((c (find "Mpg+" (? i cols) :key (lambda (z) (? z txt))
                 :test #'equal)))
    (format t "norm: ~{~a~^ ~}~%"
            (loop for v in '(10 20 30 40 50)
                  collect (format nil "~a->~,3f" v (norm c v))))
    (dolist (row (? i rows)) (incf (gethash (bin c (svref row (? c at))) h 0)))
    (loop for b from 1 to 5 do
      (format t "  bin ~a n=~4a~%" b (gethash b h 0)))))

(defun eg-train (&aux (a (tbl)) (m (train a (split a))) rows)
  (format t "~&~a rows sampled of ~a; mean ydist per bin:~%"
          (@ budget) (length (? a rows)))
  (loop for at being the hash-keys of (? m arms) using (hash-value arm)
        do (loop for b being the hash-keys of (? arm bins)
                   using (hash-value v)
                 do (push (list (elt (? a names) at) b v (? arm w))
                          rows)))
  (dolist (z (sort rows (lambda (p q) (< (? (third p) mu)
                                         (? (third q) mu)))))
    (format t "  ~10a bin ~a n=~3a ydist=~,3f w=~,4f~%"
            (first z) (second z) (? (third z) n) (? (third z) mu)
            (fourth z))))

(defun eg-test (&aux (a (tbl)) (e1 (num)) (e2 (num)) (all (num)))
  (multiple-value-bind (train test) (split a)
    (let ((ys (train a train)))
      (dolist (r (? a rows)) (add all (ydist a r)))
      (dolist (r test)
        (add e1 (abs (- (guess ys r) (ydist a r))))      ; the bins
        (add e2 (abs (- (? all mu)    (ydist a r)))))      ; the mean
      (format t "~&test ~a rows: bins err=~,3f  mean-guess err=~,3f~%"
              (? e1 n) (? e1 mu) (? e2 mu))
      (format t "one row -> ~a nums~%"
              (length (bins ys (first test)))))))

(defun eg-crash () (error "deliberate ~a" 'boom))
(cli)
