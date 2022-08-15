; Summarize numeric columns.
(defstruct+ num (txt "")  ; column name
                (at 0)    ; column position
                (n 0)     ; #items seen
                (w 1)     ; (1,-1) = (maximize, minimize)
                (lo most-positive-fixnum) ; least seen
                (hi most-negative-fixnum) ; most seen
                (kept (make-sample)))     ; items seen


(defun make-num (&optional (s "") (n 0)) 
  (%make-num :txt s :at n :w (if (eq #\- (charn s)) -1 1)))

(defmethod add ((i num) (lst cons)) (dolist (x lst i) (add i x)))
(defmethod add ((i num) x)
  (with-slots (lo hi) i
    (unless (eq x #\?)
      (incf (? i n))
      (add (? i kept) x))
      (setf lo (min x (? i lo))
            hi (max x (? i hi)))))

(defmethod norm ((i num) x)
  (with-slots (lo hi) i
    (cond ((eq x #\?)          x)
          ((< (- hi lo) 1E-12) 0)
          (t                   (/ (- x lo) (- hi lo))))))

(defmethod dist ((i num) x y)
  (cond ((and (eq #\? x) (eq #\? y)) 
                     (return-from dist 1))
        ((eq #\? x)  (setf y (norm i y) x (if (< y .5) 1 0)))
        ((eq #\? y)  (setf x (norm i x) y (if (< x .5) 1 0)))
        (t           (setf x (norm i x) y (norm i y))))
  (abs (- x y)))

(defmethod div ((i num)) (div (? i kept)))
(defmethod mid ((i num)) (mid (? i kept)))

(defmethod discretize ((i num) x &optional (bins (? my bins)))
  (with-slots (lo hi) i
    (let ((b (/ (- hi lo) bins)))
      (if (= hi lo) 1 (* b (floor (+ .5 (/ x b))))))))
