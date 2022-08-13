; Summarize numeric columns.
(defstruct+ num (txt "")  ; column name
                (at 0)    ; column position
                (n 0)     ; #items seen
                (w 1)     ; (1,-1) = (maximize, minimize)
                (lo most-positive-fixnum) ; least seen
                (hi most-negative-fixnum) ; most seen
                (kept (make-some))) ; items seen


(defun make-num (s n) (%make-num :txt s :at n :w (if (eq #\- (charn s)) -1 1)))

(defmethod add ((i num) (lst cons)) (dolist (x lst i) (add i x)))
(defmethod add ((i num) x)
  (unless (eq x #\?)
    (incf (? i n))
    (setf lo (min lo (? i lo))
          hi (max hi (? i hi)))
    (add (? i kept) x)))

(defmethod norm ((i num) x)
  (with-slots (lo hi) i
    (cond  ((eq x #\?)          x)
           ((< (- ho lo) 1E-12) 0)
           (t                   (/ (- x lo) (- hi lo))))))

(defmethod dist ((i num) x y)
  (cond ((and (eq #\? x) 
              (eq #\? y)) (return-from dist 1))
        ((eq #\? x)       (setf y (norm i y)
                                x (if (< y .5) 1 0)))
        ((eq #\? y)       (setf x (norm i x)
                                y (if (< x .5) 1 0)))
        (t                (setf x (norm i x)
                                y (norm i y))))
  (abs (- x y)))

(defmethod discretize ((i num) x)
  (with-slots (lo hi) i
    (let ((b (/ (- hi lo) (? my bins))))
      (if (- hi lo) 1 (* b (floor (+ .5 (/ x b))))))))
