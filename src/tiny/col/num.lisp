(defstruct+ num ;;; summarize numeric columns
   (txt "")  ; column name
   (at 0)    ; column position
   (n 0)     ; #items seen
   (w 1)     ; (1,-1) = (maximize, minimize)
   (lo most-positive-fixnum) ; least seen
   (hi most-negative-fixnum) ; most seen
   (_has (make-sample)))     ; items seen

(defun make-num (&optional (s "") (n 0)) ;;; create
  (%make-num :txt s :at n :w (if (eq #\- (charn s)) -1 1)))

(defmethod add ((i num) (lst cons)) ;;; Add a list of items
  (dolist (x lst i) (add i x)))

(defmethod add ((i num) x) ;;; Add one thing, updating 'lo,hi'
  (unless (eq x #\?)
    (with-slots (lo hi) i
      (incf (? i n))
      (add (? i _has) x)
      (setf lo (min x (? i lo))
            hi (max x (? i hi))))))

(defmethod norm ((i num) x) ;;; Map 'x' 0..1 (unless unknown, unless too small)
  (with-slots (lo hi) i
    (cond ((eq x #\?)         x)
          ((< (- hi lo) 1E-9) 0)
          (t                  (/ (- x lo) (- hi lo))))))

(defmethod dist ((i num) x y)
  "Gap between things (0..1). For unknowns, assume max distance."
  (cond ((and (eq #\? x) (eq #\? y)) (return-from dist 1))
        ((eq #\? x)                  (setf y (norm i y) x (if (< y .5) 1 0)))
        ((eq #\? y)                  (setf x (norm i x) y (if (< x .5) 1 0)))
        (t                           (setf x (norm i x) y (norm i y))))
  (abs (- x y)))

(defmethod mid ((i num)) 
  "Middle."
  (mid (? i _has)))

(defmethod div ((i num)) 
  "Diversity"
  (div (? i _has)))

(defmethod discretize ((i num) x bins)
  "Max 'x' to one of 'bins' integers."
  (with-slots (lo hi) i
    (let ((b (/ (- hi lo) bins)))
      (if (= hi lo) 1 (* b (floor (+ .5 (/ x b))))))))
