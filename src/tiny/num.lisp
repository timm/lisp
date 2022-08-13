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

