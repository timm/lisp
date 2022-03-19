(defstruct settings (cohen .35) (bins 16))
(defvar **my** (make-settings))

(defmacro ? (x) `(slot-value **my** ',x))

(defun char0 (x)                          (char (symbol-name x) 0))
(defun charn (x &aux (s (symbol-name x))) (char s (1- (length s))))

(defun per (lst &optional (p .5)) (elt lst (floor (* p (length lst)))))

(defmethod goalp  ((s symbol)) (or (morep s) (lessp s) (klassp s)))
(defmethod nump   ((s symbol)) (eq (char0 s) #\.))
(defmethod klassp ((s symbol)) (eq (charn s) #\!))
(defmethod lessp  ((s symbol)) (eq (charn s) #\<))
(defmethod morep  ((s symbol)) (eq (charn s) #\>))
(defmethod lessp  ((s symbol)) (eq (charn s) #\<))

(defun csv (file &aux out it)
  (with-open-file (str file)
    (loop (if (setf it (read str nil))
            (push it out)
            (return-from csv (reverse out))))))

(defstruct range lo hi id where n div)

(defun bins (xys where  &aux out)
  (let* ((n      (1- (length xys)))
         (xys    (sort xys #'< :key #'first))
         (enough (floor (/ (length xys) (? bins))))
         (sd     (/ (- (first (per xys .9)) (first (per xys .1))) 2.56)))
    (argmin xys 0 n (* (? cohen) sd) enough 1E32 where out)
    (setf (range-hi (elt out n)) most-positive-fixnum)
    out))

(defun make () (load 'bnb))
       
