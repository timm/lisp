; vim:  ts=2 sw=3 sts=2 et :
(load "etc")
(defvar our nil)

(defun _our()
  '(all (eg    "eg.hi"     ; default thing to run
         tries 0           ; number of runs
         fails 0           ; number of failed runs 
         seed 10013        ; random number seed
         data "../data/auto93.csv" ; data file to load
         loud nil          ; verbose mode
         meek nil)         ; meek mode: about  on any error
    col (p 2)              ; distance function coeffecient
    dom (samples 100)      ; samples for exploring domination
))

;------------------;-------------------;-------------------;-------------------;
; structs
(defstruct thing)
(defstruct (col    (:include thing)) (txt "") (at 0)  (n 0)  (w 1) )
(defstruct (skip   (:include col)))
(defstruct (sym    (:include col))   seen mode (most 0))
(defstruct (num    (:include col))   _all sorted)
(defstruct (row    (:include thing)) _rows cells)
(defstruct (cols   (:include thing)) names all x y klass)
(defstruct (sample (:include thing)) rows cols)

(defmethod add ((c col) (x cons)) (dolist (y x c) (add c y)))
(defmethod add ((c col) x)
  (unless (eq #\? x) 
    (incf (? c n)) 
    (add1 c x))
  x)

(defmethod add1  ((s skip) x) x)

(defmethod add1 ((s sym) x)
  (let ((n (inca x (? s seen))))
    (when (> n (? s most))
      (setf (? s most) n
            (? s mode) x))))

(defmethod add1 ((n num) (x string)) (add1 n (read-from-string x)))
(defmethod add1 ((n num) (x number))
  (push (? n _all) x)
  (setf (? n sorted) nil))
(defmethod  all ((n num))
  (unless (? n sorted) (setf (? n sorted) t)  (sort (? n _all) #'<))
  (? n _all))

(defun col+ (txt at sample)
  (let* 
    ((w  (if (find #\- txt) -1 1))
     (it (cond 
           ((find #\? txt)              (make-skip :txt txt :at at :w w))
           ((upper-case-p (char txt 0)) (make-num  :txt txt :at at :w w))
           (t                           (make-sym  :txt txt :at at :w w)))))
    (push (? sample cols all) it)
    (unless (find #\? txt)
      (cond ((find #\- txt) (push (? sample cols y) it))
            ((find #\+ txt) (push (? sample cols y) it))
            (t              (push (? sample cols x) it))))
    it))

(defmethod update ((s sample) (file string)) 
  (print file)
  (with-csv (lst file) (print `(lst ,lst)) (update1 s lst)))

(defun update1 (s lst &aux (n 0)) 
  (labels ((head  (x)     (col+ x (incf n) s))
           (datum (x col) (add col x))
           (data  (lst)   (mapcar #'datum lst (? s cols))))
    (print 11000)
    (if (? s cols)
      (push (? s rows) (make-row :_rows s :cells (data lst)))
      (setf (? s cols) (mapcar #'head lst)))
    s))

(defun eg.sample ()
  (let ((s (update (make-sample) (! our all data))))
    (print (? s cols))))

;(with-csv (lst  "../data/auto93.csv") (print  lst))
(setf our (_our))
;(eg.sample)
