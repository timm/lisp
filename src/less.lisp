; vim:  ts=2 sw=3 sts=2 et :

(defvar +config+
  `(all (eg    "eg.hi"     ; default thing to run
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

(defstruct (col (:include thing))
  (txt "") ; column name
  (at 0)   ; column position
  (n 0)    ; number of summarizes itemd
  (w 1)    ; weight
  )

(defmethod add ((c col) (x cons)) (dolist (y x c) (add c y)))

; Unless we are skipping  stuff, increment `n`.
(defmethod add ((c col) x)
  (unless (eq #\? x) 
    (incf (? c n)) 
    (add1 c x))
  x)

; columns we are going to count
(defstruct (skip (:include col)))
(defmethod add1  ((s skip) x) x)

; columns where we count symbols (and track the mode)
(defstruct (sym (:include col))  
  seen mode (most 0))

(defmethod add1 ((s sym) x)
  (let ((n (inca x (? s seen))))
    (when (> n (? s most))
      (setf (? s most) n
            (? s mode) x))))

(defstruct (num (:include col))
  (_all (make-array 32 :fill-pointer 0 :adjustable t))
  sorted)

(defmethod add1 ((n num) (x string)) (add1 n (read-from-string x)))
(defmethod add1 ((n num) (x number))
  (vector-push-extend x (? n _all))
  (setf (? n sorted) nil))

(defstruct (row (:include thing))
  _rows              ; pointer to "rows" holding this
  cells ) ; values in this row 

(defstruct (cols (:include thing))
   names           ; all the row1 names
   all
   x
   y
   klass)          ; the klass column (if it exists)

(defstruct (sample (:include thing))
  (txt "")            ; text description of source
  rows                ; list of "row"
  (cols (make-cols))) ; column information

(defun columns (txt at sample)
  (let* ((what (cond ((find #\? txt) 'skip)
                     ((upper-case-p (char txt 0) 'num))
                     (t 'sym))) 
         (it (make-instance what :txt txt :at at 
                            :w (if (find #\- txt) -1 1))))
    (push (? sample cols all) it)
    (unless (find #\? txt)
      (cond ((find #\- txt) (push (? sample cols y) it))
            ((find #\+ txt) (push (? sample cols y) it))
            (t              (push (? sample cols x) it))))))

(defmethod slurp ((s sample) file)
  (csv 
;------------------;-------------------;-------------------;-------------------;
; macros
(defmacro want (x &rest y) `(assert ,x () ,@y))

(defmacro ? (s x &rest xs) 
  (if xs `(? (slot-value ,s ',x) ,@xs) `(slot-value ,s ',x)))

;------------------;-------------------;-------------------;-------------------;
; csv reading
(defun num? (s)
  (if (eql "?" s)
    s
    (let ((n (read-from-string s)))
      (if (numberp n) n s))))

(defun s2cells (s &optional (x 0) (y (position #\, s :start (1+ x))))
  (cons (num? (subseq s x y))
        (and y (s2cells s (1+ y)))))

(defun csv (file fn)
  (with-open-file (str file)
    (loop (funcall fn (s2cells (or (read-line str nil) (return-from csv)))))))

(defmacro with-csv ((lst file &optional out) &body body)
   `(progn (csv ,file #'(lambda (,lst) ,@body)) ,out))



