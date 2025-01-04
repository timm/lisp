;&nbsp;<p>
;<img align=right width=200
;src="https://chiselapp.com/user/MistressRemilia/repository/CL-MeltySynth/uv/lisp-logo.png">
;<h1><font size=20pt><b>AI for busy people</b></font></h1>
;The more minimal the art, the<br>more maximum the explanation. <br>
;-- Hilton Kramer

(defstruct about
  "Struct for general info."
  (what  "k.lisp")
  (why   "instant AI tricks")
  (when  "(c) 2024")
  (how   "MIT license")
  (who   "Tim Menzies")
  (where "timm@ieee.org"))

(defstruct settings
  "Struct for all settings."
  (seed    1234567891)
  (buckets 2)
  (pp      2)
  (train   "../../moot/optimize/misc/auto93.csv")
  (about  (make-about)))

; [TOC]   
;## Set up
(defvar *settings* (make-settings))

(defun args()
  "Access command line."
  (cdr #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*))
   
#+sbcl (declaim (sb-ext:muffle-conditions cl:style-warning))

;### Macros
(defmacro o (x f &rest fs)
  "Nested access to slots."
  (if fs
      `(o (slot-value ,x ',f) . ,fs)
      `(slot-value ,x ',f)))

(defmacro ? (&rest slots)
  "Access settings."
  `(o *settings* . ,slots))

(defmacro has (lst x)
  "Return `lst`'s  slot value for `x` (if missing, initialize x's slot to 0)."
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x 0) ,lst))))))

(set-macro-character #\$ 
   #'(lambda (s _)
       "Expand $x to (slot-value self 'x)."
       `(slot-value self ',(read s t nil t))))

;## Structs
;### Data (has _rows_ and _cols_)
(defstruct (data (:constructor %make-data))
  "stores rows, summarized in cols (columns)"
  cols
  (rows (make-array 0 :fill-pointer 0 :adjustable t)))

(defmethod make-data (src &key sortp &aux (self (%make-data)))
  "Load in csv rows, or rows from a list into a `data`."
  (if (stringp src)
      (with-csv src #'(lambda (x) (add self (coerce x 'vector))))
      (dolist (x src) (add self x)))
  (if sortp
      (setf $rows (sort $rows #'< :key (lambda (row) (ydist self row)))))
  self)

;### Col (has _pos_  and _txt_ name)
(defstruct col
  "Columns have a `txt` name, a `pos` and count `n` of things seen."
  (n 0) (pos 0) (txt ""))

;#### Sym (isa _Col_, has symbol _counts_)
(defstruct (sym (:include col))
  "`Sym`s summarize symbolic columns."
  counts (most 0) mode klass)

;#### Num (isa _Col_, has _mu_, _sd_, _lo_ and _hi_ bounds)
(defstruct (num (:include col) (:constructor %make-num))
  "`Num`s summarize numeric columns."
  (mu 0) (m2 0) (sd 0) (lo 1E32) (hi -1E32) (goal 1))

(defun make-num (&key (txt "") (pos 0))
  "Constructor. For `nums`."

  (%make-num :txt txt :pos pos :goal (if (eql #\- (chr txt -1)) 0 1)))

;### Cols (factory for making _Cols_ from list of _names_)
(defstruct (cols (:constructor %make-cols))
  "Container for all the columns (store in `all`, some also stored in `x,y`." 
  all x y names klass)

(defun make-cols (names &aux (pos -1) x y klass all)
  "Constructor. `Names` tells us what `nums` and `syms` to make."
  (loop :for name :across names :do
    (let* ((a    (chr name 0))
           (z    (chr name -1))
           (what (if (upper-case-p a) #'make-num #'make-sym))
           (col  (funcall what :txt name :pos (incf pos))))
      (push col all)
      (unless (eql z #\X)
        (if (eql z #\!) (setf klass col))
        (if (member z '(#\! #\- #\+)) (push col y) (push col x)))))
  (%make-cols :x x :y y :klass klass :names names :all (reverse all)))

;## Methods
;### Update
(defmethod add ((self data) row)
  "Keep the row, update the `cols` summaries."
  (if $cols
      (vector-push-extend (add $cols row) $rows)
      (setf $cols (make-cols row))))

(defmethod add ((self cols) row)
  (mapcar (lambda (col) (add col (at col row))) $all)
  row)

(defmethod add ((self col) x)
  "For non-empty cells, add `x`. Always return `x`."
  (unless (eql x '?)
    (incf $n)
    (add1 self x))
  x)

(defmethod add1 ((self num) x)
  "Update numeric summaries with `x`."
  (let ((d (- x $mu)))
    (incf $mu (/ d $n))
    (incf $m2 (* d (- x $mu)))
    (setf $sd (if (< $m2 2) 0 (sqrt (/ $m2 (- $n 1))))
          $lo (min x $lo)
          $hi (max x $hi))))

(defmethod add1 ((self sym) x)
  "Update symbolic summaries with `x`."
  (let ((new (incf (has $counts x))))
    (if (> new $most)
      (setf $mode x
            $most new))))

;### Query
(defmethod at ((self col) row)
  "Access a column in a row."
  (elt row $pos))

(defmethod norm ((self num) x)
  "Normalizes x 0..1."
  (if (eql x '?) x (/ (- x $lo) (- $hi $lo 1E-32))))

(defmethod ydist ((self data) row)
  (minkowski
   (o $cols y)
   (lambda (col) (- (o col goal) (norm col (at col row))))))

(defmethod xdist ((self data) row1 row2)
  (minkowski
   (o $cols x) 
   (lambda (col) (dist col (norm col (at col row1)) (norm col (at col row2))))))

(defmethod kprune ((self data) k &keys (samples 32))
  (let ((out `(,(any $rows))))
    (dotimes (k1 (1- k) out)
      (push (_kprune self out (many $rows samples))
            out))))

(defun _kprune (data out rows)
  (let ((n (make-num)))
    (mapcar #'(lambda (r1)
                (first (sort out #'< :key (lambda (r2)
                                            `(, (add n (expt (xdist data r1 r2) 2)) r2))))))
                  
;## Functions
;### Numeric trics
(defun inca (a x)
  "Ensure `a` has a key for `x`, add one to that count."
  (incf (cdr (or (assoc x a :test #'equal)
                 (car (setf a (cons (cons x 0) a)))))))

(defmethod minkowski (lst fun)
  "p-th root of normalized sum of absolute values in `lst`, raised to p."
  (expt (/ (loop :for x :in lst :sum (expt (abs (funcall fun x)) (? pp)))
           (length lst))
        (/ 1 (? pp))))

;### Lisp Tricks
;#### Random
(defun pick (seq f &aux (all 0))
  (labels ((pairs (x &aux (fx (funcall x)))
             (incf all fx)
             `(,fx ,x )))
    (let* ((seq (sort (mapcar seq #'pairs) #'> :key #'car))
           (r (randf all)))
      (or (loop :for (fx x) :in seq :if (<= (decf r fx) 0) :return x)
          (car seq)))))
                                          
(defun any (seq)
  "return a random item from seq"
  (elt seq (floor (randf (length seq)))))

(defun many (seq &optional (n (length seq)) &aux out)
  (dotimes (_ n out) (push (any seq) out)))

(defmethod nshuffle ((seq cons))
  "shuffling a list is slow, so first coerce to a vector"
  (coerce (nshuffle (coerce seq 'vector))  'cons))

(defmethod nshuffle ((seq vector))
  "shuffle a vector"
  (loop :for i from (length seq) :downto 2
        :do (rotatef (elt seq (random i)) (elt seq (1- i))))
  seq)

(defvar *seed* (? seed))
(defun randi (&optional (n 1))
  "return a random integer"
  (floor (* n (/ (randf 1000.0) 1000))))

(defun randf (&optional (n 1.0))
  "return a random float"
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

;### String tricks
(defun chr (s n )
  "Return nth character from `s`. Negative `n` denote indexes from back." 
  (let ((s (if (stringp s) s (symbol-name s))))
    (char s (if (>= n 0) n (+ n (length s))))))         

(defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s)))
  "Coerce `s` to an atomic thing."
  (let* ((*read-eval* nil)
         (it (read-from-string s1 "")))
    (cond ((numberp it)     it)
          ((string= it "?") '?)
          (t                s1))))

(defun things (s &optional (sep #\,) (here 0)) ; --> list
  "split string to items, divided on `sep; then coerce each item"
  (let ((there (position sep s :start here)))
    (cons (thing (subseq s here there))
          (if there (things s sep (1+ there))))))

;### File I/O
(defun with-csv (&optional file (fun #'print) end)
  "call `fun` on all lines in `file`, after running lines through `filter`"
  (with-open-file (s (or file *standard-input*))
    (loop (funcall fun (things (or (read-line s nil)
                                      (return end)))))))

;##  Start-up
;### Actions
  
(defun eg-s(s)
  (setf *seed* (setf (? seed) s)))

(defun eg--settings (&optional _)
  (print *settings*))

(defun eg--csv (&optional _ &aux (pos -1))
  (with-csv (? train)  (lambda (r)
    (if (zerop (mod (incf pos) 30))  (print r)))))

(defun eg--data (&optional _ &aux (pos -1))
  "CLI action. Process data."
  (let ((self (make-data (? train) :sortp t)))
    (format t "d ~a~%" (o self cols names))
    (loop :for row :across $rows :do
      (when (zerop (mod (incf pos) 30))
         (format t "~,2f ~a~%" (ydist self row) row)))))

;### Start-up Control
(loop :for (flag arg) :on (args) :by #'cdr
      :do  (let ((com (intern (format nil "EG~:@(~a~)" flag))))
              (if (fboundp com)
                 (funcall com (if arg (thing arg))))))

; That's all folks.
