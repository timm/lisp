;&nbsp;<p>
;<img align=right width=200
;src="https://chiselapp.com/user/MistressRemilia/repository/CL-MeltySynth/uv/lisp-logo.png">
;<h1><font size=20pt><b>AI for busy people</b></font></h1>
;The more minimal the art, the<br>more maximum the explanation. <br>
;-- Hilton Kramer

(defstruct abouts
  "Struct for general info."
  (what  "k.lisp")
  (why   "instant AI tricks")
  (when  "(c) 2024")
  (how   "MIT license")
  (who   "Tim Menzies")
  (where "timm@ieee.org"))

(defstruct bayes (k 1) (m 2))

(defstruct settings
  "Struct for all settings."
  (seed    1234567891)
  (buckets 2)
  (pp      2)
  (train   "../../moot/optimize/misc/auto93.csv")
  (bayes  (make-bayes))
  (about  (make-abouts)))

; [TOC]   
;## Set up
(defvar *settings* (make-settings))

(defun args()
  "Access command line."
  (cdr #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*))
   
#+sbcl (declaim (sb-ext:muffle-conditions cl:style-warning))

;### Macros
(defmacro aif (test yes &optional no)
  "Anaphoric if. Condition results available in `it`."
  `(let ((it ,test))
     (if it ,yes ,no)))

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
  (mu 0) (m2 0) (sd 0) (lo 1E32) (hi -1E32) (goal 1)
  rank meta ; used by stats
  )

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
(defmethod add ((self col) (seq sequence))
  "Run `add` for all items  in `seq`."
  (map nil (lambda (item) (add self item)) seq)
  seq)

(defmethod add ((self data) row)
  "Keep the row, update the `cols` summaries."
  (if $cols
      (vector-push-extend (add $cols row) $rows)
      (setf $cols (make-cols row))))

(defmethod add ((self cols) row)
  "Update `Cols.all` with `row`." 
  (mapcar (lambda (col) (add col (at col row))) $all)
  row)
  
(defmethod add ((self col) x &key (n 1))
  "For non-empty cells, add `x`. Always return `x`."
  (unless (eql x '?)
    (incf $n n)
    (add1 self x) n)
  x)

(defmethod add1 ((self num) x _)
  "Update numeric summaries with `x`."
  (let ((d (- x $mu)))
    (incf $mu (/ d $n))
    (incf $m2 (* d (- x $mu)))
    (setf $sd (if (< $m2 2) 0 (sqrt (/ $m2 (- $n 1))))
          $lo (min x $lo)
          $hi (max x $hi))))

(defmethod add1 ((self sym) x n)
  "Update symbolic summaries with `x`."
  (let ((new (incf (has $counts) n)))
    (if (> new $most)
      (setf $mode x
            $most new))))

;### Query
(defmethod at ((self col) row)
  "Access a column in a row."
  (elt row $pos))

(defmethod entropy ((self sym))
  "Return diversity of a sym's counts."
  (labels ((f (n) (* (/ n $n) (log (/ n $n) 2))))
    (loop :for (_ . n) :in $counts :sum (f n))))

(defmethod norm ((self num) x)
  "Normalizes x 0..1."
  (if (eql x '?) x (/ (- x $lo) (- $hi $lo 1E-32))))

(defmethod delta ((i num) (j num))
  "Return mean difference, normalized by sd."
  (let ((n1  (+ 1E-32 (o i n)))  (n2 (+ 1E-32 (o j n)))
        (mu1 (o i mu)) (mu2 (o j mu))
        (sd1 (o i sd)) (sd2 (o j sd)))
    (/ (abs (- mu1 mu2))
       (sqrt (+ (/ (* sd1 sd1) n1)
                (/ (* sd2 sd2) n2))))))

(defmethod merge ((i sym) (j sym) (eps 20))
  "Merge `i`,`j` if they are too small or if too complex."
  (let ((k (make-sym)))
    (dolist (it `(,i ,j))
      (loop :for (x . n) :in (o it counts) :do (add k x n)))
    (if (or (< (o i n) eps)
            (< (o j n) eps)
            (<= (ent k) (/ (+ (* (o i n) (ent i))
                              (* (o j n) (ent j)))
                           (o k n))))
        k)))

(defun merges (lst xtra &aux out pairs)
  "Run `merge` over all items in `lst`."
  (dolist (one lst)
    (if out
        (aif (merge one (car out) xtra)
             (setf (car out) it)
             (push one out))
        (setf out (list one)))
    (push `(,one ,(length out)) pairs))
  (loop :for (one n) :in pairs :do
    (setf (o one meta) (elt out n)
          (o one meta rank) (code-char (+ (char-code #\a) n))))
  out)

;#### Bayes
(defmethod like ((self sym) x prior)
  (/ (+ (or (cdr (assoc x $counts)) 0)
        (* (? bayes m) prior))
     (+ $n (? bayes m))))

(defmethod like ((self num) x _)
  "Calculate the Gaussian PDF for value xa."
  (/ (exp (- (/ (* (expt (- x $mu) 2))
                (* 2 $sd $sd))))
     (* #sd (sqrt (* 2 pi)))))

(defmethod like ((self data) row all n)                 
  (let* ((prior (/ (+ (length $rows) (? k)) (+ all (* (? k) n))))
         (out prior))
    (dolist (col (o $cols x) out)
      (let ((x (at col row)))
        (unless (eql x '?) 
          (setf out (* out (max 0 (min 1 (like col x prior))))))))))

(defconstant *aqure*
  '((xplor . (lambda (b r _) (/ (+ b r)
                                (abs (- b r -1E-32)))))
    (xploit . (lambda (b r _) (/ b
                                 (+ r 1E-32))))
    (adapt . (lambda (b r p &aux (q (- 1 p)))
               (/ (+ b (* q r))
                  (abs (- (* b q) r -1E-32)))))))

(defun guess ((self data) &key (train 0.33) (start 4)
                            (stop 25) (acq (cdar *acquire*)))
  (let (best rest done)
    (labels ((yes (r) (like best r (length done) 2))
             (no  (r) (like rest r (length done) 2))
             (maybe (r) (funcall acq (yes r) (no r) (/ (length done) stop))))
      (let
  
                    
;#### Dist
(defmethod ydist ((self data) row)
  "Over y columns, return distance to goals."
  (minkowski
   (o $cols y)
   (lambda (col) (- (o col goal) (norm col (at col row))))))

(defmethod xdist ((self data) row1 row2)
  "Over x columns, return distance between items."
  (minkowski
   (o $cols x) 
   (lambda (col) (dist col (norm col (at col row1)) (norm col (at col row2))))))

(defmethod kprune ((self data) &keys (k 25) (n 32) (rows (many $rows n)))
  "k-means++ centroid generaion."
  (let ((centroids `(,(any rows))))
    (dotimes (k1 (1- k) centroids)
      (push (_kprune self centroids rows) centroids))))

(defun _kprune (data centroids rows &aux pairs (all 0))
  "Returns one new k-means++ centroid."
  (dolist (row rows)
    (labels ((dist (r) (xdist data r row)))
      (let ((near (car (sort centroids #'< :key #'dist))))
        (incf all (car (push `(,(expt (dist near) 2) ,row) pairs))))))
  (setf all (randf all))
  (or (loop :for (d row) :in pairs :if (<= (decf all d) 0) :return row)
      (second (car pairs))))

;## Functions
;### Stats
(defun cliffs (xs ys &key (delta 0.197) &aux (n 1E-32) (lt 0) (gt 0))
  "Non-parametric effect size."
  (dolist (x xs) 
    (dolist (y ys (<= (/ (abs (- gt lt)) n) delta))
      (incf n)
      (if (> y z) (incf gt))
      (if (< y z) (incf lt)))))

(defun bootstrap (y0 z0 &keys (b 512) (conf 0.05))
  "Non-parametric significance test."
  (labels ((nums (lst) (add (make-num) lst)))
    (let* ((x    (nums (nums y0) z0))
           (y    (nums y0))
           (z    (nums z0))
           (obs  (delta y z))
           (yhat (mapcar (lambda (y1) (- y1 (o y mu) (o x mu))) y0))
           (zhat (mapcar (lambda (z1) (- z1 (o z mu) (o x mu))) z0))
           (n    0))      
      (dotimes (_ b (>= (/ n b) conf))
        (if (>= (delta (nums (many yhat)) (nums (many zhat))) obs)
            (incf n))))))

;### Numerics
(defun inca (a x)
  "Ensure `a` has a key for `x`, add one to that count."
  (incf (cdr (or (assoc x a :test #'equal)
                 (car (setf a (cons (cons x 0) a)))))))

(defmethod minkowski (lst fun)
  "p-th root of normalized sum of absolute values in `lst`, raised to p."
  (expt (/ (loop :for x :in lst :sum (expt (abs (funcall fun x)) (? pp)))
           (length lst))
        (/ 1 (? pp))))


;#### Randoms                                        
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

;### Strings
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

;### Files
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
(loop :for (flag arg) :on (args) :by #'cdr(< (o i n) eps)
      :do  (let ((com (intern (format nil "EG~:@(~a~)" flag))))
              (if (fboundp com)
                 (funcall com (if arg (thing arg))))))

; That's all folks.
