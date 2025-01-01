;&nbsp;<h1><font size=20pt><b>AI for busy people</b></font></h1>
; Understanding turns insight into action.

(defstruct about
  "Struct for file meta info."
  (what  "min.lisp")
  (why   "optimization tricks")
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
;### Data
(defstruct (data (:constructor %make-data))
  "stores rows, summarized in cols (columns)"
  rows cols)

(defmethod make-data (src &key sortp &aux (self (%make-data)))
  "Load in csv rows, or rows from a list into a `data`."
  (if (stringp src)
      (with-csv src #'(lambda (x) (add self x)))
      (dolist (x src) (add self x)))
  (if sortp
      (setf $rows (sort $rows #'< :key (lambda (row) (ydist self row)))))
  self)

;### Col
(defstruct col
  "Columns have a `txt` name, a `pos` and count `n` of things seen."
  (n 0) (pos 0) (txt ""))

;### Sym
(defstruct (sym (:include col))
  "`Sym`s summarize symbolic columns."
  seen (most 0) mode klass)

;### Num
(defstruct (num (:include col) (:constructor %make-num))
  "`Num`s summarize numeric columns."
  (mu 0) (m2 0) (sd 0) (lo 1E32) (hi -1E32) (goal 1))

(defun make-num (&key (txt "") (pos 0))
  "Constructor. For `nums`."
  (%make-num :txt txt :pos pos :goal (if (eql #\- (chr txt -1)) 0 1)))

;### Cols
(defstruct (cols (:constructor %make-cols))
  "Container for all the columns (store in `all`, some also stored in `x,y`." 
  all x y names klass)

(defun make-cols (names &aux (pos -1) x y klass all)
  "Constructor. `Names` tells us what `nums` and `syms` to make."
  (dolist (name names (%make-cols :x x :y y :klass klass :names names
                                  :all (reverse all)))
    (let* ((a    (chr name 0))
           (z    (chr name -1))
           (what (if (upper-case-p a) #'make-num #'make-sym))
           (col  (funcall what :txt name :pos (incf pos))))
        (push col all)
      (unless (eql z #\X)
        (if (eql z #\!) (setf klass col))
        (if (member z '(#\! #\- #\+)) (push col y) (push col x))))))

;## Update
(defmethod add ((self data) row)
  "Keep the row, update the `cols` summaries."
  (if $cols
    (push (add $cols row) $rows)
    (setf $cols (make-cols row))))

(defmethod add ((self cols) row)
  (mapcar #'add $all row))

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
  (let ((new (incf (has $seen x))))
    (if (> new $most)
      (setf $mode x
            $most new))))

;## Query
(defun at (col row)
  "Access a column in a row."
  (elt row (o col pos)))

(defmethod norm ((self num) x)
  "Normalizes x 0..1."
  (if (eql x '?) x (/ (- x $lo) (- $hi $lo 1E-32))))

(defmethod ydist ((self data) row)
  (let ((d (loop :for c :in (o $cols y)
                 :sum (expt (abs (- (o c goal) (norm c (at c row)))) (? pp)))))
    (expt (/ d (length (o $cols y))) (/ 1 (? pp)))))

;## Utils
(defun inca (a x)
  "Ensure `a` has a key for `x`, add one to that count."
  (incf (cdr (or (assoc x a :test #'equal)
                 (car (setf a (cons (cons x 0) a)))))))

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

(defun with-csv (&optional file (fun #'print) end)
  "call `fun` on all lines in `file`, after running lines through `filter`"
  (with-open-file (s (or file *standard-input*))
    (loop (funcall fun (things (or (read-line s nil)
                                      (return end)))))))

;##  Start-up Actions
(defun eg--settings ()
  (print *settings*))

(defun eg--csv (&aux (pos -1))
  (with-csv (? train)  (lambda (r)
    (if (zerop (mod (incf pos) 30))  (print r)))))

(defun eg--data (&aux (pos -1))
  "CLI action. Process data."
  (let ((self (make-data (? train) :sortp t)))
    (dolist (row $rows)
       (when (zerop (mod (incf pos) 30))  
         (format t "~,2f ~a~%" (ydist self row) row)))))

;## Start-up
(loop :for (flag arg) :on (args) :by #'cdr
      :do  (let ((com (intern (format nil "EG~:@(~a~)" flag))))
              (if (fboundp com)
                 (funcall com))))

; That's all folks.
