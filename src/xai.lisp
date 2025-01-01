;<font size=20pt><b>AI for busy people</b></font><br>
;**Tim Menzies**<br><timm@ieee.org><br>http://timm.github.io

; Understanding turns insight into action.
; <img align=right width=300  style="margin-top:40px;"
; src="https://www.uklinkology.co.uk/wp-content/uploads/2022/02/Success-Stories.png">|#

; [TOC]

; <br clear=all>
;## SBCL Hacks

    #+sbcl (declaim (sb-ext:muffle-conditions cl:style-warning))

(defun args()
  "Access command line."
  (cdr #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*))

; From here down, it should all be standard LISP.
;## Globals
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

;## Set up
(defvar *settings* (make-settings))

;### Macros
(defmacro o (x f &rest fs)
  "Nested access to slots."
  (if fs
      `(o (slot-value ,x ',f) . ,fs)
      `(slot-value ,x ',f)))

(defmacro ? (&rest slots)
  "Access settings."
  `(o *settings* . ,slots))

(set-macro-character #\$ 
   #'(lambda (s _)
       "Expand $x to (slot-value self 'x)."
       `(slot-value self ',(read s t nil t))))

;## Structs
(defstruct (data (:constructor %make-data))
  "stores rows, summarized in cols (columns)"
  rows cols)

(defmethod make-data (src &key sortp &aux (self (%make-data)))
  "Load in csv rows, or rows from a list into a `data`."
  (if (stringp src)
      (with-csv src (lambda (x) (add self x)))
      (dolist (x src) (add self x)))
  (if sortp
      (setf $rows (sort $rows #'< :key (lambda (r) (ydist self r)))))
  self)

(defstruct col
  "Columns have a `txt` name, a `pos` and count `n` of things seen."
  (n 0) (pos 0) (txt ""))

(defstruct (sym (:include col))
  "`Sym`s summarize symbolic columns."
  has (most 0) mode klass)

(defstruct (num (:include col) (:constructor %make-num))
  "`Num`s summarize numeric columns."
  (mu 0) (m2 0) (sd 0) (lo 1E32) (hi -1E32) (goal 1))

(defun make-num (&key (txt "") (pos 0))
  "Constructor. For `nums`."
  (%make-num :txt txt :pos pos :goal (if (eql #\- (chr txt -1)) 0 1)))

(defstruct (cols (:constructor %make-cols))
  "`Cols` have `names` and `all` the cols and some cols stored in `x` and `y`."
  all x y names)

(defun make-cols (names &aux (self (%make-cols :names names)))
  "Constructor. `Names` tells us what `nums` and `syms` to make."
  (dolist (name names self)
    (let* ((a   (chr name 0))
          (z    (chr name -1))
          (what (if (upper-case-p a) #'%make-num #'%make-sym))
          (col  (funcall what :txt name :pos (length $all))))
      (push col $all)
      (unless (eql z #\X)
        (if (eql z #\!) (setf $klass col))
        (if (member z '(#\! #\- #\+)) (push col $y) (push col $x))))))

;## Update
(defmethod add ((self data) row)
  "Keep the row, update the `cols` summaries."
  (push $rows (add $cols row)))

(defmethod add ((self cols) row)
  (mapcar #'add $all row))

(defmethod add ((self num) x)
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
  (let ((new (inca $has x)))
    (add $cols(if (> new $most)
                  (setf $mode x
                        $most new)))))

;## Query
(defun cell (col row)
  "Access a column in a row."
  (elt row (o col pos)))

(defmethod norm ((self num) x)
  "Normalizes x 0..1."
  (if (eql '? x) x (/ (- x $lo) (- $hi $lo + 1E-32))))

(defmethod ydist ((self data) row)
  (let* ((ys (o self cols y))
         (d (loop :for col :in ys
                  :sum (expt (abs (- (o col goal) (norm self (cell col row))))
                             (? pp)))))
    (expt (/ d (length ys)) (/ 1 (? pp)))))

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

;## Start-up Actions
(defun eg--data (&optional file)
  "CLI action. Process data."
    (print (or file (? train)))
  (let ((data (make-data (or file (? train)))))
    (dolist (col (o data cols y))
      (format t "~a~%" col))))

;## Start-up
(loop :for (flag arg) :on (args) :by #'cdr
      :do  (let ((com (intern (format nil "EG~:@(~a~)" flag))))
             (if (fboundp com)
                 (funcall com (if arg (thing arg))))))

; That's all folks.
