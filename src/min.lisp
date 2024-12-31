#+sbcl
(declaim (sb-ext:muffle-conditions cl:style-warning))

(set-macro-character #\$ #'(lambda (s _) `(slot-value self ',(read s t nil t))))

(defun main() (print (auto93)))

;-------------------------------------------------------------------------------
(defstruct col (n 0) (col 0) (txt ""))

(defstruct (sym (:include col))
   has   (most 0) mode klass)

(defstruct (num (:include col) (:constructor %make-num))
  (mu 0) (m2 0) (sd 0) (lo 1E32) (hi -1E32) (goal 1))

(defun make-num (&key (txt "") (col 0))
  (%make-num :txt txt :col col :goal (if (eql #\- (chr txt -1)) 0 1)))

(defstruct (cols (:constructor %make-cols)) all x y names)

(defun make-cols (names &aux (pos 0) (self (%make-cols :names names)))
  (dolist (name names self)
    (let ((a    (chr name 0))
          (z    (chr name -1))
          (what (if (upper-case-p a) %make-num %make-sym))
          (col  (funcall what :txt name :pos (incf pos))))
      (push col $all)
      (unless (eql z #\M)
        (if (eql z \#!) (setf $klass col))
        (if (member z '(#\! #\< #\>)) (push $y col) (push $x col))))))

(defstruct data rows cols)

;-------------------------------------------------------------------------------
(defmethod add ((self cols) row)
  (mapcar #'add $all row)
  row)

(defmethod add ((self num) x)
  (unless (eql x '?)
    (incf $n)
    (add1 self x)))

(defmethod add1 ((self num) x)
  (let ((d (- x $mu)))
    (incf $mu (/ d $n))
    (incf $m2 (* d (- x $mu)))
    (setf $sd (if (< $m2 2) 0 (sqrt (/ $m2 (- $n 1))))
          $lo (min x $lo)
          $hi (max x $hi))))
                
(defmethod add1 ((self sym) x)
  (let ((new (inca $has x)))
    (add $cols(if (> new $most)
                  (setf $mode x
                        $most new)))))

(defmethod add ((sef data) row)
  (push $rows (add $cols row)))

(defmethod adds ((self data) src)
  (if (stringp src)
      (with-csv s (lambda (r) (add self r)))
      (dolist (y x) (add self y)))
  self)

;-------------------------------------------------------------------------------
(defun inca (a x)
  (incf (cdr (or (assoc x a :test #'equal)
                 (car (setf $count (cons (cons x 0) a)))))))

(defmethod chr ((s symbol) n )
  (chr (symbol-name s) n))

(defmethod chr ((s string) n &aux (m (length s)))
  (if (< n m)
      (char s (if (>= n 0) n (+ m n)))))

i(defun s->thing (s &aux (s1 (string-trim '(#\Space #\Tab) s)))
  "Coerce `s` to an atomic thing."
  (let ((it (let ((*read-eval* nil)) (read-from-string s1 ""))))
    (cond ((numberp it)     it)
          ((eq it t)        t)
          ((eq it nil)      nil)
          ((string= it "?") '?)
          (t                s1))))

i(defun s->things (s &optional (sep #\,) (here 0)) ; --> list
  "split string to items, divided on `sep; then coerce each item"
  (let ((there (position sep s :start here)))
    (cons (s->thing (subseq s here there))
          (if there (s->things s sep (1+ there))))))

(defun with-csv (&optional file (fun #'print) end)
  "call `fun` on all lines in `file`, after running lines through `filter`"
  (with-open-file (s (or file *standard-input*))
    (loop (funcall fun (s->things (or (read-line s nil)
                                      (return end)))))))
