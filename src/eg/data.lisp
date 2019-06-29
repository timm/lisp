;; vim: ts=2 sw=2 sts=2  et :
;-------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(got "oo/" "eg/col.lisp")

(defthing table 
  keeper (name) (nums) (syms) (cols) (rows))

(defmethod skip?  ((tb table) x) (col tb x #\?))
(defmethod more?  ((tb table) x) (col tb x #\>))
(defmethod less?  ((tb table) x) (col tb x #\<))
(defmethod klass? ((tb table) x) (col tb x #\!))
(defmethod goal?  ((tb table) x) 
  (or (klass? tb x) (less? tb x) (more? tb x)))

(defkeep mores    ((tb table)) (cols tb #'more?))
(defkeep lesss    ((tb table)) (cols tb #'less?))
(defkeep klasss   ((tb table)) (cols tb #'klass?))
(defkeep goals    ((tb table)) (cols tb #'goal?))

(defmethod klass  ((tb table)) (car (klasss tb)))

(defmethod col ((tb table) x) 
  (eql (char (symbol-name x) 0) y))

(defmethod cols ((tb table) fn)
  (remove-if-not 
    #'(lambda (x) (funcall fn (?  x 'name)))
    (? tb 'cols))))

;-------- -------- -------- -------- -------- --------
(defthing row keeper (_table) (cells))

(defmethod cell ((r row) col)
  (aref (? r cells) (? col pos)))
   
(defthing klass ((r row))
  (aref
    (? r cells)
    (? (klass  (? r table)) 'pos)))

(defmethod! klassRange! ((r row))
  (range
    (klassCol (? r table))
    (klassValue r)))

(defun data (&key name cols egs
             &aux (tab 
                    (make-instance 'table :name name)))
  "Build table for name, col, egs"
  (labels 
    ((okCol? (txt)
            (not (skip? txt)))
     (okRow? (row) 
            (assert (eql (length row) (length (? tab cols)))
                    (row) "wrong length ~a" row)
            t)
     (col+ (txt pos)
           (make-instance 
              (if (numeric? txt) 'num 'sym )
              :name txt :pos pos :_table tab))
     (row+ (cells)
           (let ((row (make-instance 'row
                         :_table tab :cells (l->a cells))))
             (dolist (col (? tab cols) row)
               (add col (cell row col))))))
    ;; now we can begin
    (doitems (txt pos cols)
      (if (okCol? txt)
        (push (col+ txt pos) 
              (? tab cols))))
    (dolist (eg egs tab)
      (if (okRow? eg) 
        (push (row+ eg) 
              (? tab rows))))))
