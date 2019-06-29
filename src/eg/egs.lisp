;; vim: ts=2 sw=2 sts=2  et :
;-------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(got "oo/" "eg/col.lisp")

(defthing egs 
  keeper (name) (nums) (syms) (cols) (rows))

(defmethod skip?  ((e egs) x) (col e x #\?))
(defmethod more?  ((e egs) x) (col e x #\>))
(defmethod less?  ((e egs) x) (col e x #\<))
(defmethod klass? ((e egs) x) (col e x #\!))
(defmethod goal?  ((e egs) x) 
  (or (klass? e x) (less? e x) (more? e x)))

(defkeep mores  ((e egs)) (cols e #'more?))
(defkeep lesss  ((e egs)) (cols e #'less?))
(defkeep klasss ((e egs)) (cols e #'klass?))
(defkeep goals  ((e egs)) (cols e #'goal?))

(defmethod klass ((e egs)) (car (klasss e)))

(defmethod col ((e egs) x) 
  (eql (char (symbol-name x) 0) y))

(defmethod cols ((e egs) fn)
  (remove-if-not 
    #'(lambda (x) (funcall fn (?  x 'name)))
    (? e 'cols))))

;-------- -------- -------- -------- -------- --------
(defthing row keeper (_table) (cells))

(defmethod cell ((r row) col)
  (aref (? r cells) (? col pos)))
   
(defthing klass ((r row))
  (aref
    (? r cells)
    (? (klass  (? r table)) 'pos)))

(defun data (&key name cols egs
             &aux (tab 
                   (make-instance 'table :name name)))
  "Build table for name, col, egs"
  (labels 
    ((okRow? (row) 
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
      (if (not (skip?  txt))
        (push (col+ txt pos) 
              (? tab cols))))
    (dolist (eg egs tab)
      (if (okRow? eg) 
        (push (row+ eg) 
              (? tab rows))))))
