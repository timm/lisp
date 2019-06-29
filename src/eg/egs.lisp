;; vim: ts=2 sw=2 sts=2  et :
;-------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(got "oo/" "lib/macros.lisp" "eg/col.lisp")

;-------- -------- -------- -------- -------- --------
(defun skip?  (x) (col x #\?))
(defun more?  (x) (col x #\>))
(defun less?  (x) (col x #\<))
(defun klass? (x) (col x #\!))
(defun goal?  (x) (or (klass? e x) (less? e x) (more? e x)))

(defun col (x y) 
  (eql (char (symbol-name x) 0) y))

;-------- -------- -------- -------- -------- --------
(defthing row keeper (egs) (cells))

(defmethod cell ((r row) col)
  (aref (? r cells) (? col pos)))
   
(defkeep klass ((r row))
  (aref
    (? r 'cells)
    (? (klass  (? r 'egs)) 'pos)))

;-------- -------- -------- -------- -------- --------
(defthing egs keeper (name) (nums) (syms) (cols) (rows))

(defkeep mores  ((e egs)) (cols e #'more?))
(defkeep lesss  ((e egs)) (cols e #'less?))
(defkeep klasss ((e egs)) (cols e #'klass?))
(defkeep goals  ((e egs)) (cols e #'goal?))

(defmethod klass ((e egs)) (car (klasss e)))

(defmethod cols ((e egs) fn)
  (remove-if-not 
    #'(lambda (x) (funcall fn (?  x 'name)))
    (? e 'cols)))

;-------- -------- -------- -------- -------- --------
(defun data (&key name cols egs
             &aux whats (width 0)
                  (out 
                    (make-instance 'table :name name)))
  "Build table for name, col, egs"
  (labels 
    ((okRow? (row) 
        (assert (eql width (length row)) 
                (row) "not of size ~a" width)
        t)
     (col+ (val what)
        (make-instance 
           (if (numberp val) 'num 'sym )
           :name (getf what :name) :pos (getf what :pos) :egs egs))
     (row+ (cells)
        (let ((row (make-instance 'row
                   :egs eg :cells (l->a cells))))
          (mapc (lambda (val what)
                   (when (not (skip? val)
                      (setf (getf what :has)
                            (or (getf what :has)
                                  (setf (getf what :has) 
                          (col+ val what)
XXX
                          (getf what :name) (getf what :ois)

                calls whats)
          olist (col (? egs cols) row)
            (add col (cell row col))))))
    ;------------------------------------
    
    (setf whats
      (doitems (txt pos cols (reverse what))
        (when (not (skip?  txt))
          (incf width)
          (push `(:name ,txt :pos ,pos :has nil) whats))))
    (dolist (eg egs tab)
      (if (okRow? eg) 
        (push (row+ eg) (? out 'rows))))))
