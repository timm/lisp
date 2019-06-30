;; vim: ts=2 sw=2 sts=2  et :
;--------- --------- --------- --------- --------- ---------
(unless (fboundp 'got) (load "../got"))

(got "oo/" "lib/macros.lisp" "sample/col.lisp")

;-------- -------- -------- -------- -------- --------
(defun skip?  (x) (col x #\?))
(defun use?   (x) (not (skip? x)))
(defun more?  (x) (col x #\>))
(defun less?  (x) (col x #\<))
(defun klass? (x) (col x #\!))
(defun goal?  (x) (or (klass? x) (less? x) (more? x)))
(defun col (x y) 
  (eql (char (symbol-name x) 0) y))

;-------- -------- -------- -------- -------- --------
(defthing eg keeper (_egs) (cells) (dom 0))

(defmethod cell ((e eg) col)
  (aref (? e 'cells) (? col 'pos)))
   
(defkept klass ((e eg))
  (aref
    (? e 'cells)
    (? (klass  (? e '_egs)) 'pos)))

;--------- --------- --------- --------- --------- ---------
(defthing sample keeper (name) (cols) (egs))

(defkept mores  ((e sample)) (names e #'more?))
(defkept lesss  ((e sample)) (names e #'less?))
(defkept klasss ((e sample)) (names e #'klass?))
(defkept goals  ((e sample)) (names e #'goal?))
(defkept nums   ((e sample)) (loop for c in (? e 'cols)
                                when #'nump collect c))
(defkept syms   ((e sample)) (loop for c in (? e 'cols) 
                                when #'symp collect c))

(defmethod klass ((e sample)) (car (klasss e)))

(defmethod names ((e sample) fn)
  (remove-if-not 
    #'(lambda (x) (funcall fn (? x 'name))) (? e 'cols)))

;--------- --------- --------- --------- --------- ---------
(defthing holder keeper (has) (name) (pos))

(defmethod add ((h holder) x &key (filter #'identity))
  (with-slots (has) h
    (when (and x (not (eql '? x)))
      (setf has 
            (or has (col+ x)))
      (add has x :filter filter)))
  x)

(defmethod col+ ((h holder) x)
	(make-instance 
		(if (numberp x) 'num 'sym)
		:w (if (and (numberp x) (less? x)) -1 1)))

(defmethod nump ((h holder)) (typep (? h 'has) 'num))
(defmethod symp ((h holder)) (typep (? h 'has) 'sym))

;--------- --------- --------- --------- --------- ---------
(defun data (&key name header rows)
  "Build table for name, col, egs"
  (let ((n -1) todo
        (out (make-instance 'sample :name name)))
    (with-slots (cols egs) out
      (labels 
        ((good? (vals)
                (assert (= (length header) (length vals)) 
                   (vals) "not of size ~a" (length header))
                t)
         (pos+ (txt)
               (if (use? txt) (holder+ (incf n) txt)))
         (eg+ (vals) 
              (make-instance 'eg :_egs out :cells (l->a vals)))
         (holder+ (pos txt)
             (let ((h (make-instance 'holder :pos pos :name txt)))
               (push h cols)
               h))
         (data1 (cols vals)
                (if cols
                  (if (car cols)
                    (cons (add   (car cols) (car vals)) 
                          (data1 (cdr cols) (cdr vals)))
                    (data1 (cdr cols) (cdr vals))))))
        ;--------- --------- --------- --------- ---------
        (setf todo (mapcar #'pos+ header)
              cols (reverse cols))
        (dolist (vals rows out)
          (when (good? vals)
            (push (eg+ (data1 todo vals)) 
                  egs)))))))
