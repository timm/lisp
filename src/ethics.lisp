; vim: ts=2 sw=2 sts=2  et :
(defmacro doarray ((one n arr &optional out) &body body )
   `(dotimes (,n (length ,arr) ,out)
       (let ((,one (aref arr ,n)))
         ,@body)))

(defmacro dohash ((key value  h &optional out) &body body )
  `(progn (maphash #'(lambda (,key ,value) ,@body) ,h) ,out))

(defstruct sym
  (count (make-hash-table))
  (n 0)
  most
  mode)

(defmethod add ((s sym) x)
  (with-slots (most mode counts  n) s
    (let*
      ((new (incf (gethash x counts 0))))
      (if (> new most)
        (setf most new
              mode x)))))

(defmethod ent ((s sym))
  (with-slots (counts n) s
    (let ((e 0))
      (do-hash (k v counts e)
        (let ((p (/ v n)))
          (decf e (* p (log p 2))))))))

(defstruct num 
  (n 0)
  (mu 0.0) (m2 0.0) (sd 0.0)
  (lo most-positive-fixnum)
  (hi most-negative-fixnum))

(defmethod add ((nu num) x)
  (with-slots (n all lo hi mu m2 sd) nu
    (let ((delta (- x mu)))
      (setf lo (min lo x)
            hi (max hi x)
            mu (+ mu (/ delta n))
            m2 (+ m2 (* delta (- x mu))))
      (sd-prim nu))))

(defmethod sd-prim ((nu num))
  (with-slots (sd n m2) nu
    (setf sd (cond ((< n 2)  0)
                   ((< m2 0) 0)
                   (t  (sqrt (/ m2 (- n 1))))))))

(defmethod norm ((n num) x)
  (with-slots (lo hi) n
    (/ (- x lo) (+ (- hi lo) (/ 1 most-positive-fixnum)))))

(defstruct col (w 1) (pos 0) (txt ""))
(defstruct cols all klass goals names indep nums syms)
(defstruct row cells poles)
(defstruct tbl rows (cols (make-cols)))

(defun skipp(x) (?? x #\?))
(defun goalp(x)  (or (?? x #\!) (?? x #\<) (?? x #\>)))
(defun nump(x)   (or (?? x #\$) (?? x #\<) (?? x #\>)))
(defmethod ??  (x y) (find y x :test #'equal))

(defmethod add ((c cols) arr)
	(with-slots (indep klass all  nums syms) c
		(do-array (x pos arr)
			 (unless (skipo x)
				 (push x names)
				 (if (?? x #\!) (setq klass pos))
				 (let* ((w     (if (? x #\<) -1 1))
								(todo  (if (num? x) #'make-num #'make-sym))
								(col   (funcall todo :pos pos :txt x :w w)))
					 (push col all)
					 (push col (if (nump  x) nums  syms))
					 (push col (if (goalp x) goals indep)))))))

(defmethod add ((tbl tbl) a)
  (if (
