; vim: ts=2 sw=2 sts=2  et :

;---------.---------.---------.---------.--------.---------.----------
; hash defines
(defun stop ()
  #+sbcl (sb-ext:exit)
  #+:clisp (ext:exit))

(defun cli ()
  #+clisp ext:*args*
  #+sbcl sb-ext:*posix-argv*)

(defun the-slots (it)
  #+clisp (class-slots (class-of it))
  #+sbcl (sb-mop:class-slots (class-of it)))

;---------.---------.---------.---------.--------.---------.----------
; macros
(defmacro af (test then &optional else)
  `(let ((a ,test)) (if a ,then ,else)))

(defmacro doarray ((one n arr &optional out) &body body )
   `(dotimes (,n (length ,arr) ,out)
       (let ((,one (aref arr ,n)))
         ,@body)))

(defmacro dohash ((key value  h &optional out) &body body )
  `(progn (maphash #'(lambda (,key ,value) ,@body) ,h) ,out))

(defmacro ? (obj first-slot &rest more-slots)
 	(if (null more-slots)
   		`(slot-value ,obj ,first-slot)
   		`(? (slot-value ,obj ,first-slot) ,@more-slots)))

(defmacro ?? (&rest x) (? *O* ,@x))

;---------.---------.---------.---------.--------.---------.----------
(defstruct ch-options 
	(meta  #\:)
	(less  #\<)
	(more  #\>)
	(klass #\!)
	(skip  #\?)
	(sep   #\,))

(defstruct lsh-options
	(poles 10)
  (trim  0.95))

(defstruct options 
	(lsh  (make-lsh-options))
	(ch   (make-ch-options)))

(defun cli2options (thing)
	(dolist (x (cli) thing)
     (aif (find (format nil ":~a" x) (the-slots things))
       (setf (slot-value thing x) 
             (read-from-string (nth (+ 1 a ) lst))))))

(defvar *O* (make-options))

;---------.---------.---------.---------.--------.---------.----------
; tricks 
(defun has (x &rest lst) 
  (dolist (y lst)
     (if (find ,y ,x :test #'equal) (return t))))

;---------.---------.---------.---------.--------.---------.----------
; symbols
(defstruct sym
  (count (make-hash-table))
  (n 0)
  (ent 0)
  most
  mode)

(defmethod var ((s sym) x) (ent x))
(defmethod mid ((s sym) x) (sym-mode x))
(defmethod prep ((s sym) x) x)

(defmethod add ((s sym) x)
  (with-slots (most mode counts  n) s
    (let* ((new (incf (gethash x counts 0))))
			(if (> new most)
				(setf most new
              mode x)))))

(defmethod ent ((s sym) &aux (e 0))
  (with-slots (counts n) s
    (dohash (k v counts e)
      (let ((p (/ v n)))
        (decf e (* p (log p 2)))))))

;---------.---------.---------.---------.--------.---------.----------
; numbers
(defstruct num 
  (n 0)
  (mu 0.0) (m2 0.0) (sd 0.0)
  (lo most-positive-fixnum)
  (hi most-negative-fixnum))

(defmethod var ((s sym) x) (num-sd x))
(defmethod mid ((s sym) x) (num-mu x))
(defmethod prep ((s sym) x) (if (numberp x) x (read-from-string x)))

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

;---------.---------.---------.---------.--------.---------.----------
; tables of data
(defstruct col (w 1) (pos 0) (txt ""))
(defstruct cols all klass goals names indep nums syms meta)
(defstruct row cells poles)
(defstruct tbl rows (cols (make-cols)))

(defmethod add ((c cols) arr)
	(labels 
		((goalp(x) (has x (?? ch klass) (?? ch less) (?? ch more)))
		 (nump(x)  (has x (?? ch num)   (?? ch less) (?? ch more))))
		(with-slots (indep klass all meta nums syms) c
			(doarray (x pos arr)
				(unless (has x (?? ch skip))
					(push x names)
					(if (has x (?? ch klass)) (setq klass pos))
					(let* ((w     (if (has x (?? ch less)) -1 1))
								 (todo  (if (nump x) #'make-num #'make-sym))
								 (col   (funcall todo :pos pos :txt x :w w)))
						(push col all)
						(push col (if (nump  x) nums  syms))
						(if (has x (?? ch meta))
							(push col meta)
							(push col (if (goalp x) goals indep)))))))))

