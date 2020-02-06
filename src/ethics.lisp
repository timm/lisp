#!/usr/bin/env clisp -q
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

(defmacro while (test &body body)
  `(do () ((not ,test)) ,@body))

(defmacro whale (test &body body )
  `(let (a)
     (while (setf a ,test) ,@body)))

(defmacro do-array ((one n arr &optional out) &body body )
   `(dotimes (,n (length ,arr) ,out)
       (let ((,one (aref arr ,n))) ,@body)))

(defmacro do-hash ((key value  h &optional out) &body body )
  `(progn (maphash #'(lambda (,key ,value) ,@body) ,h) ,out))

(defmacro ? (obj first-slot &rest more-slots)
 	(if (null more-slots)
   		`(slot-value ,obj ',first-slot)
   		`(? (slot-value ,obj ',first-slot) ,@more-slots)))

(defmacro ?? (&rest x) `(? *O* ,@x))

(defmacro do-read ((it &optional f out) &body body)
  (let ((str (gensym)))
    `(progn
       (if ,f
         (with-open-file (,str ,f)
           (while (setf ,it (read-line ,str nil))
             ,@body))
          (while (setf ,it (read-line *standard-input* nil))
            ,@body))
       ,out)))

;---------.---------.---------.---------.--------.---------.----------
(defstruct ch!
	(meta  #\%)
	(less  #\<)
	(num   #\$)
	(more  #\>)
	(klass #\!)
	(skip  #\?)
	(sep   #\,))

(defstruct lsh!
	(poles 10)
  (trim  0.95))

(defstruct control 
	(lsh  (make-lsh!))
	(ch   (make-ch!)))

(defun cli2options (thing)
	(dolist (x (cli) thing)
     (aif (find (format nil ":~a" x) (the-slots things))
       (setf (slot-value thing x) 
             (read-from-string (nth (+ 1 a ) lst))))))

(defvar *O* (make-control))

;---------.---------.---------.---------.--------.---------.----------
; tricks 
(defun has (x &rest lst) 
  (dolist (y lst)
     (if (find y x :test #'equal) (return t))))

(let ((whitespace '(#\, #\space #\tab #\newline)))
	(defun s->words (s &optional (sep whitespace))
		(with-input-from-string (str s)
			(let (tmp out)
				(labels 
					((end-of-word () 
						(when tmp
							(push (concatenate 'string (reverse tmp)) out) 
							(setf tmp nil)) 
							out))
					(whale (read-char str nil)
								 (if (member a sep :test #'eql)
										 (end-of-word)
										 (push a tmp)))
					(reverse (end-of-word)))))))

;---------.---------.---------.---------.--------.---------.----------
; symbols
(defstruct sym
  (count (make-hash-table))
  (n 0) (pos 0) (txt "") (w 1)
  (ent 0)
  most
  mode)

(defmethod var ((s sym) x) (ent x))
(defmethod mid ((s sym) x) (sym-mode x))
(defmethod prep ((s sym) x) x)

(defmethod update ((s sym) x)
  (with-slots (most mode counts  n) s
    (let* ((new (incf (gethash x counts 0))))
			(if (> new most)
				(setf most new
              mode x)))
    x))

(defmethod ent ((s sym) &aux (e 0))
  (with-slots (counts n) s
    (do-hash (k v counts e)
      (let ((p (/ v n)))
        (decf e (* p (log p 2)))))))

;---------.---------.---------.---------.--------.---------.----------
; numbers
(defstruct num 
  (n 0) (pos 0) (txt "") (w 1)
  (mu 0.0) (m2 0.0) (sd 0.0)
  (lo most-positive-fixnum)
  (hi most-negative-fixnum))

(defmethod var ((s num) x) (num-sd x))
(defmethod mid ((s num) x) (num-mu x))
(defmethod prep ((s num) x) (if (numberp x) x (read-from-string x)))

(defmethod update ((nu num) x)
  (with-slots (n all lo hi mu m2 sd) nu
    (setf x (prep nu x))
    (let ((delta (- x mu)))
      (setf lo (min lo x)
            hi (max hi x)
            mu (+ mu (/ delta n))
            m2 (+ m2 (* delta (- x mu))))
      (sd-prim nu))
    x))

(defmethod sd-prim ((nu num))
  (with-slots (sd n m2) nu
    (setf sd (cond ((< n 2)  0)
                   ((< m2 0) 0)
                   (t  (sqrt (/ m2 (- n 1))))))))

(defmethod norm ((nu num) x)
  (with-slots (lo hi) nu
    (/ (- x lo) (+ (- hi lo) (/ 1 most-positive-fixnum)))))

;---------.---------.---------.---------.--------.---------.----------
; tables of data
(defstruct col (w 1) (pos 0) (txt ""))
(defstruct cols all klass goals names indep nums syms meta)
(defstruct row cells poles)
(defstruct data rows (cols (make-cols)))

(defmethod create ((c cols) arr)
  (with-slots (indep klass all names goals meta nums syms) c
    (do-array (x pos arr)
      (labels 
        ((goalp()  (has x (?? ch klass) (?? ch less) (?? ch more)))
         (nump()   (has x (?? ch num)   (?? ch less) (?? ch more)))
         (klassp() (has x (?? ch klass)))
         (metap()  (has x (?? ch meta)))
         (lessp()  (has x (?? ch less)))
         (skipp()  (has x (?? ch skip))))
        (unless (skipp)
          (push x names)
          (if (klassp) (setq klass pos))
          (let* ((w     (if (lessp) -1 1))
                 (todo  (if (nump) #'make-num #'make-sym))
                 (col   (funcall todo :pos pos :txt x :w w)))
            (push col all)
            (push col (if (nump) nums  syms))
            (if (metap)
              (push col meta)
              (push col (if (goalp) goals indep)))))))
    (reverse all)))

(defmethod update ((c cols) arr)
  (do-array (col _ (? cols all) arr)
    (setf (aref arr (? col pos))
          (update col (aref arr (? col pos))))
    (print 22)))

(defmethod update ((d data) arr)
  (if (? d cols names) 
    (let ((arr (prog1 (update (? d cols) arr) (print 33))))
      (push (make-row arr) (data-rows d)))
    (create (? d cols) arr))
	(print (? d cols)))

(defmethod readd ((d data) &optional f)
   (do-read (x f d)
     (update d (coerce (s->words x) 'vector))))

(format t "~a" (?? ch klass))

(let ((d (make-data))) 
	(readd d)
  d
  ) 
