;; vim: ts=2 sw=2 sts=2  et :
;-------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(got "oo/thing.lisp")

(defthing keeper thing (id (gensym "kept")) (_cache))

(defmacro keep (it &body body)
  "with hash table _cache, compute once, then keep"
  (let ((val     (gensym))
	(found-p (gensym))
	(key     (gensym)))
    `(with-slots (_cache) ,it
       (setf _cache (or _cache (make-hash-table)))
       (multiple-value-bind (,val ,found-p)
	 (gethash ',key _cache)
	 (if ,found-p 
	   ,val
	   (setf (gethash ',key _cache)
		 (progn ,@body)))))))

(defmacro defkept (m a &body b)
  (if (stringp (car b))
    `(defmethod ,m (,@a) ,(car b) (keep ,(caar a) ,@(cdr b)))
    `(defmethod ,m (,@a)          (keep ,(caar a) ,@b))))
