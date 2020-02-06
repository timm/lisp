;; vim: ts=2 sw=2 sts=2  et :
;-------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(got "oo/thing.lisp")

(defthing keeper thing (id (gensym "kept")) (_cache))

(fyi "`Keeper`s are subclasses of `thing`s that,
optionally, now how to cache the results of a method
call (so if that method is called N times, we only
compute it once). 

The cache is maintain within the `_cache` variable.")

(defmacro defkept (m a &body b)
  "Define a method that will cache its result,
  returning the same result if called multiple times"
  (if (stringp (car b))
    `(defmethod ,m (,@a) ,(car b) (_keep ,(caar a) ,@(cdr b)))
    `(defmethod ,m (,@a)          (_keep ,(caar a) ,@b))))

;;; util
(defmacro _keep (it &body body)
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


