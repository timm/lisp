;; vim: ts=2 sw=2 sts=2 et :
;-------- -------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(defparameter *tests* nil)
(defparameter *tries*   0)
(defparameter *fail*    0)

(defmacro deftest (name args &optional (doc "") &body body)
    (pushnew name *tests*)
    `(defun ,name ,args ,doc
       (format t "~&~%;;; ~a~%; ~a~%" ',name ,doc)
       ,@body))

(defmacro ok (want got &optional (msg "") &rest txt)
  `(progn 
     (incf *tries*)
     (handler-case
       (if (not (equalp ,want ,got))
	 (error (format nil ,msg ,@txt)))
       (t (c)
	  (incf *fail*)
	  (format t "; E> ~a" c)))))

(defun tests ()
  (when *tests*
    (mapc #'funcall (reverse *tests*))
    (let ((pass (- *tries* *fail*)))
      (format 
	t 
	"~&~%; pass: ~a = ~5,1f% ~%; fail: ~a = ~5,1f% ~%"
	pass   (* 100 (/  pass  (+ 0.0001 pass *fail*)))
	*fail* (* 100 (/ *fail* (+ 0.0001 pass *fail*)))))))
