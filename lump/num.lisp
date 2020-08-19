; vim: noai:ts=2:sw=2:et: 
(format *error-output* "; num.lisp~%")
(or (fboundp 'test) (load "lib"))

(defstruct num()
  (pos 0)
  (txt "")
  (w 0)
  (n 0)
  (mu 0)
  (m2 0)
  (add 'numadd))

(defun col (txt pos &optional (make 'make-num))
  (let ((out (funcall make  :txt txt :pos pos)))
   (setf (? out w)  (if (find (?? chars less)  txt) -1 1 
(defun numadd(i x)
	(with-slots( n  mu) i
	 (incf n x))
	 i)


