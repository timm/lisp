; vim: noai:ts=4:sw=4:et: 

(unless (fboundp 'test) (load "lib"))

(defstruct num()
  (n 0)
  (mu 0)
  (m2 0)
  (add 'numadd))

(defun numadd(i x)
	(with-slots( n  mu) i
	 (incf n x))
	 i)


