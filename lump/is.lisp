; vim: noai:ts=2:sw=2:et: 
(load "got")
(got "my")

(labels ((is (x &rest l)
	     (when (> (length x) 0)
	       (let ((n (elt x 0)))
		 (dolist (one l)
		   (if (eql n one)
		     (return t)))))))

  (defun less?   (x) (is (my ch less)))
  (defun ignore? (x) (is (my ch skip)))
  (defun klass?  (x) (is (my ch klass)))
  (defun goal?   (x) (is (my ch less) (my ch more) (my ch klass)))
  (defun num?    (x) (is (my ch num)  (my ch less) (my ch more)))
)
