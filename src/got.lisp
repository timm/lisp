(let 
  ((pats '("../*/" "../*/*.lisp"))
   files
   gotten)
  (labels 
    ((knowns (x) 
	     (remove-if-not #'(lambda (z) (ends-with-p z x)) files))

     (containsp (str1 str2)
		(unless (string-equal str2 "")
		  (loop for p = (search str2 str1) 
			then (search str2 str1 :start2 (1+ p))
			while p 
			collect p)))
     (ends-with-p (str1 str2)
		  (let ((p (mismatch str2 str1 :from-end T)))
		    (or (not p) (= 0 p))))
     (got1 (f)
	   (if (ends-with-p f "/") 
	     (dolist (file (directory (format nil "~a/*.lisp" f)))
	       (got  (namestring file)))
	     (progn 
	       #-sbcl (load f) 
	       #+sbcl (handler-bind
			((style-warning #'muffle-warning))
			(load f)))))
     )

    (dolist (pat pats)
      (dolist (file (directory pat))
	(push (namestring file) files)))

    (defun got (&rest lst)
      (dolist (f lst)
	(let ((where (knowns f)))
	  (if (not where) ; too few
	    (error "unknown file name [~a]" f)
	    (if (cdr where) ; too many
	      (error "ambiguous file name [~a]~%" f)
	      (unless (member (car where) gotten)
		(push (car where) gotten)
		(format t "~&; loading ~a~%" f)
		(got1 (car where)) ; just right
	      ))))))))

