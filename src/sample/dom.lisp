;; vim: ts=2 sw=2 sts=2  et :
;--------- --------- --------- --------- --------- ---------
(unless (fboundp 'got) (load "../got"))

(got "sample/sample.lisp")

(defmethod doms ((s sample) &optional (n (my :dom :samples)))
  (with-slots (egs) s
    (let* ((arr   (l->a egs))
	   (goals (append (lesss s) (mores s)))
	   (most  (length arr)))
      (dolist (eg1 egs)
	(dotimes (_ n)
	  (let ((eg2 (elt arr (random most))))
	    (if (dom eg1 eg2 goals)
	      (incf (? eg1 'dom) (/ 1 n))))))
	(setf egs 
	      (sort egs #'(lambda(eg) 
			    (* -1 (? eg 'dom))))))))

(defmethod dom ((x eg) (y eg) goals)
  (let ((s1 0) 
	(s2 0) 
	(n (length goals)))
    (dolist (goal goals (< (/ s1 n) (/ s2 n)))
      (let ((w (? goal 'w))
	    (a (norm col (cell x goal)))
	    (b (norm col (cell y goal))))
	(incf s1 (pow 10 (* w (/ (- a b) n))))
	(incf s2 (pow 10 (* w (/ (- b a) n))))))))
