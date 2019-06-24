; vim: ts=2 sw=2 sts=2  et
(let 
  ((pats '("../*/" "../*/*.lisp"))
   files
   gotten)
  (labels 
    ((knowns (x) 
        (remove-if-not #'(lambda (z) (end z x)) files))

     (containsp (s1 s2)
        (unless (string-equal s2 "")
          (loop for p = (search s2 s1) 
                then (search s2 s1 :start2 (1+ p))
                while p 
                collect p)))

     (end (s1 s2)
        (let ((p (mismatch s2 s1 :from-end T)))
          (or (not p) (= 0 p))))

     (got1 (f)
        (format *error-output* "~&; loading ~a~%" f)
        (if (end f "/") 
          (dolist (f (directory (format nil "~a/*.lisp" f)))
            (got (namestring f)))
          (progn 
            #-sbcl (load f) 
            #+sbcl (handler-bind
                     ((style-warning #'muffle-warning))
                     (load f))))))

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
                (got1 (car where))))))))))
