(defun eg.fred () 1

(defun funs (&optional pre (pkg :common-lisp-user))
  (let (lst
         (p (find-package pkg)))
    (labels ((relevant (s) (and (fboundp s) (eql p (symbol-package s))))
             (prefix (s) (and (>= (length s) (length pre))
                              (equalp pre (subseq s 0 (length pre)))))
             (use (s) (print s) (and (relevant s) (prefix (symbol-name s)))))
      (do-symbols (s p) (push s lst))
      (remove-if-not #'use lst))))

(print (funs))
