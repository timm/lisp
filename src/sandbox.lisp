; vim: ts=2 sw=2 et :
;
(defun pretty (lst &optional pre)
  (labels ((item (lst pre) (when lst (pretty (first lst) pre)
                                     (when (rest lst) 
                                        (format t "~%~{~a~}" pre)
                                        (item (rest lst) pre)))))
    (cond ((null lst)  (princ "()"))
          ((atom lst)  (princ lst))
          ((listp lst) (princ "(") (item lst (cons "   " pre)) (princ ")")))))

(pretty '(a (b "asdas " de) fg))
