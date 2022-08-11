(defun a (lst)
  (destructuring-bind
    (b c d e)
    lst
    (format t "~%~%;;; => first:~a second:~a~&" b e)))

(a '(10 20 30 40))
