; vim: ts=2 sw=2 et:

(defun column (txt at rows)
  (let ((what (if (upper-case-p (subseq txt 0 0)) 'num 'sym))
        (it   (make-instance what :txt txt :at at)))
    (if (has txt #\-) 
      (setf (? it w -1)))
    (push it (? rows cols all))
    (when (not (has txt #\?))
      (if (has txt #\!) 
        (setf (? it rows cols klass) it))
      (if (has txt #\- #\+ #\!)
        (push it (? rows cols y))
        (push it (? rows cols x))))
    it))


