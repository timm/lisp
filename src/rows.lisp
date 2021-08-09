; vim: ts=2 sw=2 et:

(defun column (txt at rows)
  (let ((what (if (upper-case-p (subseq txt 0 0)) 'num 'sym))
        (it   (make-instance what :txt txt :at at)))
    (with-slots (txt w) it
      (push it (? rows cols all))
      (if (has txt #\-) 
        (setf w -1)) 
      (when (not (has txt #\?))
        (if (has txt #\!) 
          (setf (? rows cols klass) c))
        (if (has txt #\- #\+ #\!)
          (push it (? _rows cols y))
          (push it (? _rows cols x)))))
    it))


