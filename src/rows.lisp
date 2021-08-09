; vim: ts=2 sw=2 et:

; create the  right  kind of column,
; place it in  the  right kind  of places
(defun column-factory (txt at rows)
  (let* ((what (if (upper-case-p (char txt 0)) 'num 'sum))
         (it   (make-instance what :txt txt :at at)))
    (if (has txt #\-) ; something to minimize
      (setf (? it w)  -1))
    (push it (? rows cols all)) 
    (when (not (has txt #\?)); not skipping
      (if (has txt #\!)  ; klass column
        (setf (? it rows cols klass) it))
      (if (has txt #\- #\+ #\!) ; goal column
        (push it (? rows cols y))
        (push it (? rows cols x))))
    it))
