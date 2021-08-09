; vim: ts=2 sw=2 et:

(defun eg.hi(my) (format t "~&Welcome to keys~%"))
(defun eg.a(my) (print 1))

(defun eg.csv(my  &aux  (n 0))
  (setf (! my all eg) "../data/auto93.csv")
  (assert (= 3192 (with-csv (row (! my all data) n)
                    (incf n (length row))))))

(defun eg.seed (my)                    
  (let ((n 100) a b)
    (setf *seed* 1
          a      (loop for x below  n collect (randf 1000)) 
          *seed* 1
          b      (loop for x below  n collect (randf 1000)))
    (want  (equal a b) "lists not equal")))

(defun eg.inca (the)
  (let ((lst '((a . 1)  (b . 2) (c . 3) (d . 4))))
    (inca 'a lst)
    (inca 'z lst)
    (want (and 
            (equal 1 (cdr (assoc 'z lst)))
            (equal 2 (cdr (assoc 'a lst)))) "nad inca")))

(defun eg.num (the)
  (let ((n (make-num)))
    (add n '(9 2 5 4 12 7 8 11 9 3 7 4 12 5 4 10 9 6 9 4))
    ;(want (< 3.06 (var n) 3.07) "bad sd")
    (print  (var  n))
    (print  (mid  n))))

