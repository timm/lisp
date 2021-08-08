; vim: ts=2 sw=2 et:

(defun eg.hi(my) (format t "~&Welcome to keys~%"))
(defun eg.a(my) (print 1))

(defun eg.csv(my  &aux  (n 0))
  (setf (! my all eg) "../data/auto93.csv")
  (assert (= 3192 (with-csv (row (! my all data) n)
                    (incf n (length row))))))
