(defstruct+ num  (txt "") (at 0) kept ok (w 1))
(defun make-num (s n) (%make-num :txt s :at n :w (if (equal #\- (charn s)) -1 1)))

