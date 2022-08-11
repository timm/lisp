(defstruct+ sym  (txt "") (at 0) kept)

(defun make-sym (s n) (%make-sym :txt s :at n))

