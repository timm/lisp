; vim: noai:ts=2:sw=2:et: 
(load "es")

(in-package :espy)

(defvar *fails* 0)

(defmacro ok (x y &optional (msg "") &rest txt)
  `(handler-case
     (if (not (equalp ,x ,y)) (error (format nil ,msg ,@txt)))
     (t  (c)                  (format t "; E[~a]> ~a~%"  (incf *fails*) c))))

(defun demo-num (MY)
  "Testing nums"
  (ok 2.34375 (sd (add (make-num) '(1 2 3 4 5 6 6 7 7 8))) "bad sd"))

(do-all-symbols (s '())
  (when (and (fboundp s)  (in "DEMO-" (symbol-name s)))
    (format t "~&; ~a : ~a ~%" s (documentation s 'function))
    (funcall s (make-options))))
