; vim: noai:ts=2:sw=2:et: 
(format *error-output* "; okthe.lisp~%")
(or (fboundp 'dofun) (load "test"))
(or (fboundp 'while) (load "macros"))

(dofun while ()
  (let ((n 0) (sum 0))
    (while (< (incf n) 10) 
      (incf sum n))
    (test (= 46 sum))))
