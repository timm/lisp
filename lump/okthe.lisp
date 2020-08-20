; vim: noai:ts=2:sw=2:et: 
(format *error-output* "; okthe.lisp~%")
(or (boundp '*the*) (load "the"))
(or (boundp 'dofun) (load "test"))

                   
(dofun the ()
   (test (string-equal (? char skip) "?"))
   (setf (? char skip) "a")
   (test (string-equal (? char skip) "a"))
   (setf (? char skip) "?")
)
