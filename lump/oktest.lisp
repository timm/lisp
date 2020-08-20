; vim: noai:ts=2:sw=2:et: 
(format *error-output* "; okthe.lisp~%")
(or (boundp 'dofun) (load "test"))

(dofun fail ()
  (test nil))

