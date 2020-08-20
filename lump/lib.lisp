; vim: noai:ts=2:sw=2:et: 
(format *error-output* "; lib.lisp~%")
(or (boundp  '*the*) (load "the"))
(or (fboundp 'send)  (load "oo"))
(or (fboundp 'test)  (load "test"))
(or (fboundp 'while) (load "macros"))
(or (fboundp 'stop)  (load "os"))

(defun pre (x y) 
  "returns t if the string or symbol 'x' starts with 'y'"
  (typecase x
    (symbol (pre (symbol-name x) y))
    (t      (eql (char x 0) y))))

(defstruct cc
  (dd 0))

(defstruct aa
   (bb (make-cc)))

(let ((a (make-aa)))
   (setf (~  a bb dd) 21)
   (print (~  a bb dd)))

(print (? some cohen))

