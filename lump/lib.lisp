; vim: noai:ts=4:sw=4:et: 
(format *error-output* "; lib.lisp~%")
(unless (boundp '*my*) (load "my"))
(unless (fboundp 'send) (load "oo"))
(unless (fboundp 'test) (load "test"))

(defun pre (x y) 
  (typecase x
    (symbol (pre (symbol-name x) y))
    (t      (eql (char x 0) y))))
