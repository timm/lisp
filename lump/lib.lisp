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

(defun skip? (x) 
   (and (string x)
        (string-equal x (? char skip))))

(defun num? (x)
  (let ((n (elt x 0)))
    (or (string-equal n (? char num))
        (string-equal n (? char less))
        (string_equal n (? char more)))))
