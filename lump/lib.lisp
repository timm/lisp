; vim: noai:ts=2:sw=2:et: 
(format *error-output* "; lib.lisp~%")

(let (seen)
  (defun lib (&rest files)
    (mapc (lambda (f)
            (unless (member f seen)
              (format *error-output* "; ~(~a~).lisp~%" f)
              (push f seen)
              #-sbcl (load f) 
              #+sbcl (handler-bind
                       ((style-warning #'muffle-warning))
                       (load f))))
          files)))

(lib "macros" "the" "oo" "test") ; macros os))

(defun pre (x y) 
  "returns t if the string or symbol 'x' starts with 'y'"
  (typecase x
    (symbol (pre (symbol-name x) y))
    (t      (eql (char x 0) y))))
