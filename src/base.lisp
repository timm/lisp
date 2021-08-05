; vim: filetype=lisp ts=2 sw=3 sts=2 et :

(defstruct about what who why)
(defstruct stuff doc options tests (seed 10013) (fails 0) (make-about))
(defmacro ? (s x &rest xs) 
  (if xs `(? (slot-value ,s ',x) ,@xs) `(slot-value ,s ',x)))


(defmacro def (name params doc &body)
  `(progn
     (push  `(,name . (,params ,doc)) (! doc))
     (defun ,name ,params ,doc ,@body)))

(defmacro deftest (name params doc &body)
  `(progn
    (def ,name (,params ,doc) ,@body)
    (pushnew ',name (! tests))))

(let (seen)
  (def loaded (f)
    (unless (member f seen)
      (push f seen)
      (format *error-output* "; loading ~(~a~) ...~%" f)
      (handler-bind ((style-warning #'muffle-warning)) (load f)))))
