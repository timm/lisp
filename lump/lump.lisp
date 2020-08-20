; vim: noai:ts=2:sw=2:et: 
(format *error-output* "; lump.lisp~%")
(unless (fboundp 'num) (load "num"))
(unless (fboundp 'send) (load "lib"))

(defmacro !! (obj f &rest args)
  `(funcall (slot-value ,obj ',f) ,obj ,@args))


(print (send (make-num) add 22))
