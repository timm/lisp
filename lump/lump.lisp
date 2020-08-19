; vim: noai:ts=4:sw=4:et: 
(unless (fboundp 'num) (load "num"))
(unless (fboundp 'send) (load "lib"))

(defmacro !! (obj f &rest args)
  `(funcall (slot-value ,obj ',f) ,obj ,@args))


(print (send (make-num) add 22))
