; vim: noai:ts=2:sw=2:et: 
(format *error-output* "; my.lisp~%")

(defstruct tests
  (it "")
  (pass 0)
  (fail 0))
 
(defstruct chars 
  (skip  #\?)
  (num   #\$)
  (less  #\<)
  (more  #\>)
  (klass #\!))

(defstruct config 
  (tests (make-tests))
  (chars (make-chars))
  (data "../.."))

(defvar *my* (make-config))
