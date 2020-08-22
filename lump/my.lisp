; vim: noai:ts=2:sw=2:et: 
(load "got")
(got "macros")

"
Global options used by everyone.
Accessed as follows (for example):

    (? char skip) ; ==> returns #\?

"

(defvar *my*
       '(ch (     skip  #\?
                    less  #\<
                    more  #\>
                    num   #\$
                    klass #\!)
         some (     max 512 
                    step .5 
                    cohen .3 
                    trivial 1.05)
         seed 1
         yes   (    it ""
                    pass 0
                    fail 0)))

(defmacro my (&rest fs) 
   "getter for globals"
   `(getr getf *my* ,@fs))

(defun skip? (x) 
   (and (stringp x)
        (eql x (my ch skip))))



