; vim: noai:ts=2:sw=2:et: 
(or (fboundp 'lib) (load "lib"))
(lib "macros")

"
Global options used by everyone.
Accessed as follows (for example):

    (? char skip) ; ==> returns #\?

"

(defvar *the*
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
         ok   (     it ""
                    pass 0
                    fail 0)))

(defmacro ? (&rest fs) 
   "getter for globals"
   `(getr getf *the* ,@fs))

(defun skip? (x) 
   (and (stringp x)
        (string-equal x (? ch skip))))

(defun num? (x &aux (n (elt x 0)))
  (or (eql n (? ch num))
      (eql n (? ch less))
      (eql n (? ch more))))
