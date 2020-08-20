; vim: noai:ts=2:sw=2:et: 
(format *error-output* "; the.lisp~%")
(or (fboundp 'getr) (load "macros"))

"
Global options used by everyone.
Accessed as follows (for example):

    (? char skip) ; ==> returns #\?

"

(defvar *the*
       '(char (     skip "?"
                    less ">"
                    more ">"
                    num  "$"
                    klass "!")
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

