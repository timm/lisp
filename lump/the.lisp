; vim: noai:ts=2:sw=2:et: 
(format *error-output* "; the.lisp~%")
(or (fboundp '?) (load "macros"))

(defvar *the*
       '(char (     skip #\?
                    less #\>
                    more #\>
                    num #\$
                    klass #\!)
         skip "?"
         some (     max 512 
                    step .5 
                    cohen .3 
                    trivial 1.05)
         seed 1
         test (     it ""
                    pass 0
                    fail 0)))

"
ass stiff

Suicides
f
ff
adds
"

(defmacro ? (&rest fs) 
   `(getr getf *the* ,@fs))

