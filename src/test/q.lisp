#-ish(load "../lib/ish")

(has "oo/q")

(format t "~a~%" (macroexpand '(? oo 'a 'b c)))
