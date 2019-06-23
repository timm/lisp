#-ish(load "../lib/ish")

(needz  "oo/q" "lib/ok")

(deftest q1()
	 (test '(SLOT-VALUE (SLOT-VALUE (SLOT-VALUE OO 'A) 'B) C)
	       (macroexpand '(? oo 'a 'b c)) "bad expand"))

(tests)
