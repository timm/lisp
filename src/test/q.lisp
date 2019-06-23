(unless (fboundp 'got) (load "../got"))

(got   "oo/q.lisp" "lib/ok.lisp" )

(deftest q1()
	 (test '(SLOT-VALUE (SLOT-VALUE (SLOT-VALUE OO 'A) 'B) C)
	       (macroexpand '(? oo 'a 'b c)) "bad expand"))

(tests)
