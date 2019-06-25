(unless (fboundp 'got) (load "../got"))

(got "lib/" "oo/" )

(deftest q1()
	 (ok '(SLOT-VALUE (SLOT-VALUE (SLOT-VALUE OO 'A) 'B) C)
	       (macroexpand '(? oo 'a 'b c)) "bad expand"))

