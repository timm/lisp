<img src='http://www.lisperati.com/lisplogo_fancy_256.png' width=200 align=right>


(load "etc")

(is fn2 ((x number) (y number)) (values x (+ x y)))

(is test-let+ (&optional (x 1))
 (is
  (z (y 1) (z 2) (fn1 (x y) (+ x y))
   ((a b) (fn2 x (fn1 y z))))
  (format t "~&a ~a b ~a x ~a y ~a z ~a~%" a b x y z)))

(test-let+ 100)

(is f2 (a b)
 (is (z (x 1) (y 2)) (- x y) (+ 10 20) (+ a b x y)))

(print (f2 20 30))

(print (mapcar (~ (x) (+ x 2)) '(1 2 3 4)))

(print (~~ ((x) (+ x 2)) '(1 2 3)))

(demo "asdas111" (> -1 0))

(demo "round" (equal 43.2 (rnd 43.222 1)))

(defstruct xy (x 0) (y 0))

````


; W? DO DOC  FAIL
