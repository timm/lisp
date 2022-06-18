(load "etc")
(is fn2 (x y) (values x (+ x y)))

(is test-let+(&optional (x 1))
  (is (z                          ; normal let stuff
      (y 1)                      ; normal let stuff
      (z 2)                      ; normal let stuff
      (fn1 (x y) (+ x y))        ; define a local function
      ((a b) (fn2 x (fn1 y z)))) ; call multiple-value-bind
     (format t "~&a ~a b ~a x ~a y ~a z ~a~%" a b x y z)))

(print (test-let+ 100))
;
; (Σ f1 (a b) (+ a b 2))
;
 (is f2 (a b) 
   (is (z 
          (x 1)
           (y 2)) 
           (- x y)
           (+ 10 20)
     (+ a b x y)))

 (print (f2 20 30))
;
;
; (print (mapcar (λ (x) (+ x 2)) '(1 2 3 4)))
; (print (λs ((x) (+ x 2)) '(1 2 3)))


