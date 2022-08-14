; test suite
(load "tiny")
(in-package :tiny)

(defdemo my () "show options" (pprint my) t)

(defdemo div () "num divs" 
  (let ((s (add (make-sym) '(a a a a b b c))))
    (and (= 1.379 (rnd (div s))) (eq 'c (mid s)))))

(defdemo num () "num nums" 
  (let ((n  (make-num)))
   ; (add n 10)
    ;(print n)
    ))

(defdemo nums () "num nums" 
  (let ((n (add (make-num) (loop for j from 0 to 10 collect j))))
    (format t "~a ~a~%" (div n) (mid n))
    t))

(demos my *demos* (! my go))
