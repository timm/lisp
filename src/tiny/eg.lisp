; test suite
(load "tiny")
(in-package :tiny)

;(print (make-row 12 '(1 2 3 4)))
; (print (make-about '("$aa" "bb!~" "cc+")))
; (print (! my 'seed))
; (dotimes (i 20) (print (randi  200)))
; ; (defmethod clone ((d data) &optional src) (make-data (? d about names) src))
; ;(reads "../../data/auto93.lisp" 'print)

(defdemo my () "show options" (pprint my) t)

(defdemo div () "num divs" 
  (let ((s (add (make-sym) '(a a a a b b c))))
    (and (= 1.379 (rnd (div s))) (eq 'c (mid s)))))

(demos my *demos* (! my go))
