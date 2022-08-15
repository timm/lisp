; test suite
(load "tiny")
(in-package :tiny)

(defdemo my () "show options" (pprint my) t)

(defdemo sym () "sym" 
  (let ((s (add (make-sym) '(a a a a b b c))))
    (and (= 1.379 (rnd (div s))) (eq 'c (mid s)))))

(defdemo sample () "sample"
  (setf (! my keep) 64)
  (let ((s (make-sample)))
    (dotimes (i 100) (add s (1- i)))
    (and (= 32.170544 (div s)) (= 56 (mid s)))))

(defdemo num () "num nums" 
  (setf (! my keep) 64)
  (let ((n  (make-num)))
    (dotimes (i 100) (add n (1- i)))
    (and (= 98 (? n hi)) (= 32.170544 (div n)) (= 56 (mid n)))))

(defdemo cols () "cols"
   (print (make-cols '("aa" "bb" "Height" "Weight-" "Age-")))
   t)

(defdemo lines () "lines"
  (with-lines "../../data/auto93.csv" 
              (lambda (x) (print (cells x))))
  t)

(defdemo rows () "rows"
   (let ((rows (make-rows "../../data/auto93.csv")))
     (print (? (? rows cols) y)))
   t)

(defdemo dist () "dist"
  (let ((r (make-rows "../../data/auto93.csv")))
    (dotimes (i 20 t)
      (let ((one (nth (randi (length (? r rows))) (? r rows)))
            (two (nth (randi (length (? r rows))) (? r rows))))
        (print (dist (? r cols) one two))))))
   
(demos my *demos* (! my go))
