; test suite
(load "tiny")
(in-package :tiny)

(eg my () "show options" (pprint my) t)

(eg sym () "sym" 
  (let ((s (add (make-sym) '(a a a a b b c))))
    (and (= 1.379 (rnd (div s))) (eq 'c (mid s)))))

(eg sample () "sample"
  (setf (! my keep) 64)
  (let ((s (make-sample)))
    (dotimes (i 100) (add s (1- i)))
    (and (= 32.170544 (div s)) (= 56 (mid s)))))

(eg num () "num nums" 
  (setf (! my keep) 64)
  (let ((n  (make-num)))
    (dotimes (i 100) (add n (1- i)))
    (and (= 98 (? n hi)) (= 32.170544 (div n)) (= 56 (mid n)))))

(eg cols () "cols"
   (print (make-cols '("aa" "bb" "Height" "Weight-" "Age-")))
   t)

(eg lines () "lines"
  (with-lines "../../data/auto93.csv" 
              (lambda (x) (print (cells x))))
  t)

(eg rows () "rows"
   (let ((rows (make-rows "../../data/auto93.csv")))
     (print (? (? rows cols) y)))
   t)

(eg dist () "dist"
  (let ((r (make-rows "../../data/auto93.csv")))
    (dotimes (i 20 t)
      (let ((one (nth (randi (length (? r rows))) (? r rows)))
            (two (nth (randi (length (? r rows))) (? r rows))))
        (print (dist (? r cols) one two))))))
   
(demos my *egs* (! my example))
