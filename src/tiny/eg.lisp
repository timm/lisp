; test suite
(load "tiny")
(in-package :tiny)

(eg my () "show options" (pprint my) t)

(eg any () "any, many"
    (print (sort (loop repeat 20 collect (any #(10 20 30 40))) #'<))
    (print (sort (many #(10 20 30 40 50 60 70 80 90 
                         100 110 120 130 140 150) 5) #'<))
    t)

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

(eg data () "data"
    (let ((d (make-data "../../data/auto93.csv")))
      (print (? (? d cols) y)))
    t)

(eg dist () "dist"
    (let (all
           (d (make-data "../../data/auto93.csv")))
      (dolist (two (cdr (? d rows)))
        (push (dist d (car (? d rows)) two) all))
      (format t "~{ ~,3f~}" (sort all #'<))
      t))

(eg half () "half"
    (let ((d (make-data "../../data/auto93.csv")))
      (multiple-value-bind 
        (left right lefts rights c)
        (half d)
        (format t "~&~a~%~a~%~a~%" (? left cells) (? right cells) c)))
    t)

(demos my *egs* (! my example)) 
