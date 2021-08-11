---
title: "egs: "
---


```lisp
(defun eg.hi(my) (format t "~&Welcome to keys~%"))
(defun eg.a(my) (print 1))

(defun eg.csv(my  &aux  (n 0))
  (setf (! my all eg) "../data/auto93.csv")
  (assert (= 3192 (with-csv (row (! my all data) n)
                    (incf n (length row))))))

(defun eg.seed (my)                    
  (let ((n 100) a b)
    (setf *seed* 1
          a      (loop for x below  n collect (randf 1000)) 
          *seed* 1
          b      (loop for x below  n collect (randf 1000)))
    (want  (equal a b) "lists not equal")))

(defun eg.inca (the)
  (let ((lst '((a . 1)  (b . 2) (c . 3) (d . 4))))
    (inca 'a lst)
    (inca 'z lst)
    (want (and 
            (equal 1 (cdr (assoc 'z lst)))
            (equal 2 (cdr (assoc 'a lst)))) "nad inca")))

(defun eg.sym (the)
  (let ((n (add (make-sym) '("a" "b" "b" "c" "c" "c" "c"))))
    (want (< 1.378 (var n) 1.379) "bad ent")))

(defun eg.num (the)
  (let ((n (add (init(make-num  :txt "asd-"))
                '(2 3 4 4 4 4  5  5  6  7 
                  7 8 9 9 9 9 10 11 12 12))))
    (want (= 3.125 (var n)) "bad sd")
    (want (= 7     (mid n)) "bad mean")))

```
