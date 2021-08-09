; [aas](asda) [2121](asdaa)        
; [![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/Naereen/StrapDown.js/graphs/commit-activity)
; [![Ask Me Anything !](https://img.shields.io/badge/Ask%20me-anything-1abc9c.svg)](https://GitHub.com/Naereen/ama)
; [![GitHub license](https://img.shields.io/github/license/Naereen/StrapDown.js.svg)](https://github.com/Naereen/StrapDown.js/blob/master/LICENSE)
; [![GitHub release](https://img.shields.io/github/release/Naereen/StrapDown.js.svg)](https://GitHub.com/Naereen/StrapDown.js/releases/)
; [![Unlicense](https://img.shields.io/badge/License-Unlicense-blue.svg)](https://unlicense.org/)  
; 


;





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
  (let ((n (make-sym)))
    (add n '("a" "b" "b" "c" "c" "c" "c"))
    (want (< 1.378 (var n) 1.379) "bad ent")))
(defun eg.num (the)
  (let ((n (init(make-num  :txt "asd-"))))
    (print n)
    (add n '(2 3 4 4 4 4  5  5  6  7 
             7 8 9 9 9 9 10 11 12 12))
    (want (= 3.125 (var n)) "bad sd")
    (want (= 7     (mid n)) "bad mean")))
