; [aas](asda) [2121](asdaa)        
; [![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/Naereen/StrapDown.js/graphs/commit-activity)
; [![Ask Me Anything !](https://img.shields.io/badge/Ask%20me-anything-1abc9c.svg)](https://GitHub.com/Naereen/ama)
; [![GitHub license](https://img.shields.io/github/license/Naereen/StrapDown.js.svg)](https://github.com/Naereen/StrapDown.js/blob/master/LICENSE)
; [![GitHub release](https://img.shields.io/github/release/Naereen/StrapDown.js.svg)](https://GitHub.com/Naereen/StrapDown.js/releases/)
; [![Unlicense](https://img.shields.io/badge/License-Unlicense-blue.svg)](https://unlicense.org/)  
; 


;





(defmethod add1 ((n num) (x number))
  (push-vector-extend x (? n _all))
  (setf (? n sorted) nil)
  x)
(defmethod all ((n num))
  (unless (? n sorted)
    (setf (? n _all)   (sort (? n _all) #'<)
          (? n sorted) t))
  (? n _all))
(defmethod mid ((n num)) (per (all n) .5))
(defmethod var ((n num)) (sd  (all n))
(defmethod lo ((n  num)) (svref (all n) 0))
(defmethod hi ((n  num) &aux (a (all  n))) (svref a (1- (length a)))))
(defmethod dist1 ((n num) a b)
  (cond ((eq a #\?) (setf b (norm n b)
                          a (if (> b 0.5) 1 0)))
        ((eq b #\?) (setf a (norm n  a)
                          b (if (> a 0.5) 1 0)))
        (t          (setf a (norm n a)
                          b (norm n b))))
  (abs (- a b)))
(defmethod norm ((n num) x)
  (if (eq x #\?)
      x
    (let ((n1 (lo n)) (n2 (hi n)))
      (if (eql n1 n2)
          0
        (max 0 (min 1 (/ (- x n1) (- n2 n1 1E-32))))))))
 
