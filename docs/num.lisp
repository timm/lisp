; [aas](asda) [2121](asdaa)        
; [![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/Naereen/StrapDown.js/graphs/commit-activity)
; [![Ask Me Anything !](https://img.shields.io/badge/Ask%20me-anything-1abc9c.svg)](https://GitHub.com/Naereen/ama)
; [![GitHub license](https://img.shields.io/github/license/Naereen/StrapDown.js.svg)](https://github.com/Naereen/StrapDown.js/blob/master/LICENSE)
; [![GitHub release](https://img.shields.io/github/release/Naereen/StrapDown.js.svg)](https://GitHub.com/Naereen/StrapDown.js/releases/)
; [![Unlicense](https://img.shields.io/badge/License-Unlicense-blue.svg)](https://unlicense.org/)  
; 


;





; Methods
; -------
; Keep numbers (or numbers taken from  strings), in `_all`.
; Set `sorted` to nil
(defmethod add1 ((n num) (x string)) (add1 n (read-from-string x)))
(defmethod add1 ((n num) (x number))
  (vector-push-extend x (? n _all))
  (setf (? n sorted) nil))
; Central tendancy.
(defmethod mid ((n num)) (per (all n) .5))
; Variations around the mid.
(defmethod var ((n num)) (sd  (all n) t))
; Normalize numbers before we do numbers.
(defmethod dist1 ((n num) a b)
  (cond ((eq a #\?) (setf b (norm n b) a (if (> b 0.5) 1 0)))
        ((eq b #\?) (setf a (norm n a) b (if (> a 0.5) 1 0)))
        (t          (setf a (norm n a) b (norm n b))))
  (abs (- a b)))
; Functions
; ---------
; Return `_all`, sorted.
(defun all (n)
  (unless (? n sorted)
    (setf (? n _all)   (sort (? n _all) #'<)
          (? n sorted) t))
  (? n _all))
; `Lo` and `hi` is computed from `all`.
(defun lo (n)                  (aref (all n) 0))
(defun hi (n &aux (a (all n))) (aref a (1- (length a))))
; Normalized `n` 0..1 lo..hi.
(defun norm (n x &aux (n1 (lo n)) (n2 (hi n)))
  (cond ((eq x #\?)  x)
        ((eql n1 n2) 0)
        (t (max 0 (min 1 (/ (- x n1) (- n2 n1 1E-32)))))))
