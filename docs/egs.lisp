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
