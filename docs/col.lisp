; [aas](asda) [2121](asdaa)        
; [![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/Naereen/StrapDown.js/graphs/commit-activity)
; [![Ask Me Anything !](https://img.shields.io/badge/Ask%20me-anything-1abc9c.svg)](https://GitHub.com/Naereen/ama)
; [![GitHub license](https://img.shields.io/github/license/Naereen/StrapDown.js.svg)](https://github.com/Naereen/StrapDown.js/blob/master/LICENSE)
; [![GitHub release](https://img.shields.io/github/release/Naereen/StrapDown.js.svg)](https://GitHub.com/Naereen/StrapDown.js/releases/)
; [![Unlicense](https://img.shields.io/badge/License-Unlicense-blue.svg)](https://unlicense.org/)  
; 


;





(defmethod init ((c col)) c)
(defmethod add ((c col) (x cons)) (dolist (y x) (add c y)))
(defmethod add ((c col) (x row))  (add c (row-cells x)))
(defmethod add ((c col) x)
  (unless (eq #\? x) (incf (? c n)) (setf x (add1 c x)))
  x)
(defmethod dist ((c col) x y)
  (if (and (eq x #\?) (eq y #\?)) 1 (dist1 x y)))
