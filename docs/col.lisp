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
; If `x` is a list, add everything in it.
(defmethod add ((c col) (x cons)) (dolist (y x) (add c y)))
; Unless we are skipping  stuff, increment `n`.
(defmethod add ((c col) x)
  (unless (eq #\? x) 
    (incf (? c n)) 
    (add1 c x)))
; Functions
; ---------
(defmethod dist (c x y)
  (if (and (eq x #\?) (eq y #\?)) 1 (dist1 x y)))
