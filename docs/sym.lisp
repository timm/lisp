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
; Add a symbo, update symbol counts,  update mode
(defmethod add1 ((s sym) x)
  (let ((n (inca x (? s seen))))
    (when (> n (? s most))
      (setf (? s most) n
            (? s mode) x))))
; Central tendency.
(defmethod mid ((s sym)) (? s mode))
; Variable around centrality.
(defmethod var ((s sym)) (entropy (? s seen)))
; Seperation of two items.
(defmethod dist1 ((s sym) x y) (if (eql x y) 0 1))
