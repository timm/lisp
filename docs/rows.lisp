; [aas](asda) [2121](asdaa)        
; [![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/Naereen/StrapDown.js/graphs/commit-activity)
; [![Ask Me Anything !](https://img.shields.io/badge/Ask%20me-anything-1abc9c.svg)](https://GitHub.com/Naereen/ama)
; [![GitHub license](https://img.shields.io/github/license/Naereen/StrapDown.js.svg)](https://github.com/Naereen/StrapDown.js/blob/master/LICENSE)
; [![GitHub release](https://img.shields.io/github/release/Naereen/StrapDown.js.svg)](https://GitHub.com/Naereen/StrapDown.js/releases/)
; [![Unlicense](https://img.shields.io/badge/License-Unlicense-blue.svg)](https://unlicense.org/)  
; 


;





; create the  right  kind of column,
; place it in  the  right kind  of places
(defun column (txt at rows)
  (let* ((what (if (upper-case-p (subseq txt 0 1)) 'num 'sym))
         (it   (make-instance what :txt txt :at at)))
    (if (has txt #\-) ; something to minimize
      (setf (? it w -1)))
    (push it (? rows cols all)) 
    (when (not (has txt #\?)); not skipping
      (if (has txt #\!)  ; klass column
        (setf (? it rows cols klass) it))
      (if (has txt #\- #\+ #\!) ; goal column
        (push it (? rows cols y))
        (push it (? rows cols x))))
    it))
