; [aas](asda) [2121](asdaa)        
; [![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/Naereen/StrapDown.js/graphs/commit-activity)
; [![Ask Me Anything !](https://img.shields.io/badge/Ask%20me-anything-1abc9c.svg)](https://GitHub.com/Naereen/ama)
; [![GitHub license](https://img.shields.io/github/license/Naereen/StrapDown.js.svg)](https://github.com/Naereen/StrapDown.js/blob/master/LICENSE)
; [![GitHub release](https://img.shields.io/github/release/Naereen/StrapDown.js.svg)](https://GitHub.com/Naereen/StrapDown.js/releases/)
; [![Unlicense](https://img.shields.io/badge/License-Unlicense-blue.svg)](https://unlicense.org/)  
; 


;





(defstruct about what who why)
(defstruct stuff doc options tests (seed 10013) (fails 0) (make-about))
(defmacro ? (s x &rest xs) 
  (if xs `(? (slot-value ,s ',x) ,@xs) `(slot-value ,s ',x)))
(defmacro def (name params doc &body)
  `(progn
     (push  `(,name . (,params ,doc)) (! doc))
     (defun ,name ,params ,doc ,@body)))
(defmacro deftest (name params doc &body)
  `(progn
    (def ,name (,params ,doc) ,@body)
    (pushnew ',name (! tests))))
(let (seen)
  (def loaded (f)
    (unless (member f seen)
      (push f seen)
      (format *error-output* "; loading ~(~a~) ...~%" f)
      (handler-bind ((style-warning #'muffle-warning)) (load f)))))
