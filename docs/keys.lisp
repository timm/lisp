; [aas](asda) [2121](asdaa)        
; [![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/Naereen/StrapDown.js/graphs/commit-activity)
; [![Ask Me Anything !](https://img.shields.io/badge/Ask%20me-anything-1abc9c.svg)](https://GitHub.com/Naereen/ama)
; [![GitHub license](https://img.shields.io/github/license/Naereen/StrapDown.js.svg)](https://github.com/Naereen/StrapDown.js/blob/master/LICENSE)
; [![GitHub release](https://img.shields.io/github/release/Naereen/StrapDown.js.svg)](https://GitHub.com/Naereen/StrapDown.js/releases/)
; [![Unlicense](https://img.shields.io/badge/License-Unlicense-blue.svg)](https://unlicense.org/)  
; 


;





(defpackage :keys (:use :cl))
(in-package :keys)
(defun loads(f)
  (format *error-output* "; loading ~(~a~) ...~%" f)
  (handler-bind ((style-warning #'muffle-warning)) (load f)))
(loads "structs")
(loads "tricks")
(loads "config")
(loads "col")
(loads "num")
(loads "sym")
(loads "row")
(loads "rows")
(loads "egs")
(main +config+ :package :keys)
