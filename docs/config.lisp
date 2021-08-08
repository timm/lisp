; [aas](asda) [2121](asdaa)        
; [![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/Naereen/StrapDown.js/graphs/commit-activity)
; [![Ask Me Anything !](https://img.shields.io/badge/Ask%20me-anything-1abc9c.svg)](https://GitHub.com/Naereen/ama)
; [![GitHub license](https://img.shields.io/github/license/Naereen/StrapDown.js.svg)](https://github.com/Naereen/StrapDown.js/blob/master/LICENSE)
; [![GitHub release](https://img.shields.io/github/release/Naereen/StrapDown.js.svg)](https://GitHub.com/Naereen/StrapDown.js/releases/)
; [![Unlicense](https://img.shields.io/badge/License-Unlicense-blue.svg)](https://unlicense.org/)  
; 


;





(defvar +config+
  `(all (
         eg  "eg.hi"       ; default thing to run
         tries 0           ; number of runs
         fails 0           ; number of failed runs 
         seed 10013        ; random number seed
         data "../data/aa" ; data file to load
         loud nil          ; verbose mode
         meek nil)         ; meek mode: about  on any error
    col (p 2)              ; distance function coeffecient
    dom (samples 100)      ; samples for exploring domination
))
