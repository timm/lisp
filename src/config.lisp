; vim: ts=2 sw=2 et:

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
