---
title: "config: "
---

accessible via e.g. 

```lisp
;
```

   (? my all tries)

or on the command line   

   keys -seed 10013 -data ../data/fred.csv \
        --dom -samples 23

```lisp
;
(defvar +config+
  `(all (eg    "eg.hi"     ; default thing to run
         tries 0           ; number of runs
         fails 0           ; number of failed runs 
         seed 10013        ; random number seed
         data "../data/auto93.csv" ; data file to load
         loud nil          ; verbose mode
         meek nil)         ; meek mode: about  on any error
    col (p 2)              ; distance function coeffecient
    dom (samples 100)      ; samples for exploring domination
))

