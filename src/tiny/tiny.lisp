(defpackage :tiny (:use :cl))
(in-package :tiny)
(mapc #'load  '("lib/macros"    "lib/maths"  "lib/strings" "lib/lists" 
                "lib/settings" "lib/structs" "lib/egs" ))

(defvar my (settings "
   TINY: semi-supervised multi-objective explanation facility.
   (c) 2022 Tim Menzies, BSD-2 clause license

   USAGE:  lisp eg.lisp [OPTIONS] [ARG]" 
   '((far   "-F"  "how far is distant       " .95)
     (file  "-f"  "help file                " "../../data/auto93.lisp")
     (help  "-h"  "show help                " nil)
     (keep  "-K"  "items to keep            " 256)
     (k     "-k"  "nb low attributes classes" 1)
     (m     "-m"  "nb low frequency classes " 2)
     (p     "-p"  "distance coeffecient     " 2)
     (seed  "-s"  "random number seed       " 10019)
     (some  "-S"  "how many                 " 512)
     (example "-e"  "example to run         " "ls"))))

(mapc #'load '("col/sample" "col/sym" "col/num" "col/cols" "row/row" "row/rows"))
