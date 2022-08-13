(defpackage :tiny (:use :cl))
(in-package :tiny)
(mapc #'load  '("lib/macros"    "lib/maths"  "lib/strings" "lib/lists" 
                "lib/settings" "lib/structs" "lib/demos" ))

(defvar my (settings "
   TINY: semi-supervised multi-objective explanation facility.
   (c) 2022 Tim Menzies, BSD-2 clause license

   USAGE:  lisp eg.lisp [OPTIONS] [ARG]" 
   '((file  "-f"  "help file                " "../../data/auto93.lisp")
     (help  "-h"  "show help                " nil)
     (keep  "-K"  "items to keep            " 256)
     (k     "-k"  "nb low attributes classes" 1)
     (m     "-m"  "nb low frequency classes " 2)
     (seed  "-s"  "random number seed       " 10019)
     (go    "-g"  "start up action          " "ls"))))

(mapc #'load '("sample" "row" "sym" "num" "about" "data"))
