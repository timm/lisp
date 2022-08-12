; Semi-supervised multi-objective explanation facility.
(defpackage :tiny (:use :cl) (:nicknames "tn"))
(in-package :tiny)
(mapc #'load  '("lib/macros"    "lib/maths"  "lib/strings" 
                "lib/settings" "lib/structs" "lib/demos" ))
(defvar my 
  (settings "TOYIN: do stuff
             (c) 2022 Tim Menzies, BSD-2 clause license "
  '((file  "-f"  "help file                " "../../data/auto93.lisp")
    (help  "-h"  "show help                " nil)
    (keep  "-K"  "items to keep            " 256)
    (k     "-k"  "nb low attributes classes" 1)
    (m     "-m"  "nb low frequency classes " 2)
    (seed  "-s"  "random number seed       " 10019)
    (go    "-g"  "start up action          " ls))))

(mapc #'load '("sample" "sym" "num" "about" "row" "data"))
