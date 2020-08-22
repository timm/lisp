; vim: noai:ts=2:sw=2:et: 
(load "got")
(got "yes" "macros")

(dofun while (&aux (n 0) (sum 0))
  (while (< (incf n) 10) 
    (incf sum n))
  (yes (= 45 sum)))
