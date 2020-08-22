; vim: noai:ts=2:sw=2:et: 
(load "got")
(got "yes" "is")

(dofun is()
  (yes (less?   "<cat")) (yes (not (less?   "cat")))
  (yes (ignore? "?cat")) (yes (not (ignore? "cat")))
  (yes (klass?  "!cat")) (yes (not (klass?  "cat")))
  (yes (goal?   ">cat")) (yes (not (goal?   "cat")))
  (yes (num?    "$cat")) (yes (not (num?    "cat")))
)
