; vim: noai:ts=2:sw=2:et: 
(load "got")
(got "test" "is")

(dofun is()
  (test (less?   "<cat")) (test (not (less?   "cat")))
  (test (ignore? "?cat")) (test (not (ignore? "cat")))
  (test (klass?  "!cat")) (test (not (klass?  "cat")))
  (test (goal?   ">cat")) (test (not (goal?   "cat")))
  (test (num?    "$cat")) (test (not (num?    "cat")))
)
