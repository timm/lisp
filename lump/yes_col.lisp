; vim: noai:ts=2:sw=2:et: 
(load "got")
(got "col" "test")

(dofun 
  num ()
  (let ((n (add* '(9 2 5 4 12 7 8 11 9 3 
                   7 4 12 5 4 10 9 6 9 4))))
    (test (eql 7   (? n mu)))
    (test (< 3.06  (? n sd) 3.061))))

(dofun
  sym ()
  (let ((s (add* '(a  b b  c c c c) 'sym)))
    (print (ent s))
    (print s)))
