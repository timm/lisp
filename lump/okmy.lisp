; vim: noai:ts=2:sw=2:et: 
(load   "got")
(got "my" "test")
                   
(dofun the ()
   (print (my ch skip))
   (test (eql (my ch skip) #\?))
   (setf (my ch skip) #\a)
   (test (eql (my ch skip) #\a))
   (setf (my ch skip) #\?)
)
