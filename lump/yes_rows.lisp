; vim: noai:ts=2:sw=2:et: 
(load "got")
(got "rows" "yes" "yes_data")

(print (lines (getf *data* 'weather)))

(dofun use (&aux (i (make-instance 'rows)))
   (let* ((data '(("name" "$age" "?size" "!job")
                  (0      10      20     30)))
	  (u    (use? i (first data))))
     (print (use! i u (second data)))))

(dofun cols (&aux (c (make-instance 'cols)))
   (header c '("name" "$age" "$size" "!job"))
   (print (? c all)))

(dofun weather (&aux (data (make-instance 'rows)))
  (take data (lines (getf *data* 'weather)))
  (print "")
  (print (? data cols x)))

(dofun diabetes (&aux (data (make-instance 'rows)))
  (take data (lines (getf *data* 'diabetes)))
  (print (length (? data all))))
