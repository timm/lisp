; vim: noai:ts=2:sw=2:et: 
(load "got")
(got "rows" "yes")

(defvar *data*
  '(weather 
    "outlook,$temp,	?$humid,wind,	!play        
     sunny,	85,	85,	FALSE,	no
     sunny,	80,	90,	TRUE,	no
     overcast,83,	86,	FALSE,	yes
     rainy,	70,	96,	FALSE,	yes
     rainy,	68,	80,	FALSE,	yes
     rainy,	65,	70,	TRUE,	no
     overcast,64,	65,	TRUE,	yes
     sunny,	72,	95,	FALSE,	no
     sunny,	69,	70,	FALSE,	yes
     rainy,	75,	80,	FALSE,	yes
     sunny,	75,	70,	TRUE,	yes
     overcast,72,	90,	TRUE,	yes
     overcast,81,	75,	FALSE,	yes
     rainy,	71,	91,	TRUE,	no"))

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



