; vim: noai:ts=2:sw=2:et: 
(load "got")
(got "oo" "is" "strings" "col")

(defthing rows thing (all) (cols (make-instance 'cols)))
(defthing row thing (cells) (_rows))
(defthing cols thing (all) (nums) (syms) (x) (y) (klass))

;;; columns -------------------------------------
(defmethod header ((i cols) headers)
  (with-slots (all nums syms x y klass) i
    (doitems (txt pos (reverse headers))
      (let ((new (make-instance 
		   (if (num? txt) 'num 'sym)
		   :txt txt :pos pos
                   :w (if (less? txt) -1 1))))
	(if (num?  txt) (push new nums) (push new syms))
	(if (goal? txt) (push new y)    (push new x))
	(push new all)
	(if (klass? txt)
	  (setf klass new))))))

(defmethod row ((i cols) rows cells)
  (make-instance 
    'row 
    :_rows rows
    :cells (mapcar #'add (? i all) cells)))

;;; rows ----------------------------------------
(defmethod use? ((i rows) head &aux out)
  (doitems (txt pos head out)
    (unless (ignore? txt) (push pos out))))

(defmethod use! ((i rows) using row &aux out)
  (dolist (use using out)
     (push (elt row use) out)))

(defmethod take ((i rows) 
		 lst &aux (using (use? i (car lst))))
  (with-slots (cols all) i
    (dolist (tmp lst i)
      (setf tmp (use! i using tmp))
      (if (? cols all)
	(push (row cols i tmp) all)
	(header cols tmp))))
  i)
