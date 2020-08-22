; vim: noai:ts=2:sw=2:et: 
(load "got")
(got "oo" "strings" "col")

(defun ignore? (x &aux (n (elt x 0))) 
  (eql n (my ch skip)))

(defun klass? (x &aux (n (elt x 0))) 
  (eql n (my ch klass)))

(defun goal? (x &aux (n (elt x 0)))
  (or (eql n (my ch less))
      (eql n (my ch more))
      (eql n (my ch klass))))

(defun num? (x &aux (n (elt x 0)))
  (or (eql n (my ch num))
      (eql n (my ch less))
      (eql n (my ch more))))

;-------- --------- --------- --------- --------- ----------
(defthing cols thing (all) (nums) (syms) (x) (y) (klass))

(defmethod header ((i cols) headers)
  (with-slots (all nums syms x y klass) i
    (doitems (txt pos (reverse headers))
      (let ((new (make-instance 
		   (if (num? txt) 'num 'sym)
		   :txt txt :pos pos)))
	(push new (if (num?  txt) nums syms))
	(push new (if (goal? txt) y    x))
	(push new all)
	(if (klass? txt)
	  (setf klass new))))))

(defmethod row ((i cols) rows cells)
  (make-instance 
    'row 
    :_rows rows
    :cells (mapcar #'add (? i all) cells)))

;-------- --------- --------- --------- --------- ----------
(defthing row thing (cells) (_rows))

;-------- --------- --------- --------- --------- ----------
(defthing rows thing (all) (cols (make-instance 'cols)))

(defmethod use? ((i rows) head &aux todo)
  (doitems (txt pos head todo)
    (unless (ignore? txt) 
      (push pos todo))))

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

