(defstruct+ sample 
  (kept (make-array 2 :fill-pointer 0 :adjustable t)) ; where to keep
  (max (? my keep)) ; how many to keep
  ok)               ; nil if items added and list not resorted yet

(defmethod add ((i sample) (x number))
  (incf (? i n))
  (let ((size (length (? i kept))))
    (cond ((< size  (? i max)) 
           (setf (? i ok) nil)
           (vector-push-extend x (? i kept)))
          ((< (randf) (/ (? i n) (? i max)))
           (setf (? i ok) nil)
           (setf (elt (? i kept) (randi size)) x)))))

(defmethod has ((i sample))
  (unless (? i ok) 
    (sort (? i kept) #'<)
    (setf (? i ok) t))
  (? i kept))
