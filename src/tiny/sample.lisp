; Keep up to "max" numbers (after which, replace any old with new).
(defstruct+ sample 
  (_kept ; where to keep
            (make-array 2 :fill-pointer 0 :adjustable t)) 
  max    ; how many to keep
  ok)    ; nil if items added and list not resorted yet

(defun make-sample (&optional (max (? my keep_))) (%make-sample :max max))

(defmethod add ((i sample) (x number))
  (incf (? i n))
  (let ((size (length (? i _kept))))
    (cond ((< size  (? i max)) 
           (setf (? i ok) nil)
           (vector-push-extend x (? i _kept)))
          ((< (randf) (/ (? i n) (? i max)))
           (setf (? i ok) nil)
           (setf (elt (? i _kept) (randi size)) x)))))

(defmethod has ((i sample))
  (unless (? i ok) 
    (sort (? i _kept) #'<)
    (setf (? i ok) t))
  (? i _kept))
