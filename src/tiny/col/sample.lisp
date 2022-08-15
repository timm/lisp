; Keep up to "max" numbers (after which, replace any old with new).
(defstruct+ sample 
  (_kept ; where to keep
           (make-array 2 :fill-pointer 0 :adjustable t)) 
  (n 0)
  max    ; how many to keep
  ok)    ; nil if items added and list not resorted yet

(defun make-sample (&optional (max (! my keep))) 
  (%make-sample :max max))

(defmethod add ((i sample) (x number))
  (incf (? i n))
  (let ((size (length (? i _kept))))
    (cond ((< size  (? i max)) 
           (setf (? i ok) nil)
           (vector-push-extend x (? i _kept)))
          ((< (randf) (/ (? i n) (? i max)))
           (setf (? i ok) nil)
           (setf (elt (? i _kept) (randi size)) x)))))

(defmethod per ((i sample) p)
  (let* ((all (sorted i))
         (n  (1- (length all))))
    (elt all (max 0 (min n (floor (* p n)))))))

(defmethod mid ((i sample)) (per i .5))

(defmethod div ((i sample)) (/ (- (per i .9) (per i .1)) 2.58))

(defmethod sorted ((i sample))
  (unless (? i ok) 
    (sort (? i _kept) #'<)
    (setf (? i ok) t))
  (? i _kept))
