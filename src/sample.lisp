(unless (fboundp 'rand) (load "rand"))

(defstruct sample
  (max 512) (n 0)
  ok   ; true if `has` is currently sorted.
  (has ; 
     (make-array  5 :fill-pointer 0 :adjustable t)))

(defmethod add ((self sample) n)
  "store up to max items. if full, sometimes replace any old"
  (with-slots (has ok n max) self
    (unless (eql x #\?)
      (incf n)
      (cond ((< (length has) max) 
             (setf ok nil) 
             (vector-push-extend x has))
            ((<= (rand) (/ max n)) 
             (setf ok nil) 
             (setf (aref has (rint (length has))) x))))))

(defmethod  holds ((self sample))
  "returns `has`, sorted"
  (unless (num-ok num) (sort (num-has num) #'<))
  (setf (num-ok num) t)
  (num-has num))


