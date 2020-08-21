(defclass bank-account ()
  ((customer-name
     :initarg :customer-name)
   (balance
     :initarg :balance
     :initform 0)))

(let ((x  (make-instance 'bank-account :balance 20)))
  (print (slot-value x 'balance)))
