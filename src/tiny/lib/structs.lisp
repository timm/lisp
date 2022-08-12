; Creates %x for constructor, enables pretty print, hides slots with "_" prefix.
(defmacro defstruct+ (x &body body) 
	(let* ((slots  (mapcar    (lambda (x) (if (consp x) (car x) x))          body))
				 (public (remove-if (lambda (x) (eq #\_ (char (symbol-name x) 0))) slots)))
		`(progn
			 (defstruct (,x (:constructor ,(intern (format nil "%MAKE-~a" x)))) ,@body)
			 (defmethod print-object ((self ,x) str)
				 (labels ((fun (y) (format nil ":~(~a~) ~a" y (slot-value self y))))
					 (format str "~a" (cons ',x (mapcar #'fun ',public))))))))
