; From Peter Seiblel's excellent text: http://gigamonkeys.com/book
(defparameter *test-name* nil)  
(defparameter *tests* nil)  

(defmacro deftest (name parameters &body body)
  `(progn
     (unless (member ',name *tests*)
       (setf *tests* (append *tests* (list ',name))))
     (defun ,name ,parameters
       (let ((*test-name* (append *test-name* (list ',name))))
	 ,@body))))

(defmacro check (&body forms)
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect
              `(,n (make-symbol ,(string n))))
     ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect 
             `(unless ,f (setf ,result nil)))
      ,result)))

(let ((passes 0) (fails 0))
  
  (defun tests-reset()
    (setf passes 0)
    (setf fails 0))

  (defun tests-report ()
    (format t "~%PASSES: ~a (~a %)~%FAILS : ~a~%"
            passes (* 100 (/ passes (+ passes fails)))
            fails))

 (defun report-result (result form)
    (if result
        (and (incf passes) 
             (format t "."))
        (and (incf fails) 
             (format t "~%fail ... ~a: ~a~%"  *test-name* form)))
    result)
  )

(defun tests ()
  (make) 
  (tests-reset) 
  (dolist (one *tests*)
    (if (fboundp one)
	(funcall one)
	(format t "; unknown test: [~a]~%" one)))
  (tests-report))
