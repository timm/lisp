; Define one demos.
(defvar *demos* nil)
(defmacro defdemo (what arg doc &rest src) 
  `(push (list ',what ',doc (lambda ,arg ,@src)) *demos*))

; Run `one` (or `all`) the demos. Reset globals between each run.
; Return to the operating systems the failure count (so fails=0 means "success").
(defun demos (settings all &optional one)
  (let ((fails 0)
        (resets (copy-list settings)))
    (dolist (trio all)
      (destructuring-bind (what doc fun) trio
        (setf what (format nil "~(~a~)" what))
        (when (member what (list 'all one) :test 'equalp)
          (loop for (key . value) in resets do 
            (setf (cdr (assoc key settings)) value))
          (setf *seed* (or (cdr (assoc 'seed settings)) 10019))
          (unless (eq t (funcall fun ))
            (incf fails)
            (format t "~&FAIL [~a] ~a ~%" what doc)))))
    #+clisp (ext:exit fails)
    #+sbcl  (sb-ext:exit :code fails)))
