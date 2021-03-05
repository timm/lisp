; vim: noai:ts=2:sw=2:et: 
(handler-bind 
  ((style-warning #'muffle-warning)) 
  (load "espy"))

(in-package :espy)

(defvar *demos* nil)
(defvar *fails* 0)

(defmacro defdemo (name args &optional (doc "") &body body)
  (pushnew name *demos*)
  `(defun ,name ,args ,doc
     (format t "~&~%;;; ~a~%; ~a~%" ',name ,doc) ,@body))

(defmacro ok (x y &optional (msg "") &rest txt)
  `(handler-case
     (if (not (equalp ,x ,y)) (error (format nil ,msg ,@txt)))
     (t  (c)                  (format t "; E[~a]> ~a~%"  (incf *fails*) c))))

(defmethod cli ((o options) act &aux (args (mapcar #'it sb-ext:*posix-argv*)))
  (loop while args do (cli1 o (pop args) args))
  (funcall act o)
  (sb-ext:exit :code (if (< *fails* 2) 0 1)))

(defmethod usage ((o options) &optional missing) 
  (format t "~&~aOptions: ~{:~(~a~)~^, ~}~%" 
          (if missing (format nil "[~a] unknown~%" missing) "")
          (append '(h demos) (slots o))))

(defmethod cli1 ((o options) arg args)
  (cond ((equalp arg "-h")     (usage o))
        ((equalp arg "-demos") (mapcar #'funcall (reverse *demos*)))
        ((equalp arg "-sep")   (setf (? o sep)  (pop args)))
        ((equalp arg "-keep")  (setf (? o keep) (pop args)))
        ((equalp arg "-data")  (setf (? o data) (pop args)))
        ((equalp arg "-dir" )  (setf (? o dir)  (pop args)))
        ((equalp arg "-demo" ) 
         (let ((goal (pop args)))
           (dolist (f (reverse *demos*))
             (if (has (string-upcase goal) f) (funcall f)))))
        ((and (stringp arg) (eql #\- (char arg 0)) (usage o arg)))))

(defdemo num? ()
  "Testing nums"
  (print (sd (add (make-num) '(1 2 3 4 5 6 6 7 7 8)))))


(cli (make-options) #'espy) 
