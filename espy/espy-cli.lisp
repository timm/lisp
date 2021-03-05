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

(defun cli (o)
  (let (arg
        (args (mapcar #'it sb-ext:*posix-argv*))
        (all  (mapcar #'(lambda (x) `((format nil "-~(~a~)" x) . x) (slots o)))))
    (labels (
       (usage (&optional msg) 
          (format t "~&~aOptions: ~{:~(~a~)~^, ~}~%" 
                  (if msg (format nil "[~a] unknown~%" msg) "")
                  (append '(h demos) (slots o))))

       (else ()
          (let ((slot (assoc arg all :test #'equalp)))
            (if slot
              (setf (slot-value o (cdr slot)) (pop args))
              (if (and (stringp arg) (eql #\- (char arg 0)))
                (usage args)))))

      (loop while (setf arg (pop args)) do
            (cond ((equalp arg "-h")     (usage))
                  ((equalp arg "-demos") (mapcar #'funcall (reverse *demos*)))
                  ((equalp arg "-demo" ) (let ((x (pop args)))
                                           (dolist (f (reverse *demos*))
                                             (if (has (string-upcase x) f) 
                                               (funcall f)))))
                  (t (else)))))
      o)))

(defdemo num? ()
  "Testing nums"
  (print (sd (add (make-num) '(1 2 3 4 5 6 6 7 7 8)))))

(espy (cli (make-options)))