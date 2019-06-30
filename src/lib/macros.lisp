;; vim: set ts=2 sts=2 et :
(unless (fboundp 'got) (load "../got"))

(defmacro af (test then &optional else)
  "Anaphoric 'if'"
  `(let ((a ,test))
     (if a ,then ,else)))

(defmacro while (test &body body)
  "implements 'while' (which is not standard in LISP)"
  `(do ()
       ((not ,test))
     ,@body))

(defmacro whale (test &body body)
  "implements 'while' (which is not standard in LISP)"
  `(let (a)
     (while (setf a ,test)
       ,@body)))

(defmacro until (test &body body)
  "implements 'until' (which is not standard in LISP)"
  `(while (not ,test)
     ,@body))

(defmacro doitems ((one n list &optional out) &body body )
  "Set 'one' and 'n' to each item in a list, and its position."
  `(let ((,n -1))
     (dolist (,one ,list ,out)
       (incf ,n)
       ,@body)))
