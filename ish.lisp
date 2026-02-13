#!/usr/bin/env sbcl --script
#+sbcl (declaim (sb-ext:muffle-conditions cl:style-warning))

(defmacro o (x &rest f) (dolist (s f x) (setf x `(slot-value ,x ',s))))

(defvar +it+  '(seed 1 file "autos.lisp"))
(defmacro ? (x) `(getf +it+ ',x))

defun s->atom (s &aux (s1 (string-trim '(#\Space #\Tab) s))) 
  (let ((x (let ((*read-eval* nil)) (read-from-string s1 ""))))
    (if (or (numberp x) (member x '(t nil ?))) x s1)))

(defun mapcsv (fun file)
  (with-open-file (s (or file *standard-input*))
    (loop (funcall fun (s->list (or (read-line s nil nil) 
                                    (return)))))))

(print (csv (? file) #'print))
