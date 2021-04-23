
(defun klass-slots (it) (sb-mop:class-slots (class-of it)))
(defun slot-name1 (x) (sb-mop:slot-definition-name x))

(defun is (x y) `(,x ,y))
(defstruct a 
  (b (is 1 "b things")) 
  (k (is nil  "b things")) 
  (c (is 2 "c stuff")) 
  (d (is 333 "about d")))

(defun as(s) (or (read-from-string s) s))

(defun cli (b4 &optional (usage ""))
  (let* (arg out (args  (cdr sb-ext:*posix-argv*))
                 (slots (mapcar #'slot-name1 (klass-slots b4))))
    (labels ((init (s) (dolist (slot slots s)
                           (setf (slot-value s slot) (car (slot-value b4 slot)))))
             (help ()  (format t "~&~%~a~%~%USAGE:~%~%" usage)
                       (dolist (slot slots)
                         (let ((x (slot-value b4 slot)))
                           (format t " ~a~(~10a~) ~a (default=~a)~%" 
                             (if (first x) "-" "+") slot (second x) (first x)))))
             (update (flag pre)
                (if (equal pre "-") (setf (slot-value out flag) (as (pop args))))
                (if (equal pre "+") (setf (slot-value out flag) t))))
      (setf out (init (make-instance (type-of b4))))
      (loop while (setf arg (pop args)) do
            (let* ((pre  (subseq arg 0 1))
                   (flag (intern (string-upcase (subseq arg 1)))))
              (cond ((equalp arg "-h")  (help))
                   ((member flag slots) (update flag pre))
                   (t                   (format t "W> ~(~a~)?~%" arg))))))
      out))

  (print (cli (make-a) "./lisp etc.lisp" ))
