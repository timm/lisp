
(defun klass-slots (it) (sb-mop:class-slots (class-of it)))
(defun slot-name1 (x) (sb-mop:slot-definition-name x))

(defun is (x y) `(,x ,y))
(defstruct a 
  (b (is 1 "b things")) 
  (k (is nil  "b things")) 
  (c (is 2 "c stuff")) 
  (d (is 333 "about d")))

(defun as (s) (or (read-from-string s) s))

(defun cli (b4 &optional (usage ""))
  (let* (arg
          (args  (cdr sb-ext:*posix-argv*))
          (out   (make-instance (type-of b4)))
          (slots (mapcar #'slot-name1 (klass-slots b4))))
    (dolist (slot slots)
      (setf (slot-value out slot) (car (slot-value b4 slot))))
    (loop while (setf arg (pop args)) do
      (if (equalp arg "-h")
        (progn
          (format t "~&~%~a~%~%USAGE:~%~%" usage)
          (dolist (slot slots)
            (let* ((val  (first (slot-value b4 slot)))
                   (help (second (slot-value b4 slot)))
                   (pre  (if val "-" "+")))
              (format t " ~a~(~10a~) ~a (default=~a)~%" pre slot help val))))
        (let ((pre  (subseq arg 0 1))
              (flag (intern (string-upcase (subseq arg 1)))))
          (if (member flag slots)
              (cond 
                ((equal pre "-") (setf (slot-value out flag) (as (pop args))))
                ((equal pre "+") (setf (slot-value out flag) t)))
              (format t "W> ~(~a~)?~%" arg)))))
    out))

(print (cli (make-a) "./lisp etc.lisp" ))
