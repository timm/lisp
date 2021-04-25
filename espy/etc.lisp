; vim: et ts=2 sts=2 sw=2 :

(defun klass-slots (it) (sb-mop:class-slots (class-of it)))
(defun slot-name1 (x) (sb-mop:slot-definition-name x))

(defstruct a 
  (b '(1 "b things")) 
  (k '(nil  "b things")) 
  (c '(2 "c stuff")) 
  (d '(333 "about d")))

(defun as(s) (or (read-from-string s) s))

(defun cli (b4 &optional (usage ""))
  (let* (out
          (keys (mapcar #'slot-name1 (klass-slots b4)))
          (args (cdr sb-ext:*posix-argv*)))
    (labels ((init (s) (dolist (key keys s) 
                         (setf (slot-value s key) (car (slot-value b4 key)))))
             (help () (format t "~&~%~a~%~%USAGE:~%~%" usage)
                   (dolist (key keys)
                     (let ((x (slot-value b4 key)))
                       (format t " ~a~(~10a~) ~a (default=~a)~%" 
                               (if (car x) "-" "+") key (cadr x) (car x)))))
             (keep (it pre)
                 (if (equal pre "-") (setf (slot-value out it) (as (pop args))))
                 (if (equal pre "+") (setf (slot-value out it) t)))
             (work (arg) 
                   (if (null arg) 
                     (return-from cli out)
                     (let* ((pre (subseq arg 0 1))
                            (it  (intern (string-upcase (subseq arg 1)))))
                       (cond ((equalp arg "-h") (help))
                             ((member it keys)  (keep it pre))
                             (t (format t "W> ~(~a~)?~%" arg)))))))
      (setf out (init (make-instance (type-of b4))))
      (loop (work (pop args))))))

(print (cli (make-a) "./lisp etc.lisp" ))
