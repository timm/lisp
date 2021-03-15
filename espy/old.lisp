(defun slots (x)  
  (mapcar #'sb-mop:slot-definition-name(sb-mop:class-slots (class-of x))))

(defun cli (o)
  (let (arg
        (args (mapcar #'it sb-ext:*posix-argv*))
        (all  (mapcar #'(lambda (x) (cons (format nil "-~(~a~)" x) x)) 
                      (slots o))))
    (labels (
       (usage (&optional msg) 
          (format t "~&~aOptions: ~{-~(~a~)~^, ~}~%" 
                  (if msg (format nil "[~a] unknown~%" msg) "")
                  (append '(h demos) (slots o))))

       (else ()
          (let ((slot (assoc arg all :test #'equalp)))
            (if slot
              (setf (slot-value o (cdr slot)) (pop args))
              (if (and (stringp arg) (eql #\- (char arg 0)))
                (usage args))))))

      (loop while (setf arg (pop args)) do
            (cond ((equalp arg "-h")     (usage))
                  ((equalp arg "-demos") (mapcar #'funcall (reverse *demos*)))
                  ((equalp arg "-demo" ) (let ((x (pop args)))
                                           (dolist (f (reverse *demos*))
                                             (if (has (string-upcase x) f) 
                                               (funcall f)))))
                  (t (else))))
      o)))


