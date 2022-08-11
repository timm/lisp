(defmethod it (x) x)
(defmethod it ((x string))
  (let ((y (string-trim '(#\Space #\Tab #\Newline) x)))
    (if (string= y "?") y
      (let ((z (ignore-errors (read-from-string y))))
        (if (numberp z) z y)))))

(defun cli (lst)
  (destructuring-bind (key flag help default) lst
    (let* ((args #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*)
           (it (member flag args :test 'equal)))
      (cons key (cond ((not it)            default)
                      ((equal default t)   nil)
                      ((equal default nil) t)
                      (t                   (it (second it))))))))

(defun chars (x) (if (symbolp x) (symbol-name x) x))
(defun char0 (x) (char (chars x) 0))
(defun charn (x) (let ((y (chars x))) (char y (1- (length y)))))

(defun reads (file fun)
  (with-open-file (s file)
    (loop (funcall fun (or (read s nil) (return-from reads))))))

(defmacro ? (s x &rest xs)
 (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))

(defmacro def (x &body body) 
  (let* ((lst    (mapcar    (lambda (x) (if (consp x) (car x) x))          body))
         (public (remove-if (lambda (x) (eq #\_ (char (symbol-name x) 0))) lst)))
    `(progn
       (defstruct (,x (:constructor ,(intern (format nil "%MAKE-~a" x)))) ,@body)
       (defmethod print-object ((self ,x) str)
         (labels ((fun (y) (format nil ":~(~a~) ~a" y (slot-value self y))))
           (format str "~a" (cons ',x (mapcar #'fun ',public))))))))
