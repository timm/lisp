(defun trim (x) (string-trim '(#\Space #\Tab #\Newline) x))

(defmethod it (x) x)
(defmethod it ((x string))
  (let ((y (trim x)))
    (if (string= y "?") y
      (let ((z (ignore-errors (read-from-string y))))
        (if (numberp z) z y)))))

(defun splits (string &key (sep #\,) (filter #'identity))
  (loop for start = 0 then (1+ finish)
    for        finish = (position sep string :start start)
    collecting (funcall filter (trim (subseq string start finish)))
    until      (null finish)))

(defun lines (string) (splits string :sep #\Newline))
(defun cells (string) (splits string :filter #'it))

(defun cli (lst)
  (destructuring-bind (key flag help default) lst
    (let* ((args #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*)
           (it (or (member flag args :test 'equal)
                   (member key  args  :test 'equal))))
      (cons key (cond ((not it)            default)
                      ((equal default t)   nil)
                      ((equal default nil) t)
                      (t                   (it (second it))))))))

(defun settings (header options)
  (let ((tmp (mapcar #'cli options)))
    (when (cdr (assoc 'help tmp))
     (format t "~&~%~{~a~%~}~%OPTIONS:~%" (lines header))
     (dolist (one options)
       (format t "  ~a   ~a = ~a~%" (second one) (third one) (fourth one))))
    tmp))

(defun chars (x) (if (symbolp x) (symbol-name x) x))
(defun char0 (x) (char (chars x) 0))
(defun charn (x) (let ((y (chars x))) (char y (1- (length y)))))

(defun read-lines (file fun)
  (with-open-file (s file)
    (loop (funcall fun (or (read-line s nil) (return))))))

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
