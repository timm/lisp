; Update `default` from command line.  Boolean flags just flip defaults.
(defun cli (key.flag.help.default)
  "If `flag` exists on command line, update `key`."
  (destructuring-bind (key flag help default) key.flag.help.default
    (declare (ignore help))
    (let* ((args #+clisp ext:*args* 
                 #+sbcl sb-ext:*posix-argv*)
           (it (member flag args :test 'equalp)))
      (cons key (cond ((not it)            default)
                      ((equal default t)   nil)
                      ((equal default nil) t)
                      (t                   (thing (second it))))))))

(defun settings (header options)
  "Update settings. If  `help` is set, print help."
  (let ((tmp (mapcar (lambda (x) (cli x)) options)))
    (when (! tmp help)
      (format t "~&~{~a~%~}~%OPTIONS:~%" (lines header))
      (dolist (one options)
        (destructuring-bind (flag help default) (cdr one) 
           (format t "  ~a   ~a = ~a~%" flag help default))))
    tmp))
