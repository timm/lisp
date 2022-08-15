; Update `default` from command line.  Boolean flags just flip defaults.
(defun setting (key.flag.help.default)
  (destructuring-bind (key flag help default) key.flag.help.default
    (let* ((args #+clisp ext:*args* 
                 #+sbcl sb-ext:*posix-argv*)
           (it (member flag args :test 'equalp)))
      (cons key (cond ((not it)            default)
                      ((equal default t)   nil)
                      ((equal default nil) t)
                      (t                   (thing (second it))))))))

; Update settings. If  `help` is set, print help.
(defun settings (header options)
  (let ((tmp (mapcar #'setting options)))
    (when (! tmp help)
      (format t "~&~{~a~%~}~%OPTIONS:~%" (lines header))
      (dolist (one options)
        (format t "  ~a   ~a = ~a~%" (second one) (third one) (fourth one))))
    tmp))
