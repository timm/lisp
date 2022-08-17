(defun reads (f fun)
  (with-open-file (s f)
    (loop (funcall fun (or (read s nil nil) (return))))))

(defun readme(&optional (s t))
  "Generate README.md from all doco strings
  form all LISP code in a directory."
  ;(format t "~a~%# ~a~%~%~%" (para1 "../../README.md") (string-upcase dir))
  (dolist (f (sort (directory "*.lisp")
                   #'(lambda (x y) (string< (pathname-name x)
                                            (pathname-name y)))))
    (let ((name (pathname-name f)))
      (format t "~%~%## [~a.lisp](~a.lisp)~%~%" name name)
      (reads f (lambda (x)
        (labels
          ((lisps(x) (format nil "~%~%```lisp~%~(~a~)~%```~%~%" x))
           (details (x) (format nil "~%~%<details><summary>(code)<summary>~a</details>" x))
           (defp   () (member (first x) '(defun defmacro defmethod)))
           (secret () (char= #\_ (elt (symbol-name (second x)) 0)))
           (docp   () (and    (> (length x) 3)
                              (stringp (fourth x))
                              (not (equal "" (fourth x)))))
           (dump   (str  &optional (pad ""))
                   (format s "~a~a~%" pad str)))
          (when (and (defp) (docp) (not (secret)))
            (format s "~%`(~(~a~) ~(~a~))`~%~%<ul>"
                    (second x) (or (third x) ""))
            (dump (fourth x) "   ")
            (format s "</ul>~%")
            (format s "~a" (details (lisps (append (subseq x 0 3) (cddddr x)))))
            )))))))

(readme)


