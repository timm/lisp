; vim: ts=2 sw=2 sts=2 et
(unless (fboundp 'got) (load "../got"))

(got "macros.lisp")

(defun reads (f &key  (act #'print) (get #'read) (str t))
  "Read  a file, calling 'fn' on each s-expression. "
  (with-open-file (s f)
    (loop for lst = (funcall get s nil)
          while lst do 
          (funcall act lst))))      

(defun para1 (f)
  (with-output-to-string (out)
    (tagbody
       (reads f :get #'read-line :act (lambda (s)
         (if (equalp "" (string-trim '(#\Space #\Tab) s))
             (go exit)
             (format out "~a~%" s))))
        exit)))

(defun lines (x &optional (s (make-string-input-stream x)))
  (aif (read-line s nil)
       (cons it (lines  s))))
