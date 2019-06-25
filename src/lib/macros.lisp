; vim: ts=2 sw=2 sts=2  et
(unless (fboundp 'got) (load "../got"))

(defmacro aif (test then &optional else)
  "Anaphoric 'if'"
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro while (test &body body)
  "implements 'while' (which is not standard in LISP)"
  `(do ()
       ((not ,test))
     ,@body))

(defmacro until (test &body body)
  "implements 'until' (which is not standard in LISP)"
  `(while (not ,test)
     ,@body))
