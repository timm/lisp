; vim: filetype=lisp ts=2 sw=3 sts=2 et :
(defmacro reader (com fun)
  (let ((fun1 (gensym)))
    `(progn (defun ,fun1 (stream char)
              (declare (ignore char))
              (,fun (read stream t nil t)))
            (set-macro-character ,com #',fun1))))

(defmacro ? (s x &rest xs) 
  (if xs `(? (slot-value ,s ',x) ,@xs) `(slot-value ,s ',x)))

(defmacro aif (test yes &optional no) 
  "anaphoric if"
  `(let ((it ,test)) (if it ,yes ,no)))

(defun funs (package)
  "Retrieves all functions in a package."
  (when (find-package package)
    (let ((res (list)))
      (do-all-symbols (sym package)
        (when (and (fboundp sym)
                   (eql (symbol-package sym)
                        (find-package package)))
          (push sym res)))
      res)))

(defun halt (&optional (status 0)) (sb-ext:exit :code status))


