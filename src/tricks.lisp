; vim: filetype=lisp ts=2 sw=3 sts=2 et :

; ## Macros

; After calling `(reader #\c fun)`, then if
; ever we see c(sex) then (sexp) will be  
; passes to `fun`.
(defmacro reader (com fun)
  (let ((fun1 (gensym)))
    `(progn 
       (defun ,fun1 (stream char)
         (declare (ignore char))
         (,fun (read stream t nil t)))
       (set-macro-character ,com #',fun1))))

; Shorthand for recursive calls to slot-value.
(defmacro ? (s x &rest xs) 
  (if xs `(? (slot-value ,s ',x) ,@xs) `(slot-value ,s ',x)))

; Anaphoric if.
(defmacro aif (test yes &optional no) 
  `(let ((it ,test)) (if it ,yes ,no)))

; Returns all functions in a package.
(defun funs (package)
  (when (find-package package)
    (let ((res (list)))
      (do-all-symbols (sym package)
        (when (and (fboundp sym)
                   (eql (symbol-package sym)
                        (find-package package)))
          (push sym res)))
      res)))

; Exit LISP.
(defun halt (&optional (status 0)) 
  (sb-ext:exit :code status))


