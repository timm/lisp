;; vim: ts=2 sw=2 sts=2 et:
;-------- -------- -------- -------- -------- -------- --------

"### Usage

```bash
cd src/xx
sbcl --script ../lib/readme.lisp > README.md
git add README.md
``` "

(defmacro doread ((it f &optional out 
                      &key (take #'read)) &body body)
  "Iterator for running over files or strings."
  (let ((str (gensym)))
    `(with-open-file (,str f)
       (loop for 
             ,it = (handler-case (funcall ,take ,str)
                   (end-of-file () (loop-finish)))
             while ,it do 
             (progn ,@body))
       ,out)))

(defun args () sb-ext:*posix-argv*)

(defun readme(dir &optional (s t))
  "Generate README.md from all doco strings 
  form all LISP code in a directory."
  ; (format t "~a~%# ~a~%~%~%" 
  ;         (para1 "../../README.md")
  ;         (string-upcase dir))
  (dolist (f (sort (directory "*.lisp") 
                   #'(lambda (x y) (string< (pathname-name x) 
                                            (pathname-name y)))))
    (let ((name (pathname-name f)))
      (format t "~%~%## [~a.lisp](~a.lisp)~%~%" name name)
      (doread (x f)
        (labels
          ((defp   () (member (first x) '(defun 
                                           defmacro defmethod)))
           (fyip   () (stringp x))
           (secret () (char= #\_ (elt (symbol-name (second x)) 0)))
           (docp   () (and    (> (length x) 3)
                              (stringp (fourth x))
                              (not (equal "" (fourth x)))))
           (dump   (str  &optional (pad ""))
                   (format s "~a~a~%" pad str)))
          (when (fyip)
            (format t "~%~%~a~%~%" x)
            )
          (when (and (defp) (docp) (not (secret)))
            (format s "~%`~(~a~) ~(~a~)`~%~%<ul>" 
                    (second x) (or (third x) ""))
            (dump (fourth x) "   ")
            (format s "</ul>~%")))))))

; (let ((cli (args)))
;   (when (and cli (equalp "--makedoc" (first cli)))
;     (print cli)
(readme ".")
  
