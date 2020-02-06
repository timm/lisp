;; vim: ts=2 sw=2 sts=2 et:
;-------- -------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(got "sys.lisp" "lib/reads.lisp")

(fyi "### Usage

```bash
cd src/xx
sbcl --script ../lib/readme.lisp > README.md
git add README.md
``` ")

(defun readme(dir &optional (s t))
  "Generate README.md from all doco strings 
  form all LISP code in a directory."
  (format t "~a~%# ~a~%~%~%" 
          (para1 "../../README.md")
          (string-upcase dir))
  (dolist (f (sort (directory "*.lisp") 
                   #'(lambda (x y) (string< (pathname-name x) 
                                            (pathname-name y)))))
    (let ((name (pathname-name f)))
      (format t "~%~%## [~a.lisp](~a.lisp)~%~%" name name)
      (doread (x f)
        (labels
          ((defp   () (member (first x) '(deftest defun 
                                           defmacro defmethod)))
           (fyip   () (eql    (first x)  'fyi))
           (secret () (char= #\_ (elt (symbol-name (second x)) 0)))
           (docp   () (and    (> (length x) 3)
                              (stringp (fourth x))
                              (not (equal "" (fourth x)))))
           (dump   (str  &optional (pad ""))
                   (format s "~a~a~%" pad str)))
          (when (fyip)
            (terpri s) (terpri s)
            (dump (second x))
            (terpri s) (terpri s)
            )
          (when (and (defp) (docp) (not (secret)))
            (format s "~%`~(~a~) ~(~a~)`~%~%<ul>" 
                    (second x) (or (third x) ""))
            (dump (fourth x) "   ")
            (format s "</ul>~%")))))))

(let ((cli (args)))
  (if (and cli (equalp "--makedoc" (first cli)))
    (readme (second cli))))
  
