; vim: ts=2 sw=2 sts=2  et
(unless (fboundp 'got) (load "../got"))

(got "sys.lisp" "reads.lisp")

(defparameter +header+ 
  (format nil "~a~%# ~a~%~%" 
          (para1 "../../README.md")
          (string-upcase (first (args)))))

(defun garnish (&rest x) x)

(garnish "

### Usage

```bash
cd src/xx
sbcl --script ../lib/readme.lisp > README.md
git add README.md
```

")

(defun readme()
  "Generate README.md from all doco strings 
   form all LISP code in a directory."
  (let (name)
    (labels 
      ((fundoc 
         (x &optional (s t))
         "Takes the function documentation string and
         prints it, indented by a little white space"
         (labels 
           ((defp     () (member (first x) '(deftest defun 
                                              defmacro defmethod)))
            (garnishp () (eql    (first x)  'garnish))
            (secret   () (char= #\_ (elt (symbol-name (second x)) 0)))
            (docp     () (and    (> (length x) 3)
                                 (stringp (fourth x))
                                 (not (equal "" (fourth x)))))
            (dump (str  &optional (pad ""))
                  (dolist (line (string-lines str))
                    (format s "~a~a~%" pad (string-trim " ;" line)))))
           (when (garnishp)
             (terpri s)
             (dump (second x))
             (terpri s))
           (when (and (defp) (docp) (not (secret)))
             (format s "~%`~(~a~) ~(~a~)`~%~%-" (second x) (or (third x) ""))
             (dump (fourth x) "   ")))))

      (dolist (f (sort (directory "*.lisp") 
                       #'(lambda (x y) (string< (pathname-name x) 
                                                (pathname-name y)))))
        (setf name (pathname-name f))
         (format t "~%~%## [~a.lisp](~a.lisp)~%~%" name name)
        (reads f :act #'fundoc)))))

(format t "~a"  +header+)
(terpri)
(readme)
