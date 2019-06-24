; vim: ts=2 sw=2 sts=2  et
(unless (fboundp 'got) (load "../got"))

(got "reads.lisp")

(defparameter +header+ "

<a href=\"https://git.io/gotlisp\"><img src=\"https://raw.githubusercontent.com/timm/lisp/master/etc/img/gotlisp.png\"><br>
[home](http://git.io/gotlisp) | [contrib](https://github.com/timm/lisp/blob/master/CONTRIBUTING.md) | [discuss](https://github.com/timm/lisp/issues) 

# Documentation

")

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
         (x s)
         "Takes the function documentation string and
         prints it, indented by a little white space"
         (labels 
           ((defp     () (member (first x) '(defun defmacro defmethod)))
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
         (format t "~%~%## ~a.lisp~%~%" name)
        (reads f #'fundoc)))))

(format t "~a"  +header+)
(terpri)
(readme)
