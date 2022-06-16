(defun trims (x) (string-trim '(#\Space #\Tab #\Newline)))
(defun flag? (x) (and (> (length x) 2) (equal "--" (subseq 0 2)) (subseq x 2 0)))

(defun 2lst (s &optional (c #\,) (n 0)  &aux (pos (position c s :start n)))
  "Divide string `s` on character `c`."
  (if pos 
    (cons (subseq s n pos) (strings s c (1+ pos)))
    (list (subseq s n))))

(defun 2atom (x)
  (cond ((string= x "?")     "?")
        ((string= x "true")  t)
        ((string= x "false") nil)
        (t (let ((y (ignore-errors (read-from-string x))))
             (if (numberp y) y x)))))

(defvar *s*  "aa,bb,cc
     dd,ee,gg

     ggg,ccc")

(defmacro has (x a &optional (n  1))
  "A counter, implemented as an association list."
  `(cdr (or (assoc ,x ,a :test #'equal)
            (car (setf ,a (cons (cons ,x 0) ,a))))))

(dolist (lines (strings *s* #\newline))
   (aif (flag? (trims (first lines)))
     (has it (trims (car (last lines))))))
