;- vim: ts=2 sw=2 et :
(defvar *opt* '(help nil
                seed 10019
                file "../data/auto93.csv"))

(defmacro ?   (x) `(getf *opt* ',x))

(defmacro aif (expr then &optional else) 
  `(let (it) (if (setf it ,expr) ,then ,else)))

(defun trim (x) (string-trim '(#\Space #\Tab) x))

(defun thing (x)
  (if (equal x "?") #\?  (let ((y (ignore-errors (read-from-string x))))
                           (if (numberp y) y x))))

(defun str2list (s &optional (sep #\,) (x 0) (y (position sep s :start (1+ x))))
  (cons (subseq s x y) (and y (str2list s sep (1+ y)))))

(defun cli()
  (labels ((args () #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*))
    (loop for (slot b4) on *opt* by #'cddr do
      (setf (getf *opt* slot) 
       (aif (member (format nil "-~a" slot) (args) :test #'equalp)
         (if (eq b4 t) nil (if (eq b4 nil) t (thing (trim (elt it 1)))))
         b4)))))

(defmacro with-csv ((lst file &optional out) &body body)
  (let ((str (gensym)))
    `(with-open-file (,str ,file)
       (loop while (aif (read-line ,str nil)
                     (setf ,lst (mapcar #'thing (str2list it)))) do ,@body)
       ,out)))

(cli)
(print *opt*)
;(with-csv (row (? file)) (print row))
