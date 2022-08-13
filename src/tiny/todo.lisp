(load "lib/strings")
(load "lib/macros")
(defun parseHelp(str)
  (labels ((want       (s)   (and (> (length s) 0) (eq #\- (char s 0))))
           (splits     (s)   (cells s :char #\Space))
           (oneTwoLast (lst) `(,(first lst) ,(read-from-string (second lst)) 
                                            ,(car (last lst)))))
    (mapcar #'oneTwoLast (mapcar #'splits (remove-if-not #'want (lines str))))))

(defun settings1(str)
  (let ((threes (parseHelp str)))
    (list (cons '_helpstr str)
          (cons '_threes threes)
          (mapcar (lambda (lst) (cons (second lst) (third lst))) threes))))

(defun setting (flag.key.default)
  (destructuring-bind (flag key default)flag.key.default
    (let* ((args #+clisp ext:*args* 
                 #+sbcl sb-ext:*posix-argv*)
           (it (member flag args :test 'equalp)))
    ;(print (list key flag default it))
      (cons key (cond ((not it)            default)
                      ((equal default t)   nil)
                      ((equal default nil) t)
                      (t                   (thing (second it))))))))

(defun cl1 (lst) 
  (let ((tmp (mapcar #'setting  (! lst _threes))))
    (if (! tmp help) (print (! lst _helpstr)))
    lst))

(cl1 (settings1 "
ad
as
das
a
a

-h help shoe help nil
-a adas da aas as = 23
-K k asdd as as = t
-s seed asd as a asd = nil
-a asd sad as d = asda
"))
