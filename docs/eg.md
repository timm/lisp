<img src='http://www.lisperati.com/lisplogo_fancy_256.png' width=200 align=right>


(defvar *tests* nil)

(defvar *fails* 0)

(defmacro deftest (name params doc &body body)
 `(progn (pushnew ',name *tests*)
   (defun ,name ,params ,doc ,@body)))

(defun demos (my &optional what)
 (dolist (one *tests*)
  (let ((doc (documentation one 'function)))
   (when (or (not what) (eql one what))
    (srand (? my :rand :seed))
    (multiple-value-bind (_ err)
     (ignore-errors (funcall one (deepcopy my)))
     (incf *fails* (if err 1 0))
     (if err
      (format t "~&~a [~a] ~a ~a~%" (red "✖") one doc
       (yellow err))
      (format t "~&~a [~a] ~a~%" (green "✔") one doc)))))))

(deftest _aif (_) "testing test"
 (aif (- 4 3) (want (= it 1) "fail?" 2)))

(deftest _srand (_) "random number control with srand"
 (let ((n 100) a b) (srand)
  (setf a (loop for x below n collect (randi 1000))) (srand)
  (setf b (loop for x below n collect (randi 1000)))
  (want (equal a b) "lists not equal")))

(deftest _? (_) "nested plist access"
 (let ((plist '(:a (:b 23 :c 3) :d 4)))
  (want (= 4 (incf (? plist :a :c))) "add one")))

(deftest _rnd (_) "rounding number"
 (want (eql (rnd 3.14157 2) 3.14) "equal"))

(deftest _deepcopy (_)
 (let* ((a '((10) 20 30 (40 50 60))) (b (copy-tree a)))
  (print a) (print b) (want (eql 2 (length (car a))))
  (want (eql 20 (second b)))))

````


; W? DO DOC  FAIL
