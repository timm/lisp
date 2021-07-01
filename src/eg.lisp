; vim: noai ts=2 sw=2 et: 
(load "etc")
(defpackage :espy-test-suite 
  (:use :cl)  (:nicknames :eg)
  (:import-from :etc :todo :? :whale :srand :randf :randi
                     :rnd :want :aif :it :a))
(in-package :eg)

;-------------------------------------
(defvar *tests* nil)
(defvar *fails* 0)

(defmacro deftest (name params  doc  &body body)
  `(progn (pushnew  ',name *tests*) 
          (defun ,name ,params ,doc ,@body)))

(defun demos (my &optional what)
  (dolist (one *tests*)
    (let ((doc (documentation one 'function)))
    (when (or (not what) (eql one what))
      (etc::srand (? my :rand :seed))
      (multiple-value-bind (_ err)
         (ignore-errors (funcall one (etc::deepcopy my)))
         (incf *fails* (if err 1 0))
         (if err
           (format t "~&~a [~a] ~a ~a~%" 
             (etc::red "✖") one doc (etc::yellow err))
           (format t "~&~a [~a] ~a~%"    
             (etc::green "✔") one doc)))))))

(deftest _aif (_)
  "testing test"
  (aif (- 4 3)
    (want (= it 1) "fail?" 2)))

(deftest _srand (_)
  "random number control with srand"
  (let ((n 100) a b)
    (srand)
    (setf a (loop for x below  n collect (randi 1000)) )
    (srand)
    (setf b (loop for x below  n collect (randi 1000)) )
    (want  (equal a b) "lists not equal")))

(demos (etc::cli))
(etc::halt *fails*)
