;(defpackage :sublime (:use :cl))
;(in-package :sublime)

                                        ; file to samples
                                        ; samples to clusters
                                        ; clusters to ranges
                                        ; ranges to tree

(defun make () (load "sublime.lisp"))

(defun config()
  (make-our 
  :help "sbcl --noinform --script expose.lisp [OPTIONS]
(c) 2022, Tim Menzies, MIT license

Lets have some fun."
  :options (list 
  (make-cli 'enough  "-e" "enough items for a sample" 512)
  (make-cli 'far     "-F" "far away                 " .9)
  (make-cli 'file    "-f" "read data from file      " "../data/auto93.csv")
  (make-cli 'help    "-h" "show help                " nil)
  (make-cli 'license "-l" "show license             " nil)
  (make-cli 'p       "-p" "euclidean coefficient    " 2)
  (make-cli 'seed    "-s" "random number seed       " 10019)
  (make-cli 'todo    "-t" "start up action          " ""))))

;;;; lib 
;;; tricks
;; misc
(defmacro ?   (p x &rest xs) (if (null xs) `(getf ,p ,x) `(? (getf ,p ,x) ,@xs)))
(defmacro !   `(fourth (
(defmacro aif (? y &optional n) `(let ((it ,?)) (if it ,y ,n)))

(defun args   ()     (cdr sb-ext:*posix-argv*))
(defun 2alist (x xs) (mapcar (lambda (s) (cons s (slot-value x s))) xs))
(defun stop   (out)  (sb-ext:exit :code out))

(defun srand (&optional (n 10013))  
  (setf *seed* n))

(defun randi (&optional (n 1)) (floor (* n (/ (randf 1000.0) 1000))))
(defun randf (&optional (n 1.0)) 
  (setf xx (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0)))))

(defun nshuffle (lst)
  "Return a new list that randomizes over of lst"
  (let ((tmp (coerce lst 'vector)))
    (loop for i from (length tmp) downto 2
          do (rotatef (elt tmp (random i)) (elt tmp (1- i))))
    (coerce tmp 'list)))

;; thing
(defmacro defthing (x &rest slots &aux (id (gensym)) (it (gensym)))
  "Defines structs with uniq ids `_id` and a constuctor `(%make-x)`
   and a print method that hides privates slots (those starting with `_`)."
  (labels ((hidep (z) (equal (char (symbol-name z) 0) #\_))
           (name  (z) (if (consp z) (car z) z))
           (names ()  (remove-if #'hidep (mapcar #'name slots)))
           (%make ()  (intern (format nil "%MAKE-~a" (symbol-name x)))))
    `(let ((,id 0))
       (defstruct (,x  (:constructor ,(%make))) (_id (incf ,id)) ,@slots)
       (defmethod print-object ((,it ,x) out)
         (print-object (cons ',x (2alist ,it ',(names))) out)))))

;;;; my structs
;;; my things
(defthing out    help options)
(defthing cli    key flag help value)
(defthing num    at pos n w mu m2 sd)
(defthing sym    at pos n seen mode most)
(defthing cols   all x y klass)
(defthing sample rows cols)

;;;; classes
;;;  cli
(defun make-cli (key flag help value)
  (aif (member flag (args) :test #'equal)
       (setf value (cond ((equal it t)   nil)
                         ((equal it nil) t)
                         (t              (item (second it))))))
  (cons key (%make-cli :key key :flag flag :help help :value value)))

(defun print-object ((c cli) out)
  (with-slots (flag help value) c
    (format out "   ~5a  ~a " flag help)
    (if (member value '(t nil)) (terpri out) (format out "= ~a~%" value))))

;;;  our   
(defmacro $ (x) `(cdr (assoc ',x (our-options *the*))))

(defun print-objects ((o our) out)
  (format out "~a~%~%OPTIONS:~%" (our-help o))
  (dolist (x (our-options o)) (print-object (cdr x) out)))


(defun make-num () (%make-num))

;;;; coerce
(defun item (x)
  "Return a number or a trimmed string."
  (cond ((numberp x) x)
        ((equal x "?") nil)
        (t (let ((y (ignore-errors (read-from-string x))))
             (if (numberp y) y x))))) 

(defun str2items (s &optional (c #\,) (n 0) &aux (pos (position c s :start n)))
  "Divide string `s` on character `c`."
  (if pos
      (cons (item (subseq s n pos)) (str2items s (1+ pos)))
      (list (item (subseq s n)))))

(defun %csv (file &optional (fn 'print))
  "Run a function `fn` over file (sub-function of `with-csv`)."
  (with-open-file (str file)
    (loop (funcall fn (or (read-line str nil) (return-from %csv))))))

(defmacro with-csv ((lst file &optional out) &body body)
  `(progn (%with-csv ,file (lambda (,lst) ,@body)) ,out))


(defvar *tests* nil)
(defvar *fails* 0)

; ## Unit  Tests
; ### deftest
; Add a function to the `\*tests\*`.
(defmacro deftest (name params  doc  &body body)
  `(progn (pushnew  ',name *tests*) 
          (defun ,name ,params ,doc ,@body)))

; ### demos
; Run the `\*tests\*`.
(defun demos (my &optional what)
  (dolist (one *tests*)
    (let ((doc (documentation one 'function)))
    (when (or (not what) (eql one what))
      (srand (? my :rand :seed))
      (multiple-value-bind (_ err)
         (ignore-errors (funcall one (deepcopy my)))
         (incf *fails* (if err 1 0))
         (if err
           (format t "~&~a [~a] ~a ~a~%" 
             (red "✖") one doc (yellow err))
           (format t "~&~a [~a] ~a~%"    
             (green "✔") one doc)))))))


;(defun file2sample (file &aux ((s (make-sample))))
;;;; lib
;;; lists

