;(defpackage :sublime (:use :cl))
;(in-package :sublime)

;;;; bootstrap ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct options
  (help
    "sbcl --noinform --script expose.lisp [OPTIONS]
(c) 2022, Tim Menzies, MIT license

Lets have some fun.")
  (options
   (list 
    (cli! 'enough  "-e" "enough items for a sample" 512)
    (cli! 'far     "-F" "far away                 " .9)
    (cli! 'file    "-f" "read data from file      " "../data/auto93.csv")
    (cli! 'help    "-h" "show help                " nil)
    (cli! 'license "-l" "show license             " nil)
    (cli! 'p       "-p" "euclidean coefficient    " 2)
    (cli! 'seed    "-s" "random number seed       " 10019)
    (cli! 'todo    "-t" "start up action          " ""))))

(defmethod print-object ((c cli) s)
  (with-slots (key flag help value) c 
    (format s "   ~5a  ~a " flag  help)
    (if (member value '(t nil)) (terpri s) (format s "= ~a~%" value))))

(defmethod print-object ((o options) s)
  (with-slots (help options) o
    (format s "~a~%~%OPTIONS:~%" help)
    (dolist (x options) (print-object (cdr x) s)))

(defun item (x)
  "Return a number or a trimmed string."
  (cond ((numberp x)   x)
        ((equal x "?") nil)
        (t (let ((y (ignore-errors (read-from-string x))))
             (if (numberp y) y x)))))

(defun cli! (key flag help value)
  (let* ((args (cdr sb-ext:*posix-argv*))
         (it   (member flag args :test #'equal)))
    (if it (setf value (cond ((equal it t)   nil)
                             ((equal it nil) t)
                             (t (item (second it))))))
    (cons key (%make-cli :key key :flag flag :help help :value value))))

(defvar *the* (make-options))

;;;; lib 
;;; tricks
(defmacro aif (test y &optional n) `(let ((it ,test)) (if it ,y ,n)))
(defmacro ? (p x &rest xs) (if (null xs) `(getf ,p ,x) `(? (getf ,p ,x) ,@xs)))
;; misc                                                           ;;;;     ;;;;;

(defun struct->alist (x xs) (mapcar (lambda (s) (cons s (slot-value x s))) xs))

(defvar *seed* 10013)
(defun randi (&optional (n 1)) (floor (* n (/ (randf 1000.0) 1000))))
(defun randf (&optional (n 1.0)) 
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(dotimes (i 10) (print (randf )))

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
       (defmethod print-object ((,it ,x) s)
         (print-object (cons ',x (struct->alist ,it ',(names))) s)))))

;;;; my structs
;;; my things
(defthing num    at pos n w mu m2 sd)
(defthing sym    at pos n seen mode most)
(defthing cols   all x y klass)
(defthing sample rows cols)

;;;; classes
;;;  cli

;;;  our
(setf *the* (
(defmacro $ (x) `(cdr (assoc ',x (our-options *the*))))



(defun make-num () (%make-num))

;;;; coerce


(defun str->items (s &optional (c #\,) (n 0) &aux (pos (position c s :start n)))
  "Divide string `s` on character `c`."
  (if pos
      (cons (item (subseq s n pos)) (str->items s (1+ pos)))
      (list (item (subseq s n)))))

(defun %csv (file &optional (fn 'print))
  "Run a function `fn` over file (sub-function of `with-csv`)."
  (with-open-file (str file)
                  (loop (funcall fn (or (read-line str nil) (return-from %csv))))))

(defmacro with-csv ((lst file &optional out) &body body)
  `(progn (%with-csv ,file (lambda (,lst) ,@body)) ,out))

;;;; tests          |                   |                   |                   |
(defvar *tests* nil)
(defvar *fails* 0)

(defmacro deftest (name params  doc  &body body)
  `(progn (pushnew ',name *tests*) (defun ,name ,params ,doc ,@body)))

(defun demos (my &optional what)
  (dolist (one *tests*)
    (let ((doc (documentation one 'function)))
      (when (or (not what) (eql one what))
        (setf *seed* (! seed))
        (multiple-value-bind
         (_ err)
         (ignore-errors (funcall one (deepcopy my)))
         (incf *fails* (if err 1 0))
         (if err
             (format t "~&FAIL: [~a] ~a ~a~%" one doc  err)
           (format t "~&PASS: [~a] ~a~%" one doc)))))))


;(defun file2sample (file &aux ((s (make-sample))))
;;;; lib
;;; lists

(defun make () (load "sublime.lisp"))


                                        ; file to samples
                                        ; samples to clusters
                                        ; clusters to ranges
                                        ; ranges to tree
