<img src='http://www.lisperati.com/lisplogo_fancy_256.png' width=200 align=right>


(defpackage :tiny (:use :cl))

(in-package :tiny)

(defvar *about*
 '("TINY (c) 2022, Tim Menzies"
   "Multi-objective semi-supervised XAI, in not too many lines."))

(defvar *options*
 `((file "-f" "help file" "../../data/auto93.lisp")
   (help "-h" "show help" nil)
   (keep "-K" "items to keep" 256)
   (k "-k" "nb low attributes classes" 1)
   (m "-n" "nb low frequency classes" 2)
   (seed "-s" "random number seed" 10019)
   (go "-g" "start up action" ls)))

(defmacro aif (test yes &optional no)
 `(let ((it ,test)) (if it ,yes ,no)))

(defmacro ?? (x) `(fourth (assoc ',x *options*)))

(defmacro ? (s x &rest xs)
 (if (null xs) `(slot-value ,s ',x)
  `(? (slot-value ,s ',x) ,@xs)))

(defmacro counts (x a)
 `(cdr
   (or (assoc ,x ,a :test #'equal)
    (car (setf ,a (cons (cons ,x 0) ,a))))))

(defun str->thing
 (x &aux (y (string-trim '(#\space #\tab #\newline) x)))
 (if (string= y "?") "?"
  (let ((z (ignore-errors (read-from-string y))))
   (if (numberp z) z y))))

(defun cli (about lst)
 (dolist (four lst)
  (let* ((args ext:*args*))
   (aif (member (second four) args :test 'equal)
    (setf (fourth four)
     (cond ((equal (fourth four) t) nil)
      ((equal (fourth four) nil) t)
      (t (str->thing (second it))))))))
 (when (fourth (assoc 'help lst))
  (format t "~&~%~{~a~%~}~%OPTIONS:~%" about)
  (dolist (a lst)
   (format t "  ~a  ~55a  ~a ~%" (elt a 1) (elt a 2)
    (elt a 3)))))

````



##### Random number generation.


```lisp

(defvar *seed* (?? seed))

(defun randi (&optional (n 1))
 (floor (* n (/ (randf 1000.0) 1000))))

(defun randf (&optional (n 1.0))
 (setf *seed* (mod (* 16807.0d0 *seed*) 2.147483647d9))
 (* n (- 1.0d0 (/ *seed* 2.147483647d9))))

````



first and  last characters of string


```lisp

(defun chars (x) (if (stringp x) x (symbol-name x)))

(defun charn (x &aux (y (chars x)))
 (char y (1- (length y))))

(defun char0 (x &aux (y (chars x))) (char y 0))

````



iterate `f` over all items in `file`


```lisp

(defun reads (file f)
 (with-open-file (s file)
  (labels
   ((there nil (here (read s nil)))
    (here (x) (when x (funcall f x) (there))))
   (there))))

(defun doc (file &optional (str t))
 (labels
  ((writes (now after more)
    (typecase now
     (string (format t "~%~%~a~%" now)
      (typecase after (string (terpri str))
       (cons (format str "~%~%```lisp~%~%"))))
     (cons
      (write now :case :downcase :pretty t :right-margin 60)
      (terpri str)
      (if (consp after) (terpri str)
       (format str "~%````~%~%"))))
    (if after (writes after (car more) (cdr more)))))
  (let (all) (reads file (lambda (x) (push x all)))
   (nreverse all)
   (writes (car all) (cadr all) (cddr all)))))

````



### Structs
ROWs keeps 1 record in `cell` and  sets `used` if we access the `y` vals.


```lisp

(defstruct row cells used)

````



ROWS holds many records in `rows`; summarized in `cols`.


```lisp

(defstruct (rows (:constructor %make-rows)) rows cols)

````



COLS summarize the goal and independent columns in `x` and `y`.


```lisp

(defstruct (cols (:constructor %make-cols)) all x y names)

(defstruct col (n 0) (at 0) (txt "") (w 1))

(defstruct (num (:include col)) (kept (make-few)))

(defstruct (sym (:include col)) kept)

(defstruct (few (:include col))
 (kept (make-array 2 :fill-pointer 0 :adjustable t)) g
 (max (?? keep)) ok)

````



-------------------------------------------------------------------------------
### Columns


```lisp

(defmethod add ((self num) x)
 (unless (eql '? x) (incf (? self n))
  (add (? self kept) x)))

(defmethod add ((self sym) x)
 (unless (eql '? x) (incf (? self n))
  (incf (counts x (? self kept)))))

(defmethod add ((self few) (x number)) (incf (? self n))
 (let ((size (length (? self kept))))
  (cond
   ((< size (? self max)) (setf (? self ok) nil)
    (vector-push-extend x (? self kept)))
   ((< (randf) (/ (? self n) (? self max)))
    (setf (? self ok) nil)
    (setf (elt (? self kept) (randi size)) x)))))

(defmethod kept (self) (? self kept))

(defmethod kept ((self few))
 (unless (? self ok) (sort (? self kept) #'<))
 (setf (? self ok) t) (? self kept))

````



### Cols  


```lisp

(defun make-cols
 (names &aux
  (cols (%make-cols :names (mapcar 'chars names))))
 (let ((at -1))
  (dolist (txt (? cols names) cols)
   (let
    ((col
      (if (equal #\$ (char0 txt))
       (make-num :at (incf at) :txt txt)
       (make-sym :at (incf at) :txt txt))))
    (push col (? cols all))
    (setf (? col w) (if (eql #\- (charn txt)) -1 1))
    (unless (eql #\: (charn txt))
     (if (eql #\: (charn txt)) (setf (? cols klass) col))
     (if (member (charn txt) '(#\! #\- #\+))
      (push col (? cols y)) (push col (? cols x))))))))

(defmethod add ((self cols) (r row))
 (dolist (slot '(x y) r)
  (dolist (col (slot-value self slot)) (print (? col txt))
   (add col (elt (? r cells) (? col at))))))

````



### rows


```lisp

(defun make-rows (&optional src &aux (rows (%make-rows)))
 (if (stringp src)
  (reads src (lambda (x) (print `(1 ,x)) (add rows x)))
  (dolist (x src) (print `(2 ,x)) (add rows x)))
 rows)

(defmethod add ((self rows) (r cons)) (print 2)
 (add self (make-row :cells r)))

(defmethod add ((self rows) (r row))
 (if (? self cols)
  (push (add (? self cols) r) (? self rows))
  (setf (? self cols) (make-cols r))))

(defvar *run* nil)

(defmacro defdemo (name arg doc &rest src)
 `(push (list ',name ',doc (lambda ,arg ,@src)) *run*))

(defun alike (x y)
 (string= (format nil "~(~a~)" x) (format nil "~(~a~)" y)))

(defdemo ls nil "list all demos"
 (dolist (one (reverse *run*) t)
  (format t "  -g  ~(~5a~)  ~a~%" (first one)
   (second one))))

(defdemo doc nil "generate md from argum(emen to -f"
 (doc (?? file) t))

(defdemo all nil "list all demos"
 (dolist (three (reverse *run*) t)
  (unless (equal (first three) 'all)
   (when
    (or (alike (?? go) 'all) (alike (?? go) (first three)))
    (setf *seed* (?? seed))
    (unless (eq t (funcall (third three)))
     (format t "~%; W? DO ~a  FAIL" (first three)))))))

(defdemo num nil "asdsa"
 (print
  (let ((n (make-num)))
   (dotimes (i 100 (kept (? n kept))) (add n i))))
 t)

(cli *about* *options*)

(funcall (third (assoc 'all *run*)))

````



(defrun sym (print (let ((s (make-sym))) 
                    (dotimes (i 100 (? s kept)) (dolist (x '(a a b)) (add s x))))))

(cols (print (make-cols 
                    '($Cylndrs $Dsplcemnt $Hp $Lbs- $Acc+ $Model origin  $Mpg+))))
     (rows ((print (make-rows (?? file)))

; W? DO DOC  FAIL