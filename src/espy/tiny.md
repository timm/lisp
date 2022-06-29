<img src="http://www.lisperati.com/lisplogo_fancy_256.png" width=200 align=right>
# Tiny
Some tricks

<ul><details><summary>CODE</summary>

```lisp
(defpackage :tiny (:use :cl))

(in-package :tiny)
```

</details></ul>

Config

<ul><details><summary>CODE</summary>

```lisp
(defvar *about* 
  '("TINY (c) 2022, Tim Menzies" 
    "Multi-objective semi-supervised XAI, in a few 100 lines."))

(defvar *options* 
   `((file   "-f"   "help file                "  "../../data/auto93.lisp")
     (help   "-h"   "show help                "  nil)
     (keep   "-K"   "items to keep            "  256)
     (k      "-k"   "nb low attributes classes"  1)
     (m      "-n"   "nb low frequency classes "  2)
     (seed   "-s"   "random number seed       "  10019)))
```

</details></ul>

## Library
### Macros
asdas

<ul><details><summary>CODE</summary>

```lisp
(aif if then else) :anaphoric `if` (remembers results of `if` in `it`)

(defmacro aif (test yes &optional no) `(let ((it ,test)) (if it ,yes ,no)))

(?? x:atom):atom :return an option

(defmacro ?? (x) `(fourth (assoc ',x *options*)))

(? x:struct &rest slots:[atom]):atom :nested slot access

(defmacro ? (s x &rest xs)
  (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))
```

</details></ul>

A counter, implemented as an association list.

<ul><details><summary>CODE</summary>

```lisp
(defmacro counts (x a)
  `(cdr (or (assoc ,x ,a :test #'equal)
            (car (setf ,a (cons (cons ,x 0) ,a))))))
```

</details></ul>

### Misc

<ul><details><summary>CODE</summary>

```lisp
(str->thing x:str):atom :

(defun str->thing (x &aux (y (string-trim '(#\Space #\Tab #\Newline) x)))
  (if (string= y "?")     
    "?"
    (let ((z (ignore-errors (read-from-string y))))
      (if (numberp z) z y))))

(defun cli (about lst)  
  (dolist (four lst)
    (let* ((args #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*))
      (aif (member (second four) args :test 'equal)
        (setf (fourth four) (cond ((equal (fourth four) t)   nil)
                                  ((equal (fourth four) nil) t)
                                  (t (str->thing (second it))))))))
  (when (fourth (assoc 'help lst))
    (format t "~&~%~{~a~%~}~%OPTIONS:~%" about)
    (dolist (a lst) 
      (format t "  ~a  ~a  ~a ~%" (elt a 1) (elt a 2) (elt a 3)))))
```

</details></ul>

### Random number generation.

<ul><details><summary>CODE</summary>

```lisp
(defvar *seed* (?? seed))

(defun randi (&optional (n 1)) (floor (* n (/ (randf 1000.0) 1000))))

(defun randf (&optional (n 1.0)) 
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))
```

</details></ul>

first and  last characters of string

<ul><details><summary>CODE</summary>

```lisp
(defun chars (x) (if (stringp x) x (symbol-name x)))

(defun charn (x &aux (y (chars x))) (char y (1- (length y))))

(defun char0 (x &aux (y (chars x))) (char y 0))
```

</details></ul>

iterate `f` over all items in `file`

<ul><details><summary>CODE</summary>

```lisp
(defun reads (file f)
  (with-open-file (s file) 
    (labels ((there ()  (here (read s nil)))
             (here  (x) (when x (funcall f x) (there))))
      (there))))
```

</details></ul>

ROWs keeps 1 record in "cell" and  sets "used" if we access the "y" vals.

<ul><details><summary>CODE</summary>

```lisp
(defstruct row cells used)
ROWS holds many records in "rows"summarized in "cols".

(defstruct rows rows cols)
COLS summarize the goal and independent columns in "x" and "y".

(defstruct (cols (:constructor %make-cols)) all x y names)

(defstruct col (n 0) (at 0) (txt "") (w 1) )

(defstruct (num (:include col)) (kept (make-few)))

(defstruct (sym (:include col)) kept)

(defstruct (few (:include col)) 
  (kept (make-array 2 :fill-pointer 0 :adjustable t)) 
  (max (?? keep))
  ok)

(defmethod add ((self num) x)
  (unless (eql '? x)
    (incf (? self n))
    (add (? self kept) x)))

(defmethod add ((self sym) x)
  (unless (eql '? x)
    (incf (? self n))
    (incf (counts  (? self kept) x))))

(defmethod add ((self few) (x number))
  (incf (? self n))
  (let ((size (length (? self kept))))
    (cond ((< size  (? self max))
           (vector-push-extend x (? self kept))
           (setf (? self ok) nil))
          ((< (randf) (/ (? self n) (? self max)))
           (setf (elt (? self kept) (randi size) ) x)
           (setf (? self ok) nil)))))

(defmethod kept (self) (? self kept))

(defmethod kept ((self few))
  (unless  (? self ok) (setf (? self kept) 
                             (sort (? self kept) #'<)))
  (setf (? self ok) t)
  (? self kept))
```

</details></ul>

## Cols                 

<ul><details><summary>CODE</summary>

```lisp
(defun make-cols (names &aux (cols (%make-cols :names (mapcar 'chars names))))
  (let ((at -1))
    (dolist (txt (? cols names) cols)
      (let ((col (if (uppercase-p (char0 txt))
                     (make-num :at (incf at) :txt txt)
                     (make-sym :at (incf at) :txt txt))))
        (push col (? cols all))
        (setf (? cols w) (if (eql #\- (charn txt)) -1 1))
        (unless (eql #\: (charn txt))
          (if (eql   #\: (charn txt)) (setf (? cols klass) col))
          (if (member (charn txt) '(#\! #\- #\+)) 
            (push col (? cols y))
            (push col (? cols x))))))))

(defmethod add ((self cols) (r row))
  (dolist (slot '(x y) r)
    (dolist (col (slot-value self slot)) 
      (add col (elt (? r cells) (? col at))))))
                   
## rows

(defmethod add ((self rows) (r cons)) (add self (make-row :cells r)))

(defmethod add ((self rows) (r row)) 
  (if (? self cols) 
    (push (add (? self cols) r) (? self rows))
    (setf (? self cols) (make-cols r))))

(cli *about* *options*)

(print (let ((n (make-num)))
         (dotimes (i 100 (kept (? n kept))) (add n i))))
```
