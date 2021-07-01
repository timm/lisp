; vim: noai:ts=2:sw=2:et: 
"
## Etc

Misc utils
"

(defpackage :espy-misc-utils 
  (:export todo)
  (:use :cl)  (:nicknames :etc))
(in-package :etc)


(defun loading (file)
  "Load a file, ignoring the SBCL messages."
  (format *error-output* "; loading ~(~a~) ...~%" file)
  (handler-bind ((style-warning #'muffle-warning))
    (load file)))

(loading "config");

"----------------------------------------------
###  Macros"

(defmacro aif (test yes &optional no)
  "Anaphoric if (the result of the condition 
   is cached in `it`)."

  `(let ((it ,test)) (if it ,yes ,no)))

(defmacro whale (expr &body body) 
  "Anaphoric while (the current of the loop controller 
   is cached in `a`)."
  `(do ((a ,expr ,expr)) ((not a)) ,@body))

(defmacro ? (p x &rest xs)
  "Recursive  plist accessor; e.g. `(? p :outer :inner)`."
  (if (null xs) `(getf ,p ,x) `(? (getf ,p ,x) ,@xs)))

(defmacro o (s x &rest xs)
  "Recurse struct accessor; e.g. `(o s address street number)`."
  (if (null xs) `(slot-value ,s ,x) `(o (slot-value ,s ,x) ,@xs)))

(defmacro want (x &rest y)
  "Simpler assert statement."
  `(assert ,x () ,@y))

(defun rnd (number &optional (places 0))
  "Return  number with `places` number of decimals."
  (let ((div (expt 10 places)))
    (float (/ (round (* number div)) div))))

"----------------------------------------------
### Random Numbers

I confess that I never found a way to do
platform independent random number generation with
CommonLisp. So I write my own."

(defvar *seed* 10013)
(defun _park-miller (n)
  (let ((multiplier 16807.0d0)
        (modulus    2147483647.0d0))
    (setf *seed* (mod (* multiplier *seed*) modulus))
    (* n (- 1.0d0 (/ *seed* modulus)))))

(defun randf (&optional (n 1.0)) 
  "Return a random flaot 0..n-1."
  (_park-miller n))

(defun srand (&optional (n 10013))  
  "Reset random number seed,"
  (setf *seed* n))

(defun randi (&optional (n 1)) 
  "Return a random integer 0.. n-1"
  (floor (* n (/ (randf 1000.0) 1000))))

"----------------------------------------------
### System

Wrapper functions to SBCL system functions with strange 
names."

(defun halt (&optional (status 0)) (sb-ext:exit :code status))
(defun argv () sb-ext:*posix-argv*)

; ------------------------------------
(defun num? (x &optional looping)
  (cond ((numberp x) x)
        ((stringp x) (let ((y (read-from-string x)))
                       (if (numberp y) y x)))
        (t x))) 

; ---------------------------------------------
(defun cli (&key (plist  (deepcopy +config+)) 
                 (help   "")
                 (args   (cdr (deepcopy (argv))))
                 (now    (getf plist :all)))
   (whale (pop args)
     (print a)
     (setf a (read-from-string a))
     (cond ((equalp a :H)  (format t "~a~%" help))
           ((getf plist a) (setf now (getf plist a)))
           ((getf now a)   (setf (getf now a) 
                                 (read-from-string (car args))))
            ((keywordp a)   (format t (red ";?? ignoring [~a]") a))))
   plist)

;----------------------------------------------
(defun deepcopy (x)
   (if (consp x) (mapcar #'deepcopy x) x))

(defun powerset (lst)
  (let ((out (list nil)))
    (dolist (x lst out)
      (dolist (tmp out)
        (push (cons x tmp) out)))))

(defun color (s  c &optional (str t)) 
 (let ((all '((black . 30) (red . 31) (green . 32)  (yellow . 33) 
              (blue . 34)  (magenta . 35) (cyan . 36) (white .37))))
   (format str "~c[~a;1m~a~c[0m" #\ESC (cdr (assoc c all)) s #\ESC)))
(defun colorn (s  n &optional (str t)) 
   (format str "~c[~a;1m~a~c[0m" #\ESC n s #\ESC))

(defun red (s) (color s 'red nil))
(defun green (s) (color s 'green nil))
(defun yellow (s) (color s 'yellow nil))

; ----------------------------------------------
(defun str->words (s0) 
  (labels ((whitep (c) (member c '(#\space #\tab)))
           (worker (str &optional (lo 0))
                   (aif (position #\comma str :start lo)
                        (cons (subseq str lo  it) (worker str (1+ it)))
                        (list (subseq str lo)))))
    (let ((s1 (remove-if  #'whitep s0)))
      (unless (zerop (length s1)) (worker  s1)))))

(defun file->words (f fn)
  (with-open-file (s f) 
    (whale (read-line s nil)
           (aif (str->words a) (funcall fn  it)))))

;(file->words  "../data/auto93.csv" #'print)
