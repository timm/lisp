 ; vim: filetype=lisp ts=2 sw=3 sts=2 et :
 vim: ts=2 sw=2 et:

(defpackage :espy (:use :cl))
(in-package :espy)
(load "tricks")

(defstruct about
  (what "
        ,-_|\   lispth is litenning
       /     \  
       \_,-._*  Cluster, then report just the
            v   deltas between nearby clusters.")
  (who "(c) 2021 Tim Menzies unlicense.org")
  (flags '(
    (knn 2 "kth nearest neighbors")
    (loud nil "verbose mode (defaults to false)"))))

(defstruct stuff
  (runs 0) (fails 0)
  (tests))

(defvar *stuff* (make-stuff))

;(defun test (x) (push fun (? *stuff* tests)))

(defmacro aif (test yes &optional no) 
  "anaphoric if"
  `(let ((it ,test)) (if it ,yes ,no)))

   (dolist(fun  (funs :espy))
       (print (documentation fun 'function)))

(halt)

; Anaphoric while (the current of the loop controller is cached in `a`).
(defmacro whale (expr &body body) 
  `(do ((a ,expr ,expr)) ((not a)) ,@body))

; Recursive  plist accessor; e.g. `(? p :outer :inner)`.
(defmacro ? (p x &rest xs)
  (if (null xs) `(getf ,p ,x) `(? (getf ,p ,x) ,@xs)))

; Recurse struct accessor; e.g. `(o s address street number)`.
(defmacro o (s x &rest xs)
  (if (null xs) `(slot-value ,s ',x) `(o (slot-value ,s ',x) ,@xs)))

; Simpler assert statement.
(defmacro want (x &rest y)
  `(assert ,x () ,@y))

; Return  number with `places` number of decimals."
(defun rnd (number &optional (places 0))
  (let ((div (expt 10 places)))
    (float (/ (round (* number div)) div))))

; --------------------------------------------
; ## Random Numbers
; I confess that I never found a way to do
; platform independent random number generation with
; CommonLisp. So I write my own."

(defvar *seed* 10013)

; ### srand
; Reset random number seed,
(defun srand (&optional (n 10013))  
  (setf *seed* n))

; ### randi
; Return a random integer 0.. n-1.
(defun randi (&optional (n 1)) 
  (floor (* n (/ (randf 1000.0) 1000))))

; ### randf
; Return a random flaot 0..n-1.
(defun randf (&optional (n 1.0)) 
  (_park-miller n))

(defun _park-miller (n)
  (let ((multiplier 16807.0d0)
        (modulus    2147483647.0d0))
    (setf *seed* (mod (* multiplier *seed*) modulus))
    (* n (- 1.0d0 (/ *seed* modulus)))))

; ## System
; Wrapper functions to SBCL system functions with strange names.

; ### halt
; Exit
(defun halt (&optional (status 0)) (sb-ext:exit :code status))

; ### argv
; Arguments
(defun argv () sb-ext:*posix-argv*)

; ### cli
; Given a plist with keywords, if  the command line 
; has any of the same keywords, then update the plist with the
; command-line values.keywords that 
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

; ## Types
; ### cell?
; Return a number (if we can). 
(defun cell? (x &optional looping)
  (cond ((numberp x) x)
        ((stringp x) (if (equal "?" x)
                         #\?
                       (let ((y (read-from-string x)))
                         (if (numberp y) y x))))
        (t x)))

; ## List stuff
; ### inca
; A counter, implemented as an association list.
(defmacro inca (x a &optional (n  1))
  `(incf (cdr (or (assoc ,x ,a :test #'equal)
                  (car (setf ,a (cons (cons ,x 0) ,a)))))
         ,n))


; ### Powerset
; Return all subsets of a list.
(defun powerset (lst)
  (let ((out (list nil)))
    (dolist (x lst out)
      (dolist (tmp out)
        (push (cons x tmp) out)))))

; ## Colors
; ### color
; Return string `s`, surrounded by ANSI escape color sequences.
(defun color (s c &optional (str t))
  (let ((all '((black . 30) (red . 31) (green . 32)  (yellow . 33) 
               (blue . 34)  (magenta . 35) (cyan . 36) (white .37))))
    (format str "~c[~a;1m~a~c[0m" #\ESC (cdr (assoc c all)) s #\ESC)))

; ### red
(defun red (s) (color s 'red nil))
; ### green
(defun green (s) (color s 'green nil))
; ### yellow
(defun yellow (s) (color s 'yellow nil))

; ## String stuff
; ### str->words
; Kill white space, split string on `sep` (defaults to ',').
(defun str->words (s0 &optional (sep #\comma)) 
  (labels ((whitep (c) (member c '(#\space #\tab)))
           (worker (str &optional (lo 0))
                   (aif (position sep str :start lo)
                        (cons (subseq str lo  it) (worker str (1+ it)))
                        (list (subseq str lo)))))
    (let ((s1 (remove-if  #'whitep s0)))
      (unless (zerop (length s1)) (worker  s1)))))

; ### file->words
; For each line  in file `f`, call a function `fn` on a list of words in each line.
(defun file->words (f fn)
  (with-open-file (s f) 
    (whale (read-line s nil)
      (aif (str->words a) (funcall fn  it)))))
; -----------------------------------------------
; ## Columns
; ### Col
(defstruct col (n 0) txt (w -1) (pos 0))
(defmethod add ((x col) (y cons)) (dolist (z y) (add x z)))

(defmethod add ((x col) y)
  (unless (eq #\? y)
    (incf (o x n))
    (add1 x y))
  y)

(defmethod dist ((c col) x y)
  (if (and (eq x #\?) (eq y #\?)) 1 (dist1 x y)))

; ### Skip
(defstruct (skip (:include col)))
(defmethod add1 ((x skip) y &optional (n 1)) y)

; ### Sym
(defstruct (sym (:include col))  seen mode (most 0))
(defmethod add1 ((s sym) y &optional (n 1))
  (with-slots (most mode) s
    (let ((new (inca y (o s seen) n)))
      (when (> new (o s most))
        (setf most new
              mode y))))
  y)

(defmethod mid ((s sym)) (o s mode))
(defmethod var ((s sym)) (entropy s))

(defmethod entropy ((s sym) &aux (e 0))
  (dolist (x (o s seen) e)
    (let ((p (/ (cdr x) (o s n))))
      (decf e (* p (log p 2))))))
                     
(defmethod dist1 ((c sym) x y) (if (eql x y) 1 0))

; ### Num
(defstruct (num (:include col))
  (_all (make-array 32 :fill-pointer 0 :adjustable t))
  sorted)

(defmethod add1 ((n num) (x string) &optional (r 1))
  (add1 n (read-from-string x) r))

; (defmethod add1 ((n num) (x number) &optional (r 1))
;   (loop repeat r do (push-vector-extend x (o n all))
;   (setf (o n sorted) nil)
;   x)
;
(defmethod all ((n num))
  (unless (o n sorted)
    (setf (o n _all)   (sort (o n _all) #'<)
          (o n sorted) t))
   (o n _all))

(defmethod mid ((n num)) (per n .5))
(defmethod var ((n num))  (sd n))
(defmethod sd  ((n num)) (/ (- (per n .9) (per n .1)) 2.56))
(defmethod per ((n num) &optional (p .5))
  (let* ((v (all n))
         (s (length v)))
    (svref v (floor (* p s)))))

(defmethod lo ((n  num)) (svref (all n) 0))
(defmethod hi ((n  num) &aux (a (all  n))) 
  (svref a (1- (length a))))

(defmethod dist1 ((n num) a b)
  (cond ((eq a #\?) (setf b (norm n b)
                          a (if (> b 0.5) 1 0)))
        ((eq b #\?) (setf a (norm n  a)
                          b (if (> a 0.5) 1 0)))
        (t          (setf a (norm n a)
                          b (norm n b))))
  (abs (- a b)))

(defmethod norm ((n num) x)
  (if (eq x #\?)
      x
    (let ((n1 (lo n)) (n2 (hi n)))
      (if (eql n1 n2)
          0
        (max 0 (min 1 (/ (- x n1) (- n2 n1 1E-32))))))))
  
