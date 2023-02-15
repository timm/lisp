; vi: set ts=2 sw=2 sts=2 et :
;<img align=right src="https://www.lisperati.com/lisplogo_flag2_256.png">
;# Xplainable AI, Made Easy
;**Tim Menzies** 
(defpackage :xai (:use :cl))
(in-package :xai)

; This code is divided as follows:
;
; - _Settings_: control vars;
; - _Lib_: standard utils;
; - _Data_: data models;
; - _Demos_: test cases;
; - _Start-up_: and away we go
;
;[TOC]
;

;## Settings: used to define command-line flags and global `*settings`
(defvar *settings* nil)
(defvar *help* "
xai: simple lisp
(c) 2023 Tim Menzies <timm@ieee.org> BSD-2
   
USAGE: lisp xai.lisp [OPTIONS] [-g ACTION]
    
OPTIONS:
  -h   help          show help                  = nil
  -g   action        start up action            = none
  -f   far           how far to seek poles      = .95
  -R   right-margin  pretty print right margin  = 1000
  -p   p             distance coeffecient       = 2
  -s   seed          random number seed         = 10013")

;## Macros: must be defined before rest
;- `eg`: define an example (for our test suite)
;- `?`: Alist accessor: Defaults to querying `*settings*`
;- `aif`: anaophic if: use when testing on a result alsoneeded by `then`
;- `geta`: symbol frequency counter: faster than hashtables for, say or less symbols.
(defvar *egs* nil)
(defmacro eg   (what doc &body body)           `(push (list ,what ,doc (lambda () ,@body)) *egs*))
(defmacro ?    (x &optional (lst '*settings*)) `(cdr (assoc ',x ,lst :test #'equal)))
(defmacro aif  (test then &optional else)      `(let ((it ,test)) (if it ,then ,else)))
(defmacro geta (x lst &optional (init 0))      `(cdr (or (assoc ,x ,lst :test #'equal) 
                                                         (car (setf ,lst (cons (cons ,x ,init) ,lst))))))

;## Demos
According to TDD, code should be developed incrementally, test by test.
Hence, this code stores a set of tests (easiest to hardest) in in *egs*. |#


(eg "my" "show options" 
    (print 2) t)

(eg "ls"  "show options" 
    (print *settings*) t)

(eg "geta" "test adaptive alist"
    (let ((lst '((b . 100))))
      (incf (geta 'a lst))
      (incf (geta 'a lst))
      (incf (geta 'a lst))
      (equal 3 (cdr (assoc 'a lst)))))

(eg "lines" "testing file reading"
    (let ((n 0))
      (with-file "../data/auto93.csv" 
                  (lambda (s)  (incf n (length s))))
      (eql  n 3192)))

(eg "num" "test number"  
    (let  ((n (num! 10 "num"))) 
      (dolist (i '(1 1 1 1 2 2 3)) (add n i))
      (and (equalp 11/7 (mid n)) (equalp 0.7867958 (div n)))))

(eg "sym" "test symbols"  
    (let ((s (sym! 10 "sym"))) 
      (dolist (i '(a a a a b b c)) (add s i))
      (print (div s))
      (and (equalp 'a (mid s)) (<= 1.378 (div s) 1.379))))

(eg "cols" "create some columns"
    (print (cols! '("Aas" "state" "Weight-")) t))

(eg "data" "testing file reading"
    (print (cols-x (data-cols  (data! "../data/auto93.csv")))))

(eg "dist" "dostance function" 
    (let* ((n -1) 
           (data (data! "../data/auto93.csv"))
           (row1 (first (data-rows data))))
      (format t "~%~a~%" (row-cells row1))
      (dolist (row2 (data-rows data) t) 
        (if (zerop (mod (incf n) 40)) 
          (format t "~a ~a ~a~%" n (row-cells row2) (dists  data row1 row2))))))
#|### Portability
Different LISPs handle certain common task in different ways.

_Accessing command-line flags._ |#
(defun args () 
  #+clisp ext:*args*  
  #+sbcl sb-ext:*posix-argv*)

;_Quit LISP._|#
(defun stop (&optional (x 0)) 
  #+clisp (ext:exit x) 
  #+sbcl  (sb-ext:exit :code x))

;### Strings to Things
;_Coerce `s`_ into a number or string or t or nil or #\?.
(defun thing (s &aux (s1 (trim s)))
  (cond ((equal s1 "?") #\?)
        ((equal s1 "t") t)
        ((equal s1 "nil") nil)
        (t (let ((n (read-from-string s1 nil nil))) 
             (if (numberp n) n s1)))))

;### Strings
;_Kill whitespace_ at start, at end.
(defun trim (s) 
  (string-trim '(#\Space #\Tab #\Newline) s))

;_Is `s` a string holding `c`_ at position `n` (and negative `n` means 'from end of string')?.
(defun got (s c &optional (n 0))
  (if (stringp s)
    (if (< n 0) 
      (got s c (+ (length s) n))
      (and (>= n 0) (< n (length s)) (eql c (char s n))))))

;_Split  `s`, divided by `sep`_ filtered through `filter`.
(defun split (s &optional (sep #\,) (filter #'thing) (here 0))
  (let* ((there (position sep s :start here))
         (word  (funcall filter (subseq s here there))))
    (labels ((tail () (if there (split s sep filter (1+ there)))))
      (if (equal word "") (tail) (cons word (tail))))))

;_Divide a string_ on space.
(defun words (s) 
  (split s #\Space #'trim))

;### File I/O
;_Call `fun` for each line in `file`_.
(defun with-file (file fun &optional (filter #'split))
  (with-open-file (s file) 
    (loop (funcall fun (funcall filter (or (read-line s nil) (return)))))))

#|### Random
Common Lisp is infuriating: there is no simple way to set the random set. 
Hence, we roll our own.    

_Set random seed._ |#
(defvar *seed* 10013)

;_Random float 0.. < n._
(defun rand (&optional (n 2))
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

; _Random int 0..n-1._
(defun rint (&optional (n 2) &aux (base 10000000000.0))
  (floor (* n (/ (rand base) base))))

;### Settings
; _For lines like '  -Key Flag ..... Default', return `(KEY . DEFAULT)`._
(defun settings (s &optional args)
  (loop 
    :for (flag key . lst) 
    :in  (split s #\NewLine #'words)
    :if  (got flag #\-) 
    :collect (cons (intern (string-upcase key)) 
                   (cli args flag (thing (car (last lst)))))))

#| If `flag` is in `args`, then change  `b4` using `args` values;
if `b4` is a boolean, there is no need for command-line values--
just flip `b4`. |#
(defun cli (args flag b4)
  (aif (member flag args :test 'equal)
    (cond ((eql b4 t) nil)
          ((eql b4 nil) t)
          (t (thing (second it))))
    b4))

#|### Tests
#| Run 'all' actions or just the `(? action)` action (resetting
random seed and other setting before each action). Return the number
of example failures to the operating system. |#
(defun egs ()
  (let ((fails 0)
        (b4 (copy-list *settings*)))
    (dolist (x (reverse *egs*))
      (let ((name (first x)))
        (when (or (equal (? action) name) 
                  (equal (? action) "all"))
          (setf *settings*           b4
                *print-right-margin* (? right-margin)
                *seed*               (? seed))
          (format t "▶️  TEST: ~a " name)  
          (cond ((funcall (third x)) (format t "✅ PASS ~%"))
                (t                   (format t "❌ FAIL ~%")
                                     (incf fails))))))
    (stop fails)))

;#### egs and help
; _Show the help string_ (built from *help* and the doc strings from *egs*.
(defun about ()
  (format t "~a~%~%ACTIONS:~%" *help*)
  (dolist (x (reverse *egs*))
    (format t "  -g ~10a : ~a~%" (first x) (second x))))

;## Data
(defun isSym    (x) (eql 'Sym (type-of x)))
(defun isNums   (s) (and (> (length s) 1) (upper-case-p (char s 0))))
(defun isGoal   (s) (or (isKlass s) (isLess s) (isMore s)))
(defun isIgnore (s) (got s #\X -1))
(defun isKlass  (s) (got s #\! -1))
(defun isLess   (s) (got s #\- -1))
(defun isMore   (s) (got s #\+ -1))

;### sym
; _Summarizes streams of numbers_.
(defstruct sym (at 0) (txt "") (n 0) has (w 1) mode (most 0))
(defun sym! (&optional (at 0) (txt ""))
  (make-sym :at at :txt txt :w (if (isLess txt) -1 1)))

(defmethod add ((i sym) x)
  (with-slots (n has mode most) i
    (unless (eq x #\?)
      (incf n)
      (incf (geta x has))
      (when (> (geta x has) most)
        (setf most (geta x has)
              mode x)))))

(defmethod mid ((i sym)) (sym-mode i))
(defmethod div ((i sym))
  "Diversity (entropy)."
  (with-slots (has n) i 
    (labels ((fun (p) (* -1 (* p (log p 2)))))
      (loop for (_ . n1) in has sum (fun (/ n1 n))))))

(defmethod dist ((i sym) x y)
  (cond ((and (equal x #\?) (equal x #\?)) 1)
        (t                  (if (equal x y) 0 1))))

;### num
(defstruct num (at 0) (txt "") (n 0) (mu 0) (m2 0) (w 1) (lo 1E31) (hi -1321))
(defun num! (&optional (at 0) (txt ""))
  "summarizes streams of numbers"
  (make-num :at at :txt txt :w (if (isLess txt) -1 1)))

(defmethod add ((i num) x) ;;; Add one thing, updating 'lo,hi'
  (with-slots (n lo hi mu m2) i
    (unless (eq x #\?)
      (incf n)
      (let ((d (- x mu)))
        (incf mu (/ d n))
        (incf m2 (* d (- x mu)))
        (setf lo (min x lo)
              hi (max x hi))))))

(defmethod mid ((i num)) (num-mu i))
(defmethod div ((i num))
  (with-slots (n m2) i (if (<= n 1) 0 (sqrt (/ m2 (- n 1))))))

(defmethod norm ((i num) x) ;;; Map 'x' 0..1 (unless unknown, unless too small)
  (with-slots (lo hi) i
    (if (eq x #\?) x (/ (- x lo) (- hi lo 1e-32)))))

(defmethod dist ((i num) x y)
  (if (and (equal x #\?) (equal x #\?)) 
    1
    (let ((x (norm i x))
          (y (norm i y)))
      (if (eq x #\?) (setf x (if (< y .5) 1 0)))
      (if (eq y #\?) (setf y (if (< x .5) 1 0)))
      (abs (- x y)))))

; _Create something that holds `cells`s._
(defstruct row cells y-used)
(defun row! (cells)
  (make-row :cells cells))

(defmethod th ((r row) (c num))    (th r (num-at c)))
(defmethod th ((r row) (c sym))    (th r (sym-at c)))
(defmethod th ((r row) (n number)) (elt (row-cells r) n))

;### Cols
; _Factory for generating column headers_ from list of column names.
(defstruct cols all x y klass)
(defun cols! (lst &aux (i (make-cols)) (at -1))
  (with-slots (all x y klass) i
    (dolist (txt lst i)
      (let ((col (funcall (if (isNums txt) #'num! #'sym!) (incf at) txt))) 
        (push col all)
        (when (not (isIgnore txt))
          (if (isGoal txt) (push col y) (push col x))
          (if (isKlass txt) (setf klass col)))))))

; _Update x and y column headers_ from data in row. returns row.
(defmethod add ((i cols) row)
  (dolist (col (cols-x i)    ) (add col (th row col)))
  (dolist (col (cols-y i) row) (add col (th row col))))

;### Data
; _Create data_ from either a file called 'src' or a list `src'.
(defstruct data rows cols)
(defun data! (src  &aux (i (make-data)))
  (labels ((update (x) (add i x)))
    (if (stringp src) (with-file src #'update) (mapc #'update  src))
    i))

; _Make `cols`_ (if currently missing) or update the cols and rows.
(defmethod add ((i data) x)
  (with-slots (cols rows) i
    (if cols 
      (push (add cols (if (row-p x) x (row! x))) 
            rows)
      (setf cols (cols! x)))))

; _Returns 0..1._
(defmethod dists ((i data) (row1 row) (row2 row) &optional (cols (cols-x (data-cols i))))
  (let ((d 0) (n 1E-32))
    (dolist (col cols (expt (/ d n) (/ 1 (? p))))
      (incf d (expt (dist col (th row1 (slot-value col 'at)) (th row2 (slot-value col 'at))) 
                    (? p)))
      (incf n))))

(defmethod around ((i data) (row1 row) &optional (rows (data-rows i)) (cols (data-cols i)))
   (sort (mapcar (lambda (row2) (cons (dists i row1 row2 cols) row2)) rows) #'< :key #'cdr))


;## Start-up
(setf *settings* (settings *help* (args)))
(if (? help) (about) (egs))


