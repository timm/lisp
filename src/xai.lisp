; vi: set ts=2 sw=2 sts=2 et :
;<font size=20pt><b>AI for busy people</b></font><br>
;**Tim Menzies**
(defpackage :xai (:use :cl))
(in-package :xai)

;<img align=right xwidth=250 src="https://screenshotbot.io/assets/images/integrations/botty.png">
;
;[TOC]

;## Globals
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

;From this help string, we fill in `*settings*` (and define the  command-line interface
;using the  `(settings)` function, see below)
(defvar *settings* nil)

;Where to store test functions.
(defvar *egs* nil)

;Random number seed (used by `(rand), (rint)`).
(defvar *seed* 10013)

;## Macros
(defmacro eg (what doc &body body)           
  "define a new example"
  `(push (list ,what ,doc (lambda () ,@body)) *egs*))

(defmacro ? (x &optional (lst '*settings*))
  "alist accessor, defaults to searching `*settings*`"
  `(cdr (assoc ',x ,lst :test #'equal)))

(defmacro aif (test then &optional else)      
  "used to test on a result that is also needed by `then`"
  `(let ((it ,test)) (if it ,then ,else)))

(defmacro freq (x lst &optional (init 0))      
  "frequency counts for small group of symbols (say, less than 50)"
  `(cdr (or (assoc ,x ,lst :test #'equal) 
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))

#|## Demos
According to TDD, code should be developed incrementally, test by test [^a].
Hence, this code stores a set of tests (easiest to hardest) in in *egs*.

[^a]: asdasas |#

(eg "ls"  "show options" 
    (print *settings*))

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
    (let ((s (sym 10 "sym"))) 
      (dolist (i '(a a a a b b c)) (adds s i))
      (print (divs s))
      (and (equalp 'a (mids s)) (<= 1.378 (divs s) 1.379))))

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

#|## Library
### Portability
Different LISPs handle certain common task in different ways. |#
(defun args () 
  "accessing command-line flats"
  #+clisp ext:*args*  
  #+sbcl sb-ext:*posix-argv*)

(defun stop (&optional (x 0)) 
  "quit list"
  #+clisp (ext:exit x) 
  #+sbcl  (sb-ext:exit :code x))

;### Strings to Things
(defun thing (s &aux (s1 (trim s)))
  "coerce `s` into a number or string or t or nil or #\?"
  (cond ((equal s1 "?") #\?)
        ((equal s1 "t") t)
        ((equal s1 "nil") nil)
        (t (let ((n (read-from-string s1 nil nil))) 
             (if (numberp n) n s1)))))

;### Lists
(defun per (seq &optional (p .5) &aux (v (coerce seq 'vector)))
  (elt v (fllor (* p (length v)))))

;### Strings
(defun trim (s) 
  "kill whitespace at start, at end"
  (string-trim '(#\Space #\Tab #\Newline) s))

(defun got (s c &optional (n 0))
  "Does `s` hold `c` at position `n` (negative `n` means 'from end of string')"
  (if (and (stringp s) (> (length s) 0))
    (if (< n 0) 
      (got s c (+ (length s) n))
      (and (>= n 0) (< n (length s)) (eql c (char s n))))))

(defun split (s &optional (sep #\,) (filter #'thing) (here 0))
  "split  `s`, divided by `sep` filtered through `filter`"
  (let* ((there (position sep s :start here))
         (word  (funcall filter (subseq s here there))))
    (labels ((tail () (if there (split s sep filter (1+ there)))))
      (if (equal word "") (tail) (cons word (tail))))))

(defun words (s) 
  "divide a string on space"
  (split s #\Space #'trim))

;### File I/O
(defun with-file (file fun &optional (filter #'split))
  "call `fun` for each line in `file`"
  (with-open-file (s file) 
    (loop (funcall fun (funcall filter (or (read-line s nil) (return)))))))

#|### Random
Common Lisp is infuriating: there is no simple way to set the random set. 
Hence, we roll our own. |#   

(defun rand (&optional (n 2))
  "random float 0.. < n"
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 2) &aux (base 10000000000.0))
  "random int 0..n-1"
  (floor (* n (/ (rand base) base))))

;### Settings
(defun settings (s &optional args)
  "for lines like '  -Key Flag ..... Default', return `(KEY . DEFAULT)`"
  (loop 
    :for (flag key . lst) 
    :in  (split s #\NewLine #'words)
    :if  (got flag #\-) 
    :collect (cons (intern (string-upcase key)) 
                   (cli args flag (thing (car (last lst)))))))

(defun cli (lst flag b4)
  "if `flag` in `lst`, then update `b4` from `lst`"
  (aif (member flag lst :test 'equal)
    (cond ((eql b4 t) nil)
          ((eql b4 nil) t)
          (t (thing (second it))))
    b4))

;### Tests
(defun egs ()
  "run 'all' actions or just the `(? action)` action"
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
(defun about ()
  "show help string (built from *help* and the doc strings from *egs*"
  (format t "~a~%~%ACTIONS:~%" *help*)
  (dolist (x (reverse *egs*))
    (format t "  -g ~10a : ~a~%" (first x) (second x))))

;## Data
(defun isNum    (s) (and (> (length s) 1) (upper-case-p (char s 0))))
(defun isGoal   (s) (or (isKlass s) (isLess s) (isMore s)))
(defun isIgnore (s) (got s #\X -1))
(defun isKlass  (s) (got s #\! -1))
(defun isLess   (s) (got s #\- -1))
(defun isMore   (s) (got s #\+ -1))

#|### sym
Summarizes streams of numbers. |#
(defstruct sym (at 0) (txt "") (n 0) has (w 1) mode (most 0))
(defun sym! (&optional (at 0) (txt ""))
  (make-sym :at at :txt txt :w (if (isLess txt) -1 1)))

(defun add (col x &optional (inc 1))
  (unless (eql col #\?)
    (incf (slot-value col 'n) inc)
    (if (num-p col) 
      (add-num col x) 
      (add-sym col x (incf (freq x (sym-has col)) inc)))))

(defun add-sym (sym x n) 
  (with-slots (most mode) sym
    (if (> n most)
      (setf most n
            mode x))))

(defun add-num (num x) 
  (with-slots (lo hi ok has n) num
    (labels ((add-end () (setf ok nil) (vector-push x has))
             (add-any () (setf ok nil) (setf (aref has (rint (length has))) x)))
      (setf lo (min lo x)
            hi (max hi x))
      (if  (< (length has) (? max)) 
        (add-end)
        (if (< (rand) (/ (? max) n))
          (add-any)))

(defun mids (sym) (sym-mode sym))
(defun divs (sym)
  "Diversity (entropy)."
  (with-slots (has n) sym 
    (labels ((fun (p) (* -1 (* p (log p 2)))))
      (loop for (_ . n1) in has sum (fun (/ n1 n))))))

(defmethod dist ((i sym) x y)
  (cond ((and (equal x #\?) (equal x #\?)) 1)
        (t                  (if (equal x y) 0 1))))

;### num
(defstruct num (at 0) (txt "") (n 0) (w 1)
               (has (make-array 5 :fill-pointer 0 :adjustable t))
               (lo 1E31) (hi -1321))

(defun num (&optional (at 0) (txt ""))
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

; Create something that holds `cells`s.
(defstruct row cells y-used)
(defun row! (cells)
  (make-row :cells cells))

(defmethod th ((r row) (c num))    (th r (num-at c)))
(defmethod th ((r row) (c sym))    (th r (sym-at c)))
(defmethod th ((r row) (n number)) (elt (row-cells r) n))

;### Cols (factory
(defstruct cols all x y klass)
(defun cols! (lst &aux (i (make-cols)) (at -1))
  "convert list of  column names to nums or syms"
  (with-slots (all x y klass) i
    (dolist (txt lst i)
      (let ((col (funcall (if (isNums txt) #'num! #'sym!) (incf at) txt))) 
        (push col all)
        (when (not (isIgnore txt))
          (if (isGoal txt) (push col y) (push col x))
          (if (isKlass txt) (setf klass col)))))))

(defmethod add ((i cols) row)
  "update x and y column headers from data in row. returns row"
  (dolist (col (cols-x i)    ) (add col (th row col)))
  (dolist (col (cols-y i) row) (add col (th row col))))

;### Data (for rows and cols)
(defstruct data rows cols)
(defun data! (src  &aux (i (make-data)))
  "create data from either a file called 'src' or a list `src'"
  (labels ((update (x) (add i x)))
    (if (stringp src) (with-file src #'update) (mapc #'update  src))
    i))

(defmethod add ((i data) x)
  "make `cols` (if currently missing) or update the cols and rows"
  (with-slots (cols rows) i
    (if cols 
      (push (add cols (if (row-p x) x (row! x))) 
            rows)
      (setf cols (cols! x)))))

(defmethod dists ((i data) (row1 row) (row2 row) &optional (cols (cols-x (data-cols i))))
  "returns 0..1"
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

