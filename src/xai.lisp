; vi: set ts=2 sw=2 sts=2 et :
;<font size=20pt><b>AI for busy people</b></font><br>
;**Tim Menzies**<br><timm@ieee.org><br>http://timm.github.io
(defpackage :xai (:use :cl))
(in-package :xai)

#| In the  21st-century, the problem is not accessing data but
ignoring irrelevancies.  Most people  can electronically access
mountains of data such as all transactions in the  past two years or
the complete state of the entire assembly line. The trick is effectively using
the available data. In practice, this means summarizing large data
sets to find the “pearls  in the dust"-- that is, the data that
really matters. 
<img align=right width=300  style="margin-top:40px;"
src="https://www.uklinkology.co.uk/wp-content/uploads/2022/02/Success-Stories.png">|#

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
  -m   max           max size of num cache      = 512
  -p   p             distance coeffecient       = 2
  -s   seed          random number seed         = 10013")

;From this help string, we fill in `*settings*` (and define the  command-line interface
;using the  `(settings)` function, see below)
(defvar *settings* nil)

;Where to store test functions.
(defvar *egs* nil)

;Common Lisp is infuriating: there is no simple way to set the random set. 
;Hence, we roll our own (and here's our  random number seed).
(defvar *seed* 10013)

;## Macros
;Stuff we need to put at the top.
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
    (let ((lst '((a . 100))))
      (incf (freq 'a lst))
      (incf (freq 'a lst))
      (incf (freq 'a lst))
      (equal 103 (? a lst))))

(eg "lines" "testing file reading"
    (let ((n 0))
      (with-file "../data/auto93.csv" 
                  (lambda (s)  (incf n (length s))))
      (eql  n 3192)))

(eg "num" "test number"  
    (let  ((n (make-num))) 
      (dolist (i '(1 1 1 1 2 2 3)) (add n i))
      (and (equalp 1 (mid n)) (<= 0.77 (div n) 0.78))))

(eg "nums" "test lots of number"  
    (let  ((n (make-num))) 
      (dotimes (i 10000)  (add n (rand)))
      (and (<= .49  (mid n) .51) (<= .31  (div n) .32))))

(eg "sym" "test symbols"  
    (let ((s (make-sym))) 
      (dolist (i '(a a a a b b c)) (add s i))
      (print (div s))
      (and (equalp 'a (mid s)) (<= 1.378 (div s) 1.379))))

(eg "cols" "create some columns"
    (print (cols '("Aas" "state" "Weight-")) t))

'(eg "data" "testing file reading"
    (print (cols-x (data-cols  (data! "../data/auto93.csv")))))

'(eg "dist" "dostance function" 
    (let* ((n -1) 
           (data (data! "../data/auto93.csv"))
           (row1 (first (data-rows data))))
      (format t "~%~a~%" (row-cells row1))
      (dolist (row2 (data-rows data) t) 
        (if (zerop (mod (incf n) 40)) 
          (format t "~a ~a ~a~%" n (row-cells row2) (dists  data row1 row2))))))

;## Structs
(defstruct sym 
  "summarizes streams of numbers"
  (at 0) (txt "") (n 0) has (w 1) mode (most 0))

(defstruct num 
  "summarizes streams of numbers"
  (at 0) (txt "") (n 0) (w 1) ok
  (has (make-array 5 :fill-pointer 0 :adjustable t))
  (lo most-positive-fixnum) (hi most-negative-fixnum))

(defstruct cols
  "holds sets of rows and columns"
  names all x y klass)

;## Data
(defun isNum   (s) (and (> (length s) 0) (upper-case-p (char s 0))))
(defun isGoal  (s) (or (isKlass s) (isLess s) (isMore s)))
(defun isKlass (s) (got s #\! -1))
(defun isLess  (s) (got s #\- -1))
(defun isMore  (s) (got s #\+ -1))
(defun isIgnored  (s) (got s #\? -1))

(defun cols (lst &aux (n 0) (col (make-cols :names lst)))
  (with-slots (all x y klass) col
    (dolist (txt lst col)
      (let ((col (funcall (if (isNum txt) 'make-num 'make-sym) :at (incf n) :txt txt)))
        (push col all)
        (if (isLess txt) (setf (slot-value col 'w) -1))
        (unless (isIgnored txt)
          (if (isKlass col) (setf klass col))
          (if (isGoal txt) (push col x) (push col y)))))))

;### sym
(defun add (col x &optional (inc 1))
  (unless (eql col #\?)
    (incf (slot-value col 'n) inc)
    (if (num-p col) 
      (add-num col x) 
      (add-sym col x (incf (freq x (sym-has col)) inc)))))

(defun add-sym (sym x n) 
  (with-slots (most mode) sym
    (if (> n most) (setf most n
                         mode x))))

(defun add-num (num x) 
  (with-slots (lo hi ok has n) num
      (setf lo (min lo x)
            hi (max hi x))
      (cond ((< (length has) (? max)) 
             (setf ok nil) (vector-push-extend x has))
            ((<= (rand) (/ (? max) n)) 
             (setf ok nil) (setf (aref has (rint (length has))) x)))))

(defun have (col)
  (if (and (num-p col) (not (num-ok col)))
    (sort (num-has col) #'<))
  (setf (num-ok col) t)
  (slot-value col 'has))

(defun mid (col)
   (if (num-p col) (per (have col) .5) (sym-mode col)))

(defun div (col)
  "diversity is stdev for nums, or entropy for syms"
  (if (num-p col) 
    (/ (- (per (have col) .9) (per (have col) .1)) 2.58)
    (with-slots (has n) col 
      (labels ((fun (p) (* -1 (* p (log p 2)))))
        (loop for (_ . n1) in has sum (fun (/ n1 n)))))))

;### num
(defun dist (data row1 row2 &optional (cols (cols-x (data-cols data))))
  (let ((n 0) (d 0))
    (dolist (col cols (expt (/ d  n) (/ 1 (? p))))
      (incf n)
      (incf d (expt (dist1 col (elt row1 (col-at col)) 
                               (elt row2 (col-at col))) 
                    (? p))))))

(defun dist1 (col x y)
  (if (and (equal x #\?) (equal x #\?)) 
    1
    (if (sym-p col)
      (if (equal x y) 0 1)
      (let ((x (norm col x))
            (y (norm col y)))
        (if (eq x #\?) (setf x (if (< y .5) 1 0)))
        (if (eq y #\?) (setf y (if (< x .5) 1 0)))
        (abs (- x y))))))

(defun norm (num x) 
  "map 'x' 0..1 (unless unknown, unless too small"
  (with-slots (lo hi) num
    (if (eq x #\?) x (/ (- x lo) (- hi lo 1e-32)))))

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
(defun per (seq &optional (p .5))
  (elt seq (floor (* (min .999999 (max 0 p)) (length seq)))))

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

;### Random

(defun rand (&optional (n 1))
  "random float 0.. < n"
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 1) &aux (base 10000000000.0))
  "random int 0..n-1"
  (floor (* n (/ (rand base) base))))

;### Settings
;If called with no `args`, then this just parses `*help*`. Otherwise, the defaults (from `*help*`) are updated from `args`.
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
                *seed*               (? seed))
          (format t "▶️  TEST: ~a " name)  
          (cond ((funcall (third x)) (format t "✅ PASS ~%"))
                (t                   (format t "❌ FAIL ~%")
                                     (incf fails))))))
    (stop fails)))

(defun about ()
  "show help string (built from *help* and the doc strings from *egs*"
  (format t "~a~%~%ACTIONS:~%" *help*)
  (dolist (x (reverse *egs*))
    (format t "  -g ~10a : ~a~%" (first x) (second x))))


;
;
;; Create something that holds `cells`s.
;(defstruct row cells y-used)
;(defun row! (cells)
;  (make-row :cells cells))
;
;(defmethod th ((r row) (c num))    (th r (num-at c)))
;(defmethod th ((r row) (c sym))    (th r (sym-at c)))
;(defmethod th ((r row) (n number)) (elt (row-cells r) n))
;
;;### Cols (factory
;(defstruct cols all x y klass)
;(defun cols! (lst &aux (i (make-cols)) (at -1))
;  "convert list of  column names to nums or syms"
;  (with-slots (all x y klass) i
;    (dolist (txt lst i)
;      (let ((col (funcall (if (isNums txt) #'num! #'sym!) (incf at) txt))) 
;        (push col all)
;        (when (not (isIgnore txt))
;          (if (isGoal txt) (push col y) (push col x))
;          (if (isKlass txt) (setf klass col)))))))
;
;(defmethod add ((i cols) row)
;  "update x and y column headers from data in row. returns row"
;  (dolist (col (cols-x i)    ) (add col (th row col)))
;  (dolist (col (cols-y i) row) (add col (th row col))))
;
;;### Data (for rows and cols)
;(defstruct data rows cols)
;(defun data! (src  &aux (i (make-data)))
;  "create data from either a file called 'src' or a list `src'"
;  (labels ((update (x) (add i x)))
;    (if (stringp src) (with-file src #'update) (mapc #'update  src))
;    i))
;
;(defmethod add ((i data) x)
;  "make `cols` (if currently missing) or update the cols and rows"
;  (with-slots (cols rows) i
;    (if cols 
;      (push (add cols (if (row-p x) x (row! x))) 
;            rows)
;      (setf cols (cols! x)))))
;
;(defmethod dists ((i data) (row1 row) (row2 row) &optional (cols (cols-x (data-cols i))))
;  "returns 0..1"
;  (let ((d 0) (n 1E-32))
;    (dolist (col cols (expt (/ d n) (/ 1 (? p))))
;      (incf d (expt (dist col (th row1 (slot-value col 'at)) (th row2 (slot-value col 'at))) 
;                    (? p)))
;      (incf n))))
;
;(defmethod around ((i data) (row1 row) &optional (rows (data-rows i)) (cols (data-cols i)))
;   (sort (mapcar (lambda (row2) (cons (dists i row1 row2 cols) row2)) rows) #'< :key #'cdr))
;
;## Start-up
(setf *settings* (settings *help* (args)))
(if (? help) (about) (egs))

;## License
; 
; Copyright 2023, Tim Menzies
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
; 
; 1. Redistributions of source code must retain the above copyright
; notice, this list of conditions and the following disclaimer.
; 
; 2. Redistributions in binary form must reproduce the above copyright
; notice, this list of conditions and the following disclaimer in the
; documentation and/or other materials provided with the distribution.
; 
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.
