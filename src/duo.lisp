;#!/usr/bin/env clisp -q
; vim: filetype=lisp: ts=2 sw=2 sts=1  et :

(defun doc (str &key toc))
(doc "# Start up stuff")

(defun l() 
  "Quick reload of system (with fewest load warnings)."
  (handler-bind ((style-warning #'muffle-warning)) 
    (load "duo")))

;---------.---------.---------.---------.--------.---------.----------
(doc "# Unit Tests")

(defstruct tests all)
(defparameter *tests* (make-tests))

(defmethod build ((te tests) x)
	(pushnew x (tests-all te) :test #'equalp))

(defmacro eg (&body body) 
  "Add a new test. e.g. 

      (eg (equal t nil))

  (And note that if this test fails, and a failure is reported,
  then the test engine is working.)
  "
  `(build *tests* ',@body))

(defmethod run ((te tests))
  "Run all the tests."
	(let (fail (n 0) (y 0))
		(dolist (one (tests-all te))
			(cond ((eval  one) (incf y))
						(t           (incf n)
												 (push one fail))))
		(when fail
			(mapc #'print fail)
			(format t "~%PASS = ~s FAIL = ~s~%"  y n))))


(eg (equal t nil))

;---------.---------.---------.---------.--------.---------.----------
(doc "Version-specific code.")

(defun stop ()
  "Halt (for sbcl or clisp)."
  #+sbcl (sb-ext:exit)
  #+:clisp (ext:exit))

(defun cli ()
  "Get command line options  (for sbcl or clisp)."
  #+clisp ext:*args*
  #+sbcl sb-ext:*posix-argv*)

(defun the-slots (it)
  "Get slot names of the `it` struct, or class, (for sbcl or clisp)."
  #+clisp (class-slots (class-of it))
  #+sbcl (sb-mop:class-slots (class-of it)))

;---------.---------.---------.---------.--------.---------.----------
; macros

(defmacro aif (test then &optional else)
	`(let ((a ,test)) (if a ,then ,else)))

(defmacro while (test &body body)
	`(do () ((not ,test)) ,@body))

(defmacro whale (test &body body )
	`(let (a)
		 (while (setf a ,test) ,@body)))

(defmacro doitems ((one n list &optional out) &body body )
	`(let ((,n -1))
		 (dolist (,one ,list ,out) (incf ,n) ,@body)))

(eg (let (out) 
			(doitems (x pos '(a b c) out) 
				(push `(,pos ,x) out))))

(defmacro do-hash ((key value  h &optional out) &body body )
	`(progn (maphash #'(lambda (,key ,value) ,@body) ,h) ,out))

(defmacro ? (obj first-slot &rest more-slots)
	(if (null more-slots)
		`(slot-value ,obj ',first-slot)
		`(? (slot-value ,obj ',first-slot) ,@more-slots)))

(defmacro ?? (&rest x) `(? *O* ,@x))

(defmacro do-csv ((it &optional f out ) &body body)
  (let ((str (gensym)))
    `(let (,it) 
       (if ,f
         (with-open-file (,str ,f)
           (while (setf ,it  (read-line ,str nil))
             (setf ,it (s->words ,it))
             ,@body))
         (while (setf ,it (read-line *standard-input* nil))
             (setf ,it (s->words ,it))
             ,@body))
       ,out)))

(eg (let ((n 0))
			(do-csv (x "duo.lisp" (> n 0)) 
				(incf n (length x)))))

;---------.---------.---------.---------.--------.---------.----------
(defstruct ch!
  (meta  #\%)
  (less  #\<)
  (num   #\$)
  (more  #\>)
  (klass #\!)
  (skip  #\?)
  (sep   #\,))

(defstruct lsh!
  (poles 10)
  (trim  0.95))

(defstruct rands 
	(seed 			10013) 
	(seed1			10013) 
	(multiplier 16807.0d0)
	(modulus    2147483647.0d0))

(defstruct control 
  (rand (make-rands))
  (lsh  (make-lsh!))
  (ch   (make-ch!)))

(defun cli2options (thing)
  (dolist (x (cli) thing)
     (aif (find (format nil ":~a" x) (the-slots thing) :test #'equalp)
       (setf (slot-value thing x) 
             (read-from-string (nth (+ 1 a ) thing))))))

(defvar *O* (make-control))

(eg (eql #\% (?? ch meta)))

;---------.---------.---------.---------.--------.---------.----------
(doc "## Basic tools")

(defun show (&rest lst) (print lst))

(defun print-list(lst sep  str)
  (format str "~a" (car lst))
  (dolist (x (cdr lst)) (format str "~a ~a" sep x)))

(defun list-string(lst &optional (sep ","))
  (with-output-to-string (str) (print-list lst sep str)))

(eg (equalp "A, B, C" (list-string '(a b c))))

(defun ifs (flags lst)
	(if lst
		(if (car flags)
			(cons (car lst)
						(ifs (cdr flags) (cdr lst)))
			(ifs (cdr flags) (cdr lst)))))

(defun has (x &rest lst) 
  (dolist (y lst)
    (let ((x (if (stringp x) x (format nil "~a" x))))
      (if (find y x :test #'equal) 
        (return t)))))

(let ((whitespace '(#\, #\space #\tab #\newline)))
  (defun s->words (s &optional (sep whitespace))
    (with-input-from-string (str s)
      (let (tmp out)
        (labels 
          ((end-of-word () 
            (when tmp
              (push (concatenate 'string (reverse tmp)) out) 
              (setf tmp nil)) 
              out))
          (whale (read-char str nil)
                 (if (member a sep :test #'eql)
                     (end-of-word)
                     (push a tmp)))
          (reverse (end-of-word)))))))

(eg (equalp '("123" "456" "789") 
             (s->words "123,456 ,789  , ")))

(defun rand-reset (&optional seed)
	(setf (?? rand seed1) (or seed (?? rand seed))))

(defun rand (&optional (n 1)) 
	(with-slots (seed1 modulus multiplier) (?? rand)
		(setf seed1 (mod (* multiplier seed1)  modulus))
    (* n (- 1.0d0 (/ seed1 modulus)))))

(defun randi (&optional (n 100)) 
	(floor (* n (/ (rand 1000.0) 1000))))

(defun string-lines (str)
  "Convert a string to a list of lines."
  (labels 
    ((nl    (z)    (char= z #\Newline))
     (where (pos0) (position-if #'nl str :start pos0))
     (worker (pos0 &aux pos)
        (if (setf pos (where pos0))
            (cons (subseq str pos0 pos) 
                  (worker (1+ pos)))
            (list (subseq str pos0)))))
    (worker 0)))

(defun reads (f &optional  (fn #'print) (str t))
	"Read  a file, calling 'fn' on each s-expression. "
	(with-open-file (s f)
		(labels 
			((worker (&optional (one (read s nil :eof)))
							 (unless (eq  one :eof)
								 (funcall fn  one str)
								 (worker))))
			(worker))))

;---------.---------.---------.---------.--------.---------.----------
(doc "## Documentation (Lisp to Markdown)")

(setf +header+ "
[![](https://raw.githubusercontent.com/timm/ish/master/etc/img/banner.png)](https://github.com/timm/ish/blob/master/README.md)<br>
[home](http://git.io/ish)
| [code](https://github.com/timm/ish/tree/master/src)
| [doc](https://github.com/timm/ish/blob/master/src/README.md)
| [discuss](https://github.com/timm/ish/issues)
| [contribute](https://github.com/timm/ish/blob/master/CONTRIB.md)
| [cite](https://github.com/timm/ish/blob/master/CITATION.md)
| [&copy; 2018](https://github.com/timm/ish/blob/master/LICENSE.md)

[![](https://zenodo.org/badge/doi/10.5281/zenodo.1172230.svg)](https://github.com/timm/ish/blob/master/CITATION.md)

")

(defun fundoc (x s)
  "Takes the function documentation string and
  prints it, indented by a little white space"
  (labels ((defp     () (member (first x) '(defun defmacro defmethod)))
           (docp     () (eql    (first x)  'doc))
           (secret   () (char= #\_ (elt (symbol-name (second x)) 0)))
           (commentp () (and    (defp)
                                (> (length x) 3)
                                (stringp (fourth x))
                                (not (equal "" (fourth x))))))
    (if (docp)
     (format s "~%~a~%"  (second x)))
    (if (and (commentp) (not (secret)))
     (format s "~%### ~(~a~) ~%~%~a~%"  
                  (cons (second x) (third x)) (fourth x)))
               ))

(defun lisp2md (&optional (in "duo.lisp") (out "/tmp/duo.md")) 
  "Generates a Markdown file from the lisp code."
  (with-open-file (sout  out  :direction :output :if-exists :overwrite)
		(format sout "~a" +header+) 
		(format sout "# ~a ~%" in)
		(reads in #'fundoc sout)
    (terpri sout)))

;(lisp2md)

; sbcl --noinform --eval "(progn (format t "~&~a~%" 1))"

;---------.---------.---------.---------.--------.---------.---------
; symbols
(defstruct sym
  (counts (make-hash-table :test #'equal))
  (n 0) (pos 0) (txt "") (w 1)
  (ent 0)
  (most 0)
  mode)

(defmethod spread ((s sym)) (ent s))
(defmethod mid ((s sym)) (sym-mode s))
(defmethod prep ((s sym) x) x)

(defmethod update ((s sym) x)
  (with-slots (most mode counts  n) s
    (incf n)
    (let ((new (incf (gethash x counts 0))))
      (if (> new most)
        (setf most new
              mode x)))
    x))

(defmethod dec ((s sym) x)
  (with-slots (n counts) s
    (when (> (gethash x counts 0) 0)
      (decf (gethash x counts 0))
      (decf n))))

(defmethod ent ((s sym) &aux (e 0))
  (with-slots (counts n) s
    (do-hash (k v counts e)
      (when (> v 0)
				(let ((p (/ v n)))
					(decf e (* p (log p 2))))))))

(defmethod dist ((s sym) s1 s2)
  (labels ((no (x) (eql x (?? ch skip))))
    (if (and (no s1) (no s2))
      1
      (if (eql s1 s2) 0 1))))

;---------.---------.---------.---------.--------.---------.----------
; numbers
(defstruct num 
  (n 0) (pos 0) (txt "") (w 1)
  (mu 0.0) (m2 0.0) (sd 0.0)
  (lo most-positive-fixnum)
  (hi most-negative-fixnum))

(defmethod spread ((s num)) (num-sd s))
(defmethod mid ((s num)) (num-mu s))
(defmethod prep ((s num) x) (if (numberp x) x (read-from-string x)))

(defmethod update ((nu num) (x string))
  (update nu (prep nu x)))

(defmethod update ((nu num) (x number))
  (with-slots (n lo hi mu m2) nu
    (let ((delta (- x mu)))
      (setf n  (+ 1 n)
            lo (min lo x)
            hi (max hi x)
            mu (+ mu (/ delta n))
            m2 (+ m2 (* delta (- x mu))))
      (sd-prim nu))
    x))

(defmethod dec ((nu num) x)
  (with-slots (n mu m2) nu
    (let ((delta (- x mu)))
      (setf n  (- n 1)
            mu (- mu (/ delta n))
            m2 (- m2 (* delta (- x mu))))
      (sd-prim nu))))

(defmethod sd-prim ((nu num))
  (with-slots (sd n m2) nu
    (setf sd (cond ((< n 2)  0)
                   ((< m2 0) 0)
                   (t  (sqrt (/ m2 (- n 1))))))))

(defmethod norm ((nu num) x)
  (with-slots (lo hi) nu
    (/ (- x lo) (+ (- hi lo) (/ 1 most-positive-fixnum)))))

(defmethod dist ((nu num) n1 n2)
  (labels ((no (x) (eql x (?? ch skip))))
    (cond ((and (no n1) (no n2)) (return-from dist 1))
          ((no n1) (setf n2 (norm nu n2)
                         n1 (if (< n1 0.5) 1 0)))
          ((no n2) (setf n1 (norm nu n1)
                         n2 (if (< n2 0.5) 1 0)))
          (t       (setf n1 (norm nu n1)
                         n2 (norm nu n2))))
    (abs (- n1 n2))))

(defun _updateDec (lst)
	(let ((n (if (numberp (car lst)) (make-num) (make-sym))))
		(let* ((max     (length lst))
					 (min     5)
					 (diff 	  0)
					 (results (make-hash-table)))
			(loop 	
				for i from 0 to (1- max)  do
				(setf (gethash i results) (spread n))
				(update n (nth i lst)))
			(loop 
				for i from (1- max)  downto min do
				(dec n (nth i lst))
				(incf diff (- (spread n) (gethash i results))))
			(< (abs diff) 0.0001))))

(eg (progn
			(rand-reset)
			(_updateDec
				(loop for i from 1 to 1000 collect (rand)))))

(eg (progn
			(rand-reset)
			(_updateDec
				(loop for i from 1 to 1000 collect 
							(format nil "_~a" (randi 10))))))

;--------d.---------.---------.---------.--------.---------.----------
(defun xpect (f &rest lst)
	"expected value of, e.g., spread"
	(let ((sum 0) 
        (n   0))
		(dolist (one lst (/ sum n))
			(incf n   (? one n))
			(incf sum (* (? one n) (funcall f one))))))

(defun all (lst &optional how)
  (dolist (one lst how)
	  (setf how (or how 
                  (if (numberp one) (make-num) (make-sym))))
    (update how one)))

(eg (let ((x (all '( y y y y y y y y n n n n n))))
			(< 0.961 (spread x) 0.962)))

(eg (let ((x (all '(9 2 5 4 12 7 8 11 9 3 
                    7 4 12 5 4 10 9 6 9 4))))
        (and (< 3.05 (? x sd) 3.07)
						 (< 6.99 (? x mu) 7.01))))

;---------.---------.---------.---------.--------.---------.----------
; table columns
(defstruct cols all klass goals names indep nums syms meta)

(defmethod goalp ((c cols) x) (has x (?? ch klass) (?? ch less) (?? ch more)))
(defmethod nump  ((c cols) x) (has x (?? ch num)   (?? ch less) (?? ch more)))
(defmethod klassp((c cols) x) (has x (?? ch klass)))
(defmethod metap ((c cols) x) (has x (?? ch meta)))
(defmethod lessp ((c cols) x) (has x (?? ch less)))
(defmethod skipp ((c cols) x) (has x (?? ch skip)))

(defmethod build ((c cols) lst)
	"assumes all skipped columns are filtered before we get here"
	(with-slots (indep klass all names goals meta nums syms) c
		(doitems (x pos lst)
			(push x names)
			(if (klassp c x) (setq klass pos))
			(let* ((w    (if (lessp c x) -1 1))
						 (todo (if (nump c x) #'make-num #'make-sym))
						 (col  (funcall todo :pos pos :txt x :w w)))
				(push col all)
				(if (nump c x) (push col nums)  (push col syms))
				(if (metap c x)
					(push col meta)
					(if (goalp c x) (push col goals) (push col indep)))))
		(setf all (reverse all))))

(eg (build (make-cols) '($a   <e !f)))

(defmethod update ((cs cols) lst)
  (dolist (col (? cs all) lst)
    (setf (nth (? col pos) lst)
          (update col (nth (? col pos) lst)))))

;---------.---------.---------.---------.--------.---------.----------
; data has many rows and coumns
(defstruct row cells poles)
(defstruct data  rows (cols (make-cols)) (npoles 0))

(defmethod update ((d data) lst)
  (if (? d cols names) 
    (let ((tmp (update (? d cols) lst)))
      (push (make-row :cells tmp) (data-rows d)))
    (build (? d cols) lst))) 

(defmethod skipp ((d data) lst)
	(mapcar (lambda (x) (skipp (? d cols) x)) lst))

(defmethod without ((d data) skips lst)
	(if lst
		(if (car skips)
			(without d (cdr skips) (cdr lst))
			(cons (car lst)
						(without d (cdr skips) (cdr lst))))))

(defmethod readd ((d data) &optional file )
	(let (bad)
		(do-csv (cells file)
			(setf bad (or bad (skipp d cells)))
			(update d (without d bad cells)))))

(defmethod print-object ((d data) str)
  (print-list (? d cols names)  ","  str)
  (dotimes (i  (? d npoles)) 
    (format str ", %pole~a" i))
  (terpri str)
  (dolist (row (? d rows))
    (print-list (coerce (? row cells) 'list)  ","  str)
    (if (? row poles)
      (print-list (? row poles)  ","  str)
      (dotimes (i  (? d npoles)) 
        (format str ",0")))
    (terpri str)))

; make do-csv for csv. do s->words inside it
; a comment
(eg 
  (let ((d (make-data)))
     (readd d "../data/weather.csv")
     (and (eql 13 (length (? d rows)))
          (< 10.33 (spread (second (? d cols all))) 10.34))))

(run *tests*)
