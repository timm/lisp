; vi: set ts=2 sw=2 sts=2 et :
(defpackage :xai (:use :cl))
(in-package :xai)

;;; ## Vars
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

(defvar *egs* nil)
(defvar *settings* nil)

;;; ## Lib
;; ### macros
(defmacro ? (x &optional (lst '*settings*))
  "alist accessor macro (defaults to querying *settings*)"
  `(cdr (assoc ',x ,lst :test #'equal)))

(defmacro aif (test then &optional else)
  "anaphoric if"
  `(let ((it ,test)) (if it ,then ,else)))

(defmacro geta (x lst &optional (init 0))
  "(1) ensure that `lst` includes (x num); (2) return the value of that cell"
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))

;; ### strings
(defun got (s c &optional (n 0))
  "is `s` a string holding `c` at position `n` (and `n` can be negative)?"
  (if (stringp s)
    (if (< n 0) 
      (got s c (+ (length s) n))
      (and (>= n 0) (< n (length s)) (eql c (char s n))))))

(defun trim (s) 
 "kill leading,trailing whitespace"
  (string-trim '(#\Space #\Tab #\Newline) s))

;; ### things
(defun thing (s &aux (s1 (trim s)))
  "coerce `s` into a number or string or t or nil or #\?"
  (cond ((equal s1 "?") #\?)
        ((equal s1 "t") t)
        ((equal s1 "nil") nil)
        (t (let ((n (read-from-string s1 nil nil))) 
             (if (numberp n) n s1)))))

(defun subseqs (s &optional (sep #\,) (filter #'thing) (here 0))
  "find subsequences from `s`, divided by `sep`, filtered through `filter`"
  (let* ((there (position sep s :start here))
         (word  (funcall filter (subseq s here there))))
    (labels ((tail () (if there (subseqs s sep filter (1+ there)))))
      (if (equal word "") (tail) (cons word (tail))))))

(defun with-lines (file fun &optional (filter #'subseqs))
  "Call `fun` for each line in `file`"
  (with-open-file (s file) 
    (loop (funcall fun (funcall filter (or (read-line s nil) (return)))))))

;; ### random
; Unlike Common Lisp, these  randoms let reset the seed.
(defvar *seed* 10013)

(defun rand (&optional (n 2))
  "Random float 0.. < n"
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 2) &aux (base 10000000000.0))
  "random int 0..n-1"
  (floor (* n (/ (rand base) base))))

;; ### settings
(defun settings (s &optional args)
  "for lines like '  -Key Flag ..... Default', return (KEY flag (thing Default))"
  (loop 
    :for (flag key . lst) 
    :in  (subseqs s #\NewLine (lambda (s1) (subseqs s1 #\Space #'trim)))
    :if  (got flag #\-) 
    :collect (cons (intern (string-upcase key)) 
                   (cli args flag (thing (car (last lst)))))))

(defun cli (args flag b4)
  "If `flag` is in `args`, then change  `b4` using command-line values; 
   if `b4` is a boolean, there is no need for command-line values-- just flip `b4`."
  (aif (member flag args :test 'equal)
    (cond ((eql b4 t) nil)
          ((eql b4 nil) t)
          (t (thing (second it))))
    b4))

(defun args () 
  "return command line flags"
  #+clisp ext:*args*  
  #+sbcl sb-ext:*posix-argv*)

(defun stop (&optional (x 0)) 
  "quit lisp"
  #+clisp (ext:exit x) 
  #+sbcl  (sb-ext:exit :code x))

;; ### egs
(defmacro eg (what doc &body body)
  "define an example"
  `(push (list ,what ,doc (lambda () ,@body)) *egs*))

(defun egs ()
  "run 'all' actions or just the (! action) action 
  (resetting random seed and other setting before each action)"
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
                (t                    (format t "❌ FAIL ~%")
                                      (incf fails))))))
    (stop fails)))

;### egs and help
(defun about ()
  "show the help string (built from *help* and the doc strings from *egs*"
  (format t "~a~%~%ACTIONS:~%" *help*)
  (dolist (x (reverse *egs*))
    (format t "  -g ~10a : ~a~%" (first x) (second x))))

;;; ## Data
(defun isSym    (x) (eql 'Sym (type-of x)))
(defun isNums   (s) (and (> (length s) 1) (upper-case-p (char s 0))))
(defun isGoal   (s) (or (isKlass s) (isLess s) (isMore s)))
(defun isIgnore (s) (got s #\X -1))
(defun isKlass  (s) (got s #\! -1))
(defun isLess   (s) (got s #\- -1))
(defun isMore   (s) (got s #\+ -1))

;### sym
(defstruct sym (at 0) (txt "") (n 0) has (w 1) mode (most 0))
(defun sym! (&optional (at 0) (txt ""))
  "summarizes streams of numbers"
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

(defstruct row cells y-used)
(defun row! (cells)
  "create something that holds `cells`"
  (make-row :cells cells))

(defmethod th ((r row) (c num))    (th r (num-at c)))
(defmethod th ((r row) (c sym))    (th r (sym-at c)))
(defmethod th ((r row) (n number)) (elt (row-cells r) n))

;### Cols
(defstruct cols all x y klass)
(defun cols! (lst &aux (i (make-cols)) (at -1))
  "factory for generating column headers from list of column names"
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

;; ### Data
(defstruct data rows cols)
(defun data! (src  &aux (i (make-data)))
  "create data from either a file called 'src' or a list `src'"
  (labels ((update (x) (add i x)))
    (if (stringp src) (with-lines src #'update) (mapc #'update  src))
    i))

(defmethod add ((i data) x)
  "make `cols` (if currently missing) or update the cols and rows"
  (with-slots (cols rows) i
    (if cols 
      (push (add cols (if (row-p x) x (row! x))) 
            rows)
      (setf cols (cols! x)))))

(defmethod dists ((i data) (row1 row) (row2 row) &optional (cols (cols-x (data-cols i))))
  "Returns 0..1"
  (let ((d 0) (n 1E-32))
    (dolist (col cols (expt (/ d n) (/ 1 (? p))))
      (incf d (expt (dist col (th row1 (slot-value col 'at)) (th row2 (slot-value col 'at))) 
                    (? p)))
      (incf n))))

(defmethod around ((i data) (row1 row) &optional (rows (data-rows i)) (cols (data-cols i)))
   (sort (mapcar (lambda (row2) (cons (dists i row1 row2 cols) row2)) rows) #'< :key #'cdr))

; (defmethod far ((i data) (row1 row &optional (rows (? i rows)) (cols (? i cols x))))
;   (cdr (elt (around i row1 rows cols) 
;             (floor (* (! far) (length rows))))))
;
;;; ## Demos
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
      (with-lines "../data/auto93.csv" 
                  (lambda (s)  (incf n (length s))))
      (eql  n 3192)))

(eg "num" "test number"  
    (let  ((n (num! 10 "num"))) 
      (dolist (i '(1 1 1 1 2 2 3)) (add n i))
      (and (equalp 11/7 (mid n)) (equalp 0.7867958 (div n)))))

(eg "sym" "test symbols"  
    (let ((s (sym! 10 "num"))) 
      (dolist (i '(a a a a b b c)) (add s i))
      (and (equalp 'a (mid s)) (equalp 1.3787835 (div s)))))

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

(setf *settings* (settings *help* (args)))
(if (? help) (about) (egs))
