;; vim: set ft=lisp ts=2 sw=2 et :
;; Once upon I wanted to write some code. 

;; --------------------------------------------------------------
;; ## *settings* : the settings

;; First, I told LISP to calm down about error reporting.
#+sbcl (declaim (sb-ext:muffle-conditions warning style-warning))

;; Then, I make space for any special settings (like my random
;; number geenerator).
(defvar *settings*  nil)

;; These settings are defined in lists with four items:   
;; 
;;      (name flag comment default)
;; 
;; For example
;;
;;      (defun defaults ()
;;        '((seed "-s" "random number seed" 1234567891)
;;          (p    "-p" "distance coeffecient" 2)))
;;
;; For simplicity's sake, we use a quick macro to find
;; `name`s in a setting 
(defmacro ! (x) `(fourth (assoc ',x *settings*)))

;; --------------------------------------------------------------
;; ## (cli) : the command-line inteface

;; Also, I defined a way to read updates to the settings 
;; from the  command-line interface `(args)`.  Anything like
;; `-s 42` sets an option and `--foo` runs a function `(eg--foo)`.
(defun cli (b4 &aux (av (args) (bad 0)))
  (loop for flag = (pop av) while flag do
        (let ((spec (find flag b4 :key #'second :test #'equal))
              (f    (egp flag)))
          (cond (spec (setf (fourth spec) (thing (pop av))))
                (f    (setf *settings* b4)
                      (setf bad (run f bad)))
                (t    (format t "?? ~a~%" flag)))))
  (stop bad))

;; This `(cli)` function needs uses helpers.
;;
;; - `(args)` returns the strings on the command line;
;; - `(thing)` ceoerce strings into numbers, booleans, or atoms;
;; - `(egp)` checks for functions assocaited with cli strings; 
;; - `(run)` calls that function, resetting the random seed 
;;   beforehand, and printing any errors afterwards.
;; - `(stop)`  returns the number of errors, then quits.
(defun args ()
  #+sbcl  (cdr sb-ext:*posix-argv*)
  #+clisp ext:*args*)

(defun thing (str &aux (*read-eval* nil))
  (let ((v (ignore-errors (read-from-string str ""))))
    (if (or (numberp v) (member v '(t nil ?))) v str)))

(defun egp (flag)
  (let ((f (intern (format nil "EG~:@(~a~)" flag))))
    (and (fboundp f) f)))

(defun run (f &optional (fails 0))
  (setf *seed* (! seed))
  (handler-case (progn (funcall f) fails)
    (error (e)
           (format t "~&!! ~(~a~): ~a~%" f 
                   (substitute #\Space #\Newline 
                               (princ-to-string e)))
           (1+ fails))))

(defun stop (&optional (status 0))
  (when  (> status 0) (print "ERROR")
    #+clisp (ext:exit status)
    #+sbcl  (sb-ext:exit :code status)))

;; --------------------------------------------------------------
;; ## (rand) : random number generator

;; To ensure we get the same stream of random numbers
;; on different platforms, we need control of their
;; generation.
(defvar *seed* 1234567891)

(defun rand (&optional (n 1))
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 100) &aux (base 1E10))
  (floor (* n (/ (rand base) base))))

;; This means we can do things like shuffle lists
;; (to  make things run fast, `(shuffle)` uses 
;; a temporary vector to allow fast random access).
(defun shuffle (lst &aux (v (coerce lst 'vector)))
  (loop for i from (1- (length v)) downto 1 do
    (rotatef (aref v i) (aref v (rint (1+ i)))))
  (coerce v 'list))

;; --------------------------------------------------------------
;; ## Misc Tricks

;; Anaphoic if: 1) trap slow conditions, 2) access them via `it`.
(defmacro aif (test then &optional else)
  `(let ((it ,test)) (if it ,then ,else)))

;; Short lambda; args %1 .. %5, all optional but %1.
(defmacro -> (&body b)
  `(lambda (%1 &optional %2 %3 %4 %5)
     (declare (ignorable %2 %3 %4 %5))
     ,@b))

;; Nested accessors.    
;; `(? x a b)` ==> `(slot-value (slot-value x 'a) 'b)`
(defmacro ? (x k &rest ks)
  (if ks `(? (slot-value ,x ',k) ,@ks)
         `(slot-value ,x ',k)))

;; Short-cut to slot access.
(set-macro-character #\$
  (lambda (s c) (declare (ignore c))
    `(slot-value i ',(read s t nil t))))

;; Alist place for X, adding (X . 0) if missing.
(defmacro has (x lst)
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x 0) ,lst))))))

;; print KEY VALUE pairs, one per line.
(defun kv (&rest kvs)
  (loop for (k v) on kvs by #'cddr do 
        (format t "~&~(~a~)~10t~s~%" k v)))

;; return rows from files
(defun read-rows (file)
  (labels ((read-row (s &optional (sep #\,))
                     (loop for lo = 0 then (1+ hi)
                           for hi = (position sep s :start lo)
                           collect (thing (subseq s lo hi))
                           while hi)))
    (with-open-file (s file)
      (loop for line = (read-line s nil) while line
            collect (read-row line)))))
