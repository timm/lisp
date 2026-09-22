;; <!-- vim: set ft=lisp ts=2 sw=2 et lispwords+=loop,format,error,labels: -->  
;; Once upon I wanted to write some code. 
;; First, I told LISP to calm down about error reporting.
#+sbcl (declaim (sb-ext:muffle-conditions warning style-warning))
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

;; ## *settings* : the settings
;; My settings are defined in lists with four items:   
;; 
;;      (name flag comment default)
;; 
;; For example
;;
;;      (defun defaults ()
;;        '((seed "-s" "random number seed" 1234567891)
;;          (p    "-p" "distance coeffecient" 2)))
;;
;; To manipaute these settings:
;;
;; - They are stored in `*settings*`;
;; - `??` is a macro to qucikly access a setting.
;; - `(cli b4)` updates settings from command-line `(cli-args)`.  
;;   `-s 42` sets an option and    
;;   `--foo` runs a function `(eg--foo)`.
;;    
;; `(cli)` uses some helpers:
;;  
;; - `(cli-args)` returns the strings on the command line;
;; - `(cli-eg)` checks for functions assocaited with cli strings; 
;; - `(cli-run)` calls that function, resetting the random seed 
;;     beforehand, and printing any errors afterwards.
;; - `(cli-stop)` quits, with the number of errors as exit code.
(defvar *settings*  nil)
(defmacro my (x) `(fourth (assoc ',x *settings*)))

(defun cli (b4 &aux (av (cli-args)) (bad 0))
  (setf *settings* b4)
  (loop for flag = (pop av) while flag do
    (aif (find flag b4 :key #'second :test #'equal)
         (setf (fourth it) (thing (pop av)))
         (aif (cli-eg flag)
              (setf bad (cli-run it bad))
              (format t "?? ~a~%" flag))))
  (cli-stop bad))

(defun cli-args ()
  #+sbcl  (cdr sb-ext:*posix-argv*)
  #+clisp ext:*args*)

(defun cli-eg (flag)
  (let ((f (intern (format nil "EG~:@(~a~)" flag))))
    (and (fboundp f) f)))

(defun cli-run (f &optional (fails 0))
  (setf *seed* (my seed))
  (handler-case (progn (funcall f) fails)
    (error (e) 
      (format t "~&!! ~(~a~): ~a~%" f 
        (substitute #\Space #\Newline (princ-to-string e)))
      (1+ fails))))

(defun cli-stop (&optional (fails 0))
  (when (> fails 0)
    (format t "~&ERROR: ~a failure~:p~%" fails)
    #+clisp (ext:exit fails)
    #+sbcl  (sb-ext:exit :code fails)))

;; Finally `(thing)` ceoerce strings to nums, bools, or atoms.
(defun thing (str &aux (*read-eval* nil))
  (let ((v (handler-case (read-from-string str nil :none)
             (error () :none))))
    (if (or (numberp v) (member v '(t nil ?))) v str)))

;; --------------------------------------------------------------
;; ## (rand) : random number generator

;; To ensure we get the same stream of random numbers
;; on different platforms, we need control of their
;; generation.
(defvar *seed* 1234567891)

(defun rand (&optional (n 1))
  (setf *seed* (mod (* 16807 *seed*) 2147483647))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 100))
  (floor (* n (rand))))

;; This means we can do things like shuffle lists
;; (to  make things run fast, `(shuffle)` uses 
;; a temporary vector to allow fast random access).
(defun shuffle (lst &aux (v (coerce lst 'vector)))
  (loop for i from (1- (length v)) downto 1 do
    (rotatef (aref v i) (aref v (rint (1+ i)))))
  (coerce v 'list))

;; --------------------------------------------------------------
;; ## Misc Tricks

;; Alist place for X, adding (X . 0) if missing.
(defmacro has (x lst)
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x 0) ,lst))))))

;; print KEY VALUE pairs, one per line.
(defun kv (&rest kvs)
  (loop for (k v) on kvs by #'cddr do 
    (format t "~&~(~a~)~10t~s~%" k v)))

;; return rows from files
(defun csv (file)
  (labels ((csv-row (s &optional (sep #\,))
                    (loop for lo = 0 then (1+ hi)
                      for hi = (position sep s :start lo)
                      collect (thing (subseq s lo hi))
                      while hi)))
    (with-open-file (s file)
      (loop for line = (read-line s nil) while line collect
        (csv-row (string-right-trim '(#\Return) line))))))
