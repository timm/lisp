;; -- Tim Menzies<br>timm@ieee.org<br>http://timm.fyi<br>Sept'26

;; The prose that explains this file lives in lib.md.
;; `make weave` pulls these docstrings over there.

(defvar *settings* nil
  "What tunes me, as (name flag doc value).  `cli` sets it.")

;;;; script header 

(defvar *rand-seed* 1234567891
  "State of the random number generator.")

(defun oops (c &optional who)
  "One line for condition C, blamed on WHO."
  (format *error-output* "~&!! ~@[~(~a~): ~]~a~%" who
    (substitute #\Space #\Newline (princ-to-string c))))

(defun brief-error (c h) (declare (ignore h)) (oops c) (halt 1))

(defun halt (&optional (fails 0))
  "Exit, reporting FAILS. Zero exits quietly, with status zero."
  (when (> fails 0)
    (format t "~&ERROR: ~a failure~:p~%" fails)
    #+clisp (ext:exit fails) #+sbcl (sb-ext:exit :code fails)))

#+sbcl (declaim (sb-ext:muffle-conditions warning style-warning))
#+sbcl (setf sb-ext:*invoke-debugger-hook* #'brief-error)

;;;; macros 

(defmacro aif (test then &optional else)
  "Anaphoric if: THEN and ELSE read TEST's value as `it`; e.g.:
   (aif (parse thing) (print it))"
  `(let ((it ,test)) (if it ,then ,else)))

(defmacro ? (x k &rest ks)
  "Dive through nested structs; e.g.:
   (? x a b) ==> (slot-value (slot-value x 'a) 'b)"
  (if ks `(? (slot-value ,x ',k) ,@ks)
         `(slot-value ,x ',k)))

;; Dollar prefix reads a slot of the current struct, which must
;; be named `i`; e.g. `$x` ==> `(slot-value i 'x)`. Reader
;; macros have nowhere to hang a docstring, so this one is
;; documented in a comment.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\$
    (lambda (s c) (declare (ignore c))
      `(slot-value i ',(read s t nil t)))))

(defmacro has (x lst)
  "Count X in alist LST, starting the count at zero if new; e.g.:
   (let (seen)
     (mapc (lambda (x) (incf (has x seen))) '(a a b b b))
     seen) ==> ((b . 3) (a . 2))"
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x 0) ,lst))))))

(defmacro -> (&body b)
  "A short lambda whose args arrive as %1 to %5. 
  This makes (e.g.) the last example a one liner:
  (let (seen)
    (->> (incf (has %1 seen)) '(a a b b b))
    seen) ==> ((b . 3) (a . 2))"
  `(lambda (%1 &optional %2 %3 %4 %5)
     (declare (ignorable %2 %3 %4 %5))
     ,@b))

(defmacro ->> (body &rest lists)
  "Map BODY, as a short lambda, over LISTS.  `map`, not
   `mapcar`: vectors are sequences too."
  `(map 'list (-> ,body) ,@lists))

;;;; settings 

(defmacro my (x)
  "The current value of setting X, e.g. `(my seed)`."
  `(fourth (assoc ',x *settings*)))

(defun cli (maker &aux (av (cli-args)) (bad 0))
  "Run MAKER's settings against the command line; e.g.
  `-s 42` sets an option, `--foo` runs `(eg--foo)` then
  resets to MAKER's defaults.  Halts, with the number of
  failed examples as the exit status."
  (setf *settings* (funcall maker))
  (loop for flag = (pop av) while flag do
    (aif (find flag *settings* :key #'second :test #'equal)
      (setf (fourth it) (thing (pop av)))
      (aif (cli-eg flag)
        (progn (setf bad (cli-run it bad))
               (setf *settings* (funcall maker)))  ; fresh
        (format t "?? ~a~%" flag))))
  (halt bad))

(defun cli-args ()
  "The strings after the program name on the command line."
  #+sbcl  (cdr sb-ext:*posix-argv*)
  #+clisp ext:*args*)

(defun cli-eg (flag)
  "The example function named by FLAG, if one is defined."
  (aif (intern (format nil "EG~:@(~a~)" flag))
    (and (fboundp it) it)))

(defun cli-run (f &optional (fails 0))
  "Call F on a fresh seed; return FAILS, +1 if F blew up."
  (setf *rand-seed* (my seed))
  (handler-case (progn (funcall f) fails)
    (error (c) (oops c f) (1+ fails))))

(defun thing (str &aux (*read-eval* nil))
  "Coerce STR to a number, a boolean, `?`, or leave it alone."
  (let ((v (handler-case (read-from-string str nil :none)
             (error () :none))))
    (if (or (numberp v) (member v '(t nil ?))) v str)))

;;;; randoms 

(defun rand (&optional (n 1))
  "A random float in [0,N), from our own generator."
  (setf *rand-seed* (mod (* 16807 *rand-seed*) 2147483647))
  (* n (- 1.0d0 (/ *rand-seed* 2147483647.0d0))))

(defun rand-int (&optional (n 100))
  "A random integer in [0,N)."
  (floor (* n (rand))))

(defun shuffle (lst &aux (v (coerce lst 'vector)))
  "A new list holding LST's items in random order."
  (loop for i from (1- (length v)) downto 1 do
    (rotatef (aref v i) (aref v (rand-int (1+ i)))))
  (coerce v 'list))

;;;; misc 

(defun kv (&rest kvs)
  "Print KVS as key/value pairs, one pair per line."
  (loop for (k v) on kvs by #'cddr do
    (format t "~&~(~a~)~10t~s~%" k v)))

(defun csv (file)
  "The rows of FILE, each one a vector of coerced cells."
  (with-open-file (s (truename file))
    (loop for line = (read-line s nil) while line
      collect (coerce (csv-cells
                        (string-right-trim '(#\Return) line))
                      'vector))))

(defun csv-cells (s &optional (sep #\,) (lo 0)
                  (hi (position sep s :start lo)))
  "Split S on SEP, coercing each cell with `thing`."
  (cons (thing (subseq s lo hi))
        (if hi (csv-cells s sep (1+ hi)))))

(defun chars (s &optional (n 0))
  "Char N of S; a negative N counts back from the end; e.g.:
   (chars \"lisp\" 0) ==> #\\l
   (chars \"lisp\" -1) ==> #\\p"
   (if (plusp (length s)) (char s (mod n (length s)))))
