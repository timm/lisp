#!/usr/bin/env sbcl --script
;;<!-- vim: set ft=lisp ts=2 et sw=2 : -->
;;<!-- vim: set lispwords+=loop,format,error,labels,aif : -->
;;<!-- vim: set lispwords+=handler-case : -->

;; -- Tim Menzies<br>timm@ieee.org<br>http://timm.fyi<br>Sept'26

;; The prose that explains this file lives in lib.md.
;; `make weave` pulls these docstrings over there.

;;; ---- script header -----------------------------------------

(defun brief-error (c h)
  "Avoid SBCL's long errr dump. Just show 1 line messages."
  (declare (ignore h))
  (format *error-output* "~&!! ~a~%"
    (substitute #\Space #\Newline (princ-to-string c)))
  (halt 1))

(defun halt (&optional (fails 0))
  "Exit, reporting FAILS. Zero exits quietly, with status zero."
  (when (> fails 0)
    (format t "~&ERROR: ~a failure~:p~%" fails)
    #+clisp (ext:exit fails) #+sbcl (sb-ext:exit :code fails)))

#+sbcl (declaim (sb-ext:muffle-conditions warning style-warning))
#+sbcl (setf sb-ext:*invoke-debugger-hook* #'brief-error)

;;; ---- macros ------------------------------------------------

(defmacro aif (test then &optional else)
  "Anaphoric if: THEN and ELSE read TEST's value as `it`; e.g.
   (aif (parse thing) (print it))"
  `(let ((it ,test)) (if it ,then ,else)))

(defmacro ? (x k &rest ks)
  "Dive through nested structs; e.g.
   (? x a b) ==> (slot-value (slot-value x 'a) 'b)"
  (if ks `(? (slot-value ,x ',k) ,@ks)
         `(slot-value ,x ',k)))

;; Dollar prefix reads a slot of the current struct, which must
;; be named `i`; e.g. `$x` ==> `(slot-value i 'x)`. Reader
;; macros have nowhere to hang a docstring, so this one is
;; documented in a comment.
(set-macro-character #\$
  (lambda (s c) (declare (ignore c))
    `(slot-value i ',(read s t nil t))))

(defmacro has (x lst)
  "Count X in alist LST, starting the count at zero if new; e.g.
   (let (seen)
     (mapc (lambda (x) (incf (has x seen))) '(a a b b b))
     seen) ==> ((b . 3) (a . 2))"
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x 0) ,lst))))))

(defmacro -> (&body b)
  "A short lambda whose args arrive as %1 to %5; e.g.
   (let (seen)
     (->> (incf (has %1 seen)) '(a a b b b))
     seen) ==> ((b . 3) (a . 2))

   That is the `has` example above, now a one-liner."
  `(lambda (%1 &optional %2 %3 %4 %5)
     (declare (ignorable %2 %3 %4 %5))
     ,@b))

(defmacro ->> (body &rest lists)
  "Mapcar BODY, as a short lambda, over LISTS."
  `(mapcar (-> ,body) ,@lists))

;;; ---- settings ----------------------------------------------

(defvar *settings* nil
  "Everything that tunes behaviour, as (name flag doc default).")

(defmacro my (x)
  "The current value of setting X, e.g. `(my seed)`."
  `(fourth (assoc ',x *settings*)))

(defun cli (b4 &aux (av (cli-args)) (bad 0))
  "Update settings B4 from the command line, then halt.
   `-s 42` sets one option and `--foo` runs `(eg--foo)`.
   Exit status is the number of examples that failed."
  (setf *settings* b4)
  (loop for flag = (pop av) while flag do
    (aif (find flag b4 :key #'second :test #'equal)
      (setf (fourth it) (thing (pop av)))
      (aif (cli-eg flag)
        (setf bad (cli-run it bad))
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
  "Call F on a fresh seed, reporting any error it raises.
   Returns FAILS, incremented if F blew up."
  (setf *seed* (my seed))
  (handler-case (progn (funcall f) fails)
    (error (e)
      (format t "~&!! ~(~a~): ~a~%" f
        (substitute #\Space #\Newline (princ-to-string e)))
      (1+ fails))))

(defun thing (str &aux (*read-eval* nil))
  "Coerce STR to a number, a boolean, `?`, or leave it alone."
  (let ((v (handler-case (read-from-string str nil :none)
             (error () :none))))
    (if (or (numberp v) (member v '(t nil ?))) v str)))

;;; ---- randoms -----------------------------------------------

(defvar *seed* 1234567891
  "State of the random number generator.")

(defun rand (&optional (n 1))
  "A random float in [0,N), from our own generator.
   Rolling our own keeps the stream identical across platforms."
  (setf *seed* (mod (* 16807 *seed*) 2147483647))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 100))
  "A random integer in [0,N)."
  (floor (* n (rand))))

(defun shuffle (lst &aux (v (coerce lst 'vector)))
  "A new list holding LST's items in random order.
   Copies to a vector first, for fast random access."
  (loop for i from (1- (length v)) downto 1 do
    (rotatef (aref v i) (aref v (rint (1+ i)))))
  (coerce v 'list))

;;; ---- misc --------------------------------------------------

(defun kv (&rest kvs)
  "Print KVS as key/value pairs, one pair per line."
  (loop for (k v) on kvs by #'cddr do
    (format t "~&~(~a~)~10t~s~%" k v)))

(defun csv (file)
  "The rows of FILE, each one a list of coerced cells."
  (with-open-file (s file)
    (loop for line = (read-line s nil) while line
      collect (csv-cells (string-right-trim '(#\Return) line)))))

(defun csv-cells (s &optional (sep #\,) (lo 0)
                  (hi (position sep s :start lo)))
  "Split S on SEP, coercing each cell with `thing`."
  (cons (thing (subseq s lo hi))
        (if hi (csv-cells s sep (1+ hi)))))
