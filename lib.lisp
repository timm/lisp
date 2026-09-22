#!/usr/bin/env sbcl --script 
;;<!-- vim: set ft=lisp ts=2 et sw=2 : -->
;;<!-- vim: set lispwords+=loop,format,error,labels,aif : -->
;;<!-- vim: set lispwords+=handler-case : -->

;; -- Tim Menzies<br>timm@ieee.org<br>http://timm.fyi<br>Sept'26

;; Here's a file of useful LISP script tips. It implements active
;; learning for explanatiobale multi-objective optimization. So
;; you can read this file as tutorial on either XAI or LISP

;; --------------------------------------------------------------
;; ## Preliminaries

;; TIP1 (installs): I recommend SBCL (for execution); rlwrap (for
;; debugging), nvim (for editting), pycco (for doc).

;; TIP2 (doco): to allow for listing in a technical document,
;; code should be 65 chars wide (max).

; TIP1 (scripting) the TUI 21. editor, command line LHS,
;; LLM RHS (claude code, codsx0.

;; TIP0: TUI21 is an example of a generatl principles:
;; Do things that prot to many languages. Always a next
;; language. When you move to it, what skills can you brng from
;; the past. e.g. test suites exampes.

;; TIP2: Write lo

;;; TIP3 (scripting): "Hash-bang" scripts are smart enough to know
;; how to execute themselves. To do this, the first step is tell
;; the file it is excutable (on Linux and Mac this is just `chmod
;; +x x.lisp`). The second step, on line one of the file, is to
;; list the interpreter:   
;; `#!/usr/bin/env sbcl --script`
;;
;; Once these two steps are completed, scripts can be execuited
;; usimg; e.g.   `./lib.lisp`

;; TIP3 (edit): for nvim users,  use `nvim --clean` for a sane
;; IDE that supports e.g. bracket matching. When using nvim,
;; update Also add some indent control
;; to lines 1 and 2; e.g.:  
;; `;;<!-- vim: set ft=lisp ts=2 sw=2 : -->`  
;; `;;<!-- vim: set lispwords+=loop,format,error,labels,aif: -->`

;; TIP4 (doco): for simple documentation, if all doc strings are
;; commented markdown outside of the functions, then simple
;; documentation can be generated via  
;; `pycco -l scheme -d ~/tmp x.lisp`

;; TIP5 (lisp): SBCL is faster than CLISP but CLISP is installed
;; by default if many sites. So I try to code for both. LISP
;; allows for this with the condictional execution forms (see all
;; the `#+sbcl` or `#+clisp` forms in my code).

;; TIP6 (lisp): SBCL internactive command line is broken (e.g.
;; cannot up arrow to last command). To fix this, when running
;; things at the LISP command line, use  
;; `rlwrap sbcl`

;; ## Script Header

;; TIP7 (lisp): Default SBCL is very verbose about crashes.
;; Tell it to calm down about its error messages.
(defun brief-error (c h)
	(declare (ignore h))
	(format *error-output* "~&!! ~a~%"
		(substitute #\Space #\Newline (princ-to-string c)))
	(halt 1))

(defun halt (&optional (fails 0))
	(when (> fails 0) 
		(format t "~&ERROR: ~a failure~:p~%" fails) 
		#+clisp (ext:exit fails) #+sbcl (sb-ext:exit :code fails)))

#+sbcl (declaim (sb-ext:muffle-conditions warning style-warning))
#+sbcl (setf sb-ext:*invoke-debugger-hook* #'brief-error)

;; ## Macros
;; TIP8 (lisp): Write macros near top-of-file so
;; everyone else can use their expansions.

;; TIP9 (lisp): Use macros sparingly-- they can make code harder
;; to understand. I find the following very useful.

;; The anaphoic if: 1) traps conditions that are slow to
;; recompute, 2) then allows you access those results via `it` .
;; e.g.  
;; `(aif (parse thing) (print it))`
(defmacro aif (test then &optional else)
  `(let ((it ,test)) (if it ,then ,else)))

;; Nested accessors let you dive into nested structs; e.g.   
;; `(? x a b)` ==> `(slot-value (slot-value x 'a) 'b)`
(defmacro ? (x k &rest ks)
  (if ks `(? (slot-value ,x ',k) ,@ks)
         `(slot-value ,x ',k)))

;; The dollar prefix macro makes it easier to access slots of
;; some current struct (that must be called `i` ); e.g.  
;; `$x ==> (slot-value i 'x)`
(set-macro-character #\$
  (lambda (s c) (declare (ignore c))
    `(slot-value i ',(read s t nil t))))

;; Alists can implement a simple counter for symbols `x,y,z..`,
;; initialized counters when required; e.g.
;;
;;     (let (seen)
;;       (mapc (lambda (x) (incf (has x seen)))
;;             '(a a b b b))
;;       seen) ==> ((b . 3) (a . 2))
(defmacro has (x lst)
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x 0) ,lst))))))

;; The arrow macro makes it simpler to use lambda. Using the
;; arrow macro, the above example of `has` is now very succinct.
;;
;;     (let (seen)
;;       (->> (incf (has %1 seen)) '(a a b b b))
;;       seen) ==> ((b . 3) (a . 2))
(defmacro -> (&body b)
  `(lambda (%1 &optional %2 %3 %4 %5)
     (declare (ignorable %2 %3 %4 %5))
     ,@b))

(defmacro ->> (body &rest lists) `(mapcar (-> ,body) ,@lists))
;; ## Settings and Tests
;; TIP9 (scripting): Avoid magic numbers buried in your code.
;; Anything that controls behaviour should be in `*settings`
;; and update-able from the command line. 

;; In my `*settings` each setting has four parts:

;;     (name flag documentation default)

;; For example
;;
;;     (defun defaults ()
;;       '((seed "-s" "random number seed"   1234567891)
;;         (p    "-p" "distance coeffecient" 2)))
;;
;; To manipaute these settings:
;;
;; - `my` is a macro to qucikly access a setting.
;; - `(cli b4)` updates settings from command-line `(cli-args)`.
;;   `-s 42` sets an option and `--foo` runs a function
;;   `(eg--foo)` .
;;
;; `(cli)` uses some helpers:
;;
;; - `(cli-args)` returns the strings on the command line;
;; - `(cli-eg)` checks for functions assocaited with cli strings;
;; - `(cli-run)` calls that function, resetting the random seed
;;   beforehand, and printing any errors afterwards.
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
  (halt bad))

(defun cli-args ()
  #+sbcl  (cdr sb-ext:*posix-argv*)
  #+clisp ext:*args*)

(defun cli-eg (flag)
  (aif (intern (format nil "EG~:@(~a~)" flag))
    (and (fboundp it) it)))

(defun cli-run (f &optional (fails 0))
  (setf *seed* (my seed))
  (handler-case (progn (funcall f) fails)
    (error (e) 
      (format t "~&!! ~(~a~): ~a~%" f 
        (substitute #\Space #\Newline (princ-to-string e)))
      (1+ fails))))

;; Finally, when reading arguments from the command line, the 
;; `(thing)` function ceoerces strings to nums,bools, or atoms.
(defun thing (str &aux (*read-eval* nil))
  (let ((v (handler-case (read-from-string str nil :none)
             (error () :none))))
    (if (or (numberp v) (member v '(t nil ?))) v str)))

;; TIPS10 (scripting): Your code needs to ship with demos that 
;; show of the code and which print error messages if anything
;; is broken. From the command line, we should be able to call
;; one or all of them. In this code, all the demos start with 
;; `eg-`. e.g.    

;;     (defun eg-settings () (print *settings*))



;; --------------------------------------------------------------
;; ## (rand) : random number generator

;; To ensure we get the same stream of random numbers on
;; different platforms, we need control of their generation.
(defvar *seed* 1234567891)

(defun rand (&optional (n 1))
  (setf *seed* (mod (* 16807 *seed*) 2147483647))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 100))
  (floor (* n (rand))))

;; This means we can do things like shuffle lists (to make things
;; run fast, `(shuffle)` uses a temporary vector to allow fast
;; random access).
(defun shuffle (lst &aux (v (coerce lst 'vector)))
  (loop for i from (1- (length v)) downto 1 do
    (rotatef (aref v i) (aref v (rint (1+ i)))))
  (coerce v 'list))

;; --------------------------------------------------------------
;; ## Misc Tricks

;; print KEY VALUE pairs, one per line.
(defun kv (&rest kvs)
  (loop for (k v) on kvs by #'cddr do 
    (format t "~&~(~a~)~10t~s~%" k v)))

;; return rows from files
(defun csv (file)
  (with-open-file (s file)
    (loop for line = (read-line s nil) while line
      collect (csv-cells (string-right-trim '(#\Return) line)))))

(defun csv-cells (s &optional (sep #\,) (lo 0)
                  (hi (position sep s :start lo)))
  (cons (thing (subseq s lo hi))
        (if hi (csv-cells s sep (1+ hi)))))


