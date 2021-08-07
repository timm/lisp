; vim:  ts=2 sw=3 sts=2 et :

; Macros
; ------
; After calling `(reader #\c fun)`, then if
; ever we see c(sex) then (sexp) will be
; passes to `fun`.
(defmacro reader (com fun)
  `(set-macro-character ,com 
     #'(lambda (str _) (declare (ignore _)) (,fun (read str t nil t)))))

; Shorthand for recursive calls to slot-value.
(defmacro ? (s x &rest xs) 
  (if xs `(? (slot-value ,s ',x) ,@xs) `(slot-value ,s ',x)))

; Shorthand for recursive calls to getf
(defmacro ! (s x &rest xs) 
  (if xs `(! (getf ,s ',x) ,@xs) `(getf ,s ',x)))

; Anaphoric if (and `it` is the anaphoric variable).
(defmacro aif (test yes &optional no) 
  `(let ((it ,test)) (if it ,yes ,no)))

; Anaphoric while (and `now` is the anaphoric variable).
(defmacro while (expr &body body) 
  `(do ((now ,expr ,expr)) ((not now)) ,@body))

; Colors
; -------
; all colors
(defun color (s c &optional (str t))
  (let ((all '((black . 30) (red . 31) (green . 32)  (yellow . 33) 
               (blue . 34)  (magenta . 35) (cyan . 36) (white .37))))
    (format str "~c[~a;1m~a~c[0m" #\ESC (cdr (assoc c all)) s #\ESC)))

; Red, green, yellow
(defun red (s) (color s 'red nil))
(defun green (s) (color s 'green nil))
(defun yellow (s) (color s 'yellow nil))

; System Stuff
; ------------
; Exit LISP.
(defun halt (&optional (status 0)) 
  (sb-ext:exit :code status))

; Get command line
(defun argv () sb-ext:*posix-argv*)

; Command-Line Interface
; ----------------------
; Given a plist with `flags`, if  the command line
; has any of the same keys, then update `flags` with the
; command-line values.If a flag is `all` then print some help.
; Flags are `grouped` and the the default group is `all`.
; To change groups, just mention it before mentioning
; the flags in that group.
(defun cli (flags &key 
            (help   "help")
            (args   (cdr (deepcopy (argv))))
            (group  (getf flags 'all)))
   (while (pop args)
     (setf now (read-from-string (remove #\- now)))
     (cond ((getf group now)   (assert (not (null args)) (args))
                               (setf (getf group now) 
                                     (read-from-string (pop args))))
           ((getf flags now)   (setf group (getf flags now)))
           ((member now group) (setf (getf group now) t))
           ((equalp now 'h)   (format t "~a~%" help))))
   flags)

; Just to give an example of its use, suppose we run `eg-cl`
; with this command line:
;
; `lisp tricks.lisp -loud --dom -k 23 -samples 1`
;
; The  dashes at the start of the flags are  for human
; readers (internally, the code just deletes them).
; Since the default group is `all`, then the
; intiial referece to `loud` is really `all loud`.
; Note how once we change goupds (with `dom` then
; we can set multiple flags in that group.
;
; This will set `loud` to true and update the other flags: 
;
;     > (print (cli '(all (tries 0
;                          fails 0
;                          un    nil
;                          seed  10013
;                          data  "../data/aaa.csv"
;                          loud  nil)
;                     col (p 2)
;                     dom (samples 100 
;                          k 23))))
;
;      (ALL (FAILS 0  
;            TRIES 0 
;            SEED 11 
;            DATA "../data/aa" 
;            LOUD T  
;            UN NIL) 
;       COL (P 2) 
;       DOM (SAMPLES 100 
;            K 23)) 
;
(defun eg.cli(_)
  "demo cli"
  (pprint 
    (cli '(all (fails 0  tries 0
                seed 10013  
                data "../data/aa" 
                loud nil un nil)
           col (p 2)
           dom (samples 100 
                k        23)))))

; Random Numbers
; --------------
; I confess that I never found a way to do
; platform independent random number generation with
; CommonLisp. So I write my own.

(defvar *seed* 10013)

; Return a random integer 0.. n-1.
(defun randi (&optional (n 1)) 
  (floor (* n (/ (randf 1000000.0) 1000000))))

; Return a random flaot 0..n-1.
(defun randf (&optional (n 1.0)) 
  (let ((multiplier 16807.0d0)
        (modulus    2147483647.0d0))
    (setf *seed* (mod (* multiplier *seed*) modulus))
    (* n (- 1.0d0 (/ *seed* modulus)))))

; Meta
; ----
; Does a symbol name start with `b4`?
(defun b4-sym (b4 sym &aux (n (length b4)) (s (symbol-name sym)))
  (and (>= (length s) n) 
       (equalp b4 (subseq s 0 n))))
 
; Deepcopy
(defun deepcopy (x)
   (if (atom x) x (mapcar #'deepcopy x)))

; Returns all functions in a package.
(defun funs (&optional (package :common-lisp-user) &aux out)
  (aif (find-package package)
    (do-all-symbols (s package)
      (if (and (fboundp s) (eql it (symbol-package s)))
        (push s out))))
  out)

; Start-up
; --------

; To run one of the examples, reset the **seed**,
; take a deepcopy of `my` (so `fun`
(defun run (eg my)
  (setf my       (deepcopy my)
        *seed*  (! my all seed))
  (if (! my all un)
    (funcall eg my)
    (multiple-value-bind (_ e)
      (ignore-errors (funcall eg my))
      (incf (! my all tries))
      (incf (! my all fails) (if e 1 0))
      (if e 
        (format t "~&~a [~a] ~a~%" (red "✖") eg (yellow e))
        (format t "~&~a [~a]~%" (green "✔") eg )))))

(defun main(my &key (package :common-lisp-user) (b4 "EG."))
   (let* ((all   (loop for fun in (funs package) 
                   if (b4-sym b4 fun) collect fun))
          (my    (cli my))
          (eg    (! my all eg)))
     (case eg
      (all       (loop for fun in all do (run fun my)))
      (ls        (loop for fun in all do 
                    (format t "  :eg ~15a : ~a~%" 
                      fun (or (documentation fun 'function) ""))))
      (otherwise (if (member eg all) (run eg my))))))

(main '(all (eg   "eg.cli"
             fails 0  tries 0
             seed 10013  
             data "../data/aa" 
             loud nil un nil)
        col (p 2)
        dom (samples 100 
             k        23)))

