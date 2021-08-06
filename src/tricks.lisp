; vim:  ts=2 sw=3 sts=2 et :

; Macros
; ------
; After calling `(reader #\c fun)`, then if
; ever we see c(sex) then (sexp) will be
; passes to `fun`.
(defmacro reader (com fun)
  `(set-macro-character ,com #'(lambda (stream char)
                                 (declare (ignore char))
                                 (,fun (read stream t nil t)))))

; Shorthand for recursive calls to slot-value.
(defmacro ? (s x &rest xs) 
  (if xs `(? (slot-value ,s ',x) ,@xs) `(slot-value ,s ',x)))

; Shorthand for recursive calls to getf
(defmacro ?? (s x &rest xs) 
  (if xs `(?? (getf ,s ',x) ,@xs) `(getf ,s ',x)))

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
(defun funs (package &aux out)
  (aif (find-package package)
    (do-all-symbols (s package)
      (if (and (fboundp s) (eql it (symbol-package s)))
        (push s out))))
  out)

; Find function names starting with `b4`?
(defun tests (&key (package :common-lisp-user) (b4 "EG-"))
  (loop for fun in (funs package) if (b4-sym b4 fun) collect fun))

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
; command-line values.If a flag is `-h` then print some help.
; Flags are `grouped` by flags starting with two
; hypherns. THE default group is `--all`.
; To change groups, just mention it before mentioning
; the flags in that group.
(defun cli (flags &key 
                 (help   "help")
                 (args   (cdr (deepcopy (argv))))
                 (group  (getf flags '--all)))
   (while (pop args)
     (setf now (read-from-string now))
     (cond ((equalp now '-h)   (format t "~a~%" help))
           ((getf flags now)   (setf group (getf flags now)))
           ((getf group now)   (assert (not (null args)) (args))
                               (setf (getf group now) 
                                     (read-from-string (pop args))))
           ((member now group) (setf (getf group now) t))
           (t                  (format t "~a ~a" (red "??") now))))

   flags)

; Just to give an example of its use, consider the command line        
;
; `lisp tricks.lisp -loud --dom -k 23 -samples 1`
;
; and this `eg-cli`  call to `cli`. 
; Since the default group is `--all``, the the
; intiial referece to `-loud` is really `--all -loud`.
; Note how once we change goupds (with `--dom` then
; we can set multiple flags in that group.
;
; This will set `-loud` to true and update the other flags: 
;
;     > (print (cli '(--all (-seed 10013
;                          -data "../data/aaa.csv"
;                          -loud nil)
;                   --col (-p 2)
;                   --dom (-samples 100 
;                         -k 23))))
;
;     (--ALL (-SEED 10013 
;             -DATA "../data/aa" 
;             -LOUD T) 
;      --COL (-P 2) 
;      --DOM (-SAMPLES 1 
;             -K 23))
;
(defun eg-cli()
  (pprint 
    (cli '(--all (-seed 10013  
                        -data "../data/aa" 
                        -loud nil )
                 --col (-p 2)
                 --dom (-samples 100 -k 23)))))

(eg-cli)
