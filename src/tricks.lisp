; vim:  ts=2 sw=3 sts=2 et :
  
; ## Macros

; After calling `(reader #\c fun)`, then if
; ever we see c(sex) then (sexp) will be
; passes to `fun`.
(defmacro reader (com fun &aux (fun1 (gensym)))
  `(progn (defun ,fun1 (stream char)
            (declare (ignore char))
            (,fun (read stream t nil t)))
          (set-macro-character ,com #',fun1)))

; Shorthand for recursive calls to slot-value.
(defmacro ? (s x &rest xs) 
  (if xs `(? (slot-value ,s ',x) ,@xs) `(slot-value ,s ',x)))

; Anaphoric if.
(defmacro aif (test yes &optional no) 
  `(let ((it ,test)) (if it ,yes ,no)))

; Anaphoric while
(defmacro whale (expr &body body) 
  `(do ((il ,expr ,expr)) ((not il)) ,@body))


; Colors
(defun color (s c &optional (str t))
  (let ((all '((black . 30) (red . 31) (green . 32)  (yellow . 33) 
               (blue . 34)  (magenta . 35) (cyan . 36) (white .37))))
    (format str "~c[~a;1m~a~c[0m" #\ESC (cdr (assoc c all)) s #\ESC)))

; red
(defun red (s) (color s 'red nil))
; green
(defun green (s) (color s 'green nil))
; ### yellow
(defun yellow (s) (color s 'yellow nil))


; Does a symbol name start with `b4`?

(defun b4-sym (b4 sym &aux (n (length b4)) (s (symbol-name sym)))
  (and (>= (length s) n) 
       (equalp b4 (subseq s 0 n))))
 
; ### Lists
(defun deepcopy (x)
   (if (atom x) x (mapcar #'deepcopy x)))

; ### Meta
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

; Exit LISP.
(defun halt (&optional (status 0)) 
  (sb-ext:exit :code status))

; Get command line
(defun argv () sb-ext:*posix-argv*)

; Given a plist with keywords, if  the command line 
; has any of the same keywords, then update the plist with the
; command-line values.keywords that 
(defun cli (&key (plist nil)
                 (help   "")
                 (args   (cdr (deepcopy (argv))))
                 (now    (getf plist '-all)))
   (whale (pop args)
     (setf il (read-from-string il))
     (cond ((equalp il '-h) (format t "~a~%" help))
           ((getf plist il) (setf now (getf plist il)))
           ((getf now il)   (setf (getf now il) (read-from-string (car args))))
           ((member il now) (setf (getf now il) t))
           (t               (format t "~a ~a" (red "??") il))))

   plist)

(print 
  (cli :plist '(-all (-seed 10013
                            -data "../data/aaa.csv"
                            -loud nil)
                -col (-p 2)
                -dom (-samples 100 -kkk 23))))

