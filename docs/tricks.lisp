; [aas](asda) [2121](asdaa)        
; [![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/Naereen/StrapDown.js/graphs/commit-activity)
; [![Ask Me Anything !](https://img.shields.io/badge/Ask%20me-anything-1abc9c.svg)](https://GitHub.com/Naereen/ama)
; [![GitHub license](https://img.shields.io/github/license/Naereen/StrapDown.js.svg)](https://github.com/Naereen/StrapDown.js/blob/master/LICENSE)
; [![GitHub release](https://img.shields.io/github/release/Naereen/StrapDown.js.svg)](https://GitHub.com/Naereen/StrapDown.js/releases/)
; [![Unlicense](https://img.shields.io/badge/License-Unlicense-blue.svg)](https://unlicense.org/)  
; 


;





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
; ----------------------------------
; ## Functions
; ### Symbols
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
(print  (tests :b4 "B4-"))
; --------------------------
; ### Sustms
; Exit LISP.
(defun halt (&optional (status 0)) 
  (sb-ext:exit :code status))
; Get command line
(defun argv () sb-ext:*posix-argv*)
; Given a plist with keywords, if  the command line 
; has any of the same keywords, then update the plist with the
; command-line values.keywords that 
(defun cli (&key plist
                 (help   "")
                 (args   (cdr (deepcopy (argv))))
                 (now    (getf plist :all)))
   (whale (pop args)
     (print a)
     (setf a (read-from-string a))
     (cond ((equalp a :H)  (format t "~a~%" help))
           ((getf plist a) (setf now (getf plist a)))
           ((getf now a)   (setf (getf now a) 
                                 (read-from-string (car args))))
            ((keywordp a)   (format t (red ";?? ignoring [~a]") a))))
   plist)
