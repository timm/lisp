(defpackage :runr (:use :cl))
(in-package :runr)

(defvar *options* '("
runr: simple lisp
(c)2032 TIm Menzies <timm@ieee.org> BSD-2

USAGE: lisp runr.lisp [OPTIONS])"

  help ("-h" "show help          " t)
  seed ("-s" "random number seed " 10019)))

(defun args () #+clisp ext:*args*
               #+sbcl  sb-ext:*posix-argv*)

(loop :for (key val) :on *settings* :by #'cddr :do (print `(,key ,val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  _   _   _    
; | | (_) | |__ 
; | | | | | '_ \
; |_| |_| |_.__/
(defun l() (load "runr"))

(defmacro ? (s x &rest xs) 
  "recursive slot-value access"
  (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))

(defun trim (x) 
 "kill leading,trailing whitespace"
  (string-trim '(#\Space #\Tab #\Newline) x))

(defun thing (x &aux (y (trim x)))
 "coerce `x` into a number or string or t or nil or #\?"
  (or (cdr (assoc y '(("?" . #\?) ("t" .t) ("nil" . nil)) :test #'equal))
      (let ((z (read-from-string y nil nil))) 
        (if (numberp z) z y))))

(defun words (str &optional (char #\,) (filter #'thing) (here 0))
  "generate words, filtered, from str"
  (let ((there (position char str :start here)))
    (cons (funcall filter (subseq str here there))
          (if there (words str char filter (1+ there))))))

(defun with-lines (file fun)
  "Call `fun` for each line in `file`"
  (with-open-file (s file) 
    (loop (funcall fun (or (read-line s nil) (return))))))

(defmacro defstruct+ (x doco &body body)
  "Creates %x for constructor, enables pretty print, hides slots with '_' prefix."
  (let* ((slots (mapcar    (lambda (x) (if (consp x) (car x) x))          body))
         (show  (remove-if (lambda (x) (eq #\_ (char (symbol-name x) 0))) slots)))
    `(progn (defstruct (,x (:constructor ,(intern (format nil "%MAKE-~a" x)))) ,@body)
            (defmethod print-object ((self ,x) str)
              (labels ((fun (y) (format nil ":~(~a~) ~a" y (slot-value self y))))
                (format str "~a" (cons ',x (mapcar #'fun ',show))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ako ((s string) kind)
  "given a column header, comment on its the propertoes of that column"
  (if (upper-case-p (char s 0)) 
    'num
    (first (member (char s (1- (length s))) 
                   (cdr (assoc kind '((ignore #\X) (klass #\!) (less #\-)
                                                   (more #\+) (goal #\+ #\- #\!))))))))

(defstruct num (at 0) (txt "") (n 0) (mu 0) (m2 0) (lo 1E31) (hi -1E21))

(defun num! (&optional (at 0) (txt ""))
  (make-num :at at :txt txt :w (if (ako txt 'less) -1 1)))

(defmethod add ((self num) x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *egs* nil)

(defmacro eg (what arg doc &rest src) 
  "define a example"
  `(push (list ',what ',doc (lambda ,arg ,@src)) *egs*))

(defun demo (what doc fun)
  (unless (eq t (funcall fun ))
    (incf fails)
    (format t "~&FAIL [~(~a~)] ~a ~%" what doc)
    1)
  0) 

(defun demos (settings all &optional one)
  "Run `one` (or `all`) the demos, resttomg glonal s seatch time"
  (let ((fails 0)
        (resets (copy-list settings)))
    (dolist (trio all) 
       (loop for (key . value) in resets do (setf (cdr (assoc key settings)) value))
       (setf *seed* (or (cdr (assoc 'seed settings)) 10019))
       (incf fails (demo (first trio) (second trio) (third trio))))
    #+clisp (ext:exit fails)
    #+sbcl  (sb-ext:exit :code fails)))

 


