(defpackage :runr (:use :cl))
(in-package :runr)

(defun l() (load "runr"))

(defmacro ? (s x &rest xs) 
  "recursive slot-value access"
  (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))

(defun trim (x) 
 "kill leading,trailing whitespace"
  (string-trim '(#\Space #\Tab #\Newline) x))

(defun thing (x &aux (y (trim x)))
 "coerce `x` into a number or string or t or nil or #\?"
  (if (equal y "?") #\?
    (if (equal y "t") t
      (if (equal y "nil") nil
        (let ((z (read-from-string y nil nil)))
          (if (numberp z) z y))))))

(defun words (str &optional (char #\,) (filter #'thing) (here 0))
  "generate words, filtered, from str"
  (let ((there (position char str :start here)))
    (cons (funcall filter (subseq str here there))
          (if there (words str char filter (1+ there))))))

(defun with-lines (file fun)
  "Call `fun` for each line in `file`"
  (with-open-file (s file)
    (loop (funcall fun (or (read-line s nil) (return))))))

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

(defmethod add ((self num) x)
  )
