(defpackage :runr (:use :cl))
(in-package :runr)

(defun l() (load "runr"))

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
  "Call `fun` for each line in `file`."
  (with-open-file (s file)
    (loop (funcall fun (or (read-line s nil) (return))))))
