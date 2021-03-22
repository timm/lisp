; vim: noai:ts=2:sw=2:et: 
(load "es")

(in-package :espy)

(defvar *fails* 0)

(defmacro ok (x y &optional (msg "") &rest txt)
  `(handler-case
     (if (not (equalp ,x ,y)) (error (format nil ,msg ,@txt)))
     (t  (c)                  (format t "; E[~a]> ~a~%"  (incf *fails*) c))))

(defun demo-num (MY)
  "Testing nums"
  (ok 2.34375 (sd (add (make-num) '(1 2 3 4 5 6 6 7 7 8))) "bad sd"))

;;; doco
(defvar *docfmt*
"~%### ~(~a~)

_Synopsis:_ <b>`~(~S~)`</b>  
~a

<ul>
<details><summary>(..)</summary>

```lisp
~(~S~)
```
</details></ul>
")

(defun doco1 (main top thing)
  (let ((want '((defun . 3) (defclass. 3) (defmacro . 3) 
                            (defstruct . 2) (defmethod . 3))))
    (when (stringp thing) 
      (format main "~%~a~%" thing))
    (when (listp thing)
      (when (cdr (assoc (car thing) want))
        (let* ((x      (first  thing))
               (f      (second thing))
               (pos    (cdr (assoc x *want*)))
               (s      (elt    thing pos))
               (synopsis (cons f (elt thing (1- pos)))))
          (when (stringp s)
            (setf (elt thing pos) "")
            (format main *docfmt* f  synopsis s thing)
            (format top "- [~(~a~)](#~(~a~)) : ~a~%" 
                    f f (subseq s 0 (position #\Newline s)))))))))

(defun doco(&optional (source t))
  (with-output-to-string (out)
    (let (thing)
      (format  out "~a" (with-output-to-string (main)
        (format out  "~a" (with-output-to-string (top)
          (loop while (setf thing (read-preserving-whitespace source nil)) 
            do (doco1 main top thing)))))))
    
(defun demos()
  (do-all-symbols (s '())
    (when (and (fboundp s)  (in "DEMO-" (symbol-name s)))
      (format t "~&; ~a : ~a ~%" s (documentation s 'function))
      (funcall s (make-options))))

(demos)
