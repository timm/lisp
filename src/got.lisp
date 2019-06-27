;; vim: ts=2 sw=2 sts=2  et :
;-------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(let 
  ((pats '("../*.lisp" "../*/" "../*/*.lisp"))
   files
   gotten)
  (labels 
    ((knowns (x) 
        (remove-if-not #'(lambda (z) (end z x)) files))
     (end (s1 s2)
        (let ((p (mismatch s2 s1 :from-end T)))
          (or (not p) (= 0 p))))
     (got1 (f)
        (format *error-output* "~&; loading ~a~%" f)
        (if (end f "/") 
          (dolist (f (directory (format nil "~a/*.lisp" f)))
            (got (namestring f)))
          (progn 
            #-sbcl (load f) 
            #+sbcl (handler-bind
                     ((style-warning #'muffle-warning))
                     (load f))))))

    (dolist (pat pats)
      (dolist (file (directory pat))
        (push (namestring file) files)))

    (defun got (&rest lst)
      (dolist (f lst)
        (let ((where (knowns f)))
          (if (not where) ; too few
            (error "unknown file name [~a]" f)
            (if (cdr where) ; too many
              (error "ambiguous file name [~a]~%" f)
              (unless (member (car where) gotten)
                (push (car where) gotten)
                (got1 (car where))))))))))

(defmacro getfs (plist first-slot &rest more-slots)
	"Recursive access to plists; 
  e.g. `(getfs plist 'a 'b c)` expands to 
  `(getf (getf (getf plist 'a) 'b) c)`.
  Built using advice from https://goo.gl/dqnmvH."
	(if (null more-slots)
		`(getf ,plist ,first-slot)
		`(getfs (getf ,plist ,first-slot) ,@more-slots)))

(defmacro my (x &rest y)
  "Access global config options."
  `(getfs +config+ ,x ,@y))

(defun config0()
  '(:rand  (:seed 10013)
    :col   (:p 2)))

(defparameter +config+ (config0))
