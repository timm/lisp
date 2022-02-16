;(defpackage :sublime (:use :cl))
;(in-package :sublime)

(defvar *config* '(make-our 
  :help "sbcl --noinform --script expose.lisp [OPTIONS]
(c) 2022, Tim Menzies, MIT license

Lets have some fun."
  :options (list 
  (make-cli 'enough  "-e" "enough items for a sample" 512)
  (make-cli 'far     "-F" "far away                 " .9)
  (make-cli 'file    "-f" "read data from file      " "../data/auto93.csv")
  (make-cli 'help    "-h" "show help                " nil)
  (make-cli 'license "-l" "show license             " nil)
  (make-cli 'p       "-p" "euclidean coefficient    " 2)
  (make-cli 'seed    "-s" "random number seed       " 10019)
  (make-cli 'todo    "-t" "start up action          " ""))))

;;;; lib
;;; tricks
(defmacro ? (p x &rest xs) (if (null xs) `(getf ,p ,x) `(? (getf ,p ,x) ,@xs)))

(defmethod char1 ((x symbol) c) (char1 (symbol-name x) c))
(defmethod char1 ((x string) c) (equal (elt x 0) c))

;;; system
(defun args () sb-ext:*posix-argv*)
(defun stop (&optional (status 0)) (sb-ext:exit :code status))

;;; structs
(defstruct thing)
(defmethod print-object ((x thing) out)
  (labels ((pair (z) (let ((v (slot-value x z))) (if v `(,z ,v) z))))
    (print-object (cons (class-name (class-of x)) (mapcar #'pair (show x))) out)))

(defmacro defthing (x &rest slots &aux (id (gensym)))
  (labels ((hidep (z) (char1 z #\_))
           (name  (z) (if (consp z) (car z) z)))
    `(let ((,id 0))
       (defstruct
           (,x (:include thing)
               (:constructor ,(intern (format nil "%MAKE-~a" (symbol-name x)))))
         (_id (incf ,id)) ,@slots)
       (defmethod show ((x ,x)) ',(remove-if #'hidep (mapcar #'name slots))))))

(defthing num n mu m3 sd)
(defthing cols all x  klass y)
(defthing sample rows (cols (%make-cols)))

(defun make-num () (%make-num))

;;;; coerce
(defun item (x)
  "Return a number or a trimmed string."
  (cond ((numberp x) x)
        ((equal x "?") nil)
        (t (let ((y (ignore-errors (read-from-string x))))
             (if (numberp y) y x))))) 

(defun str2items (s &optional (c #\,) (n 0) &aux (pos (position c s :start n)))
  "Divide string `s` on character `c`."
  (if pos
      (cons (item (subseq s n pos)) (str2items s (1+ pos)))
      (list (item (subseq s n)))))

(defun file2items (file &optional (fn #'print))
  (with-open-file (s file)
    (loop (funcall fn (str2items (or (read-line s nil) (return-from file2items)))))))

;(defun file2sample (file &aux ((s (make-sample))))
;;;; lib
;;; lists
(defun nshuffle (lst)
  (let ((tmp (coerce lst 'vector)))
    (loop for i from (length tmp) downto 2
          do (rotatef (elt tmp (random i)) (elt tmp (1- i))))
    (coerce tmp 'list)))

(print (sample-_id (%make-sample)))
(print (sample-_id (%make-sample)))
(print (sample-_id (%make-sample)))
