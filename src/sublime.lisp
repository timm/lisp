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
;;; system
(defun klass-slots (it)
 "what are the slots of a class?"
 #+clisp (class-slots (class-of it))
 #+sbcl  (sb-mop:class-slots (class-of it)))

(defun slot-name (x)
  "what is a slot's name?"
  #+clisp (slot-definition-name x)
  #+sbcl (sb-mop:slot-definition-name x))

(defun args ()
 "what are the command line args?"
  #+clisp ext:*args*
  #+sbcl sb-ext:*posix-argv*)

(defun stop (&optional (status 0))
  "how to halt?"
  #+sbcl (sb-ext:exit :code status)
  #+:clisp (ext:exit status))

;;; structs
(defmethod print-object ((it thing) out)
  "print string for all public slot names"
  (labels ((hide  (x) (char1 x #\_))
           (show  ()  (remove-if #'hide (mapcar #'slot-name (klass-slots it))))
           (pair  (s) (,s ,(slot-value it s)))
           (pairs ()  (mapcar  #'pair (sort (show) #'string<))))
    (format out "~a" (cons (class-name (class-of it)) (pairs)))))

;;; strings
(defmethod char1 ((x symbol) c) (char1 (symbol-name x) c))
(defmethod char1 ((x string) c) (equal (elt (symbol-name x) 0) c)


(defstruct (num    (:constructor %num))  n mu m3 sd)
(defstruct (cols   (:constructor %col)) all x y klass)
(defstruct (sample (:constructor %sample)) rows (cols (make-cols)))

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
    (loop (funcall fn (items (or (read-line s nil) (return-from csv)))))))

(defun file2sample (file &aux ((s (make-sample))))
;;;; lib
;;; lists
(defun nshuffle (lst)
  (let ((tmp (coerce lst 'vector)))
    (loop for i from (length tmp) downto 2
          do (rotatef (elt tmp (random i)) (elt tmp (1- i))))
    (coerce tmp 'list)))
