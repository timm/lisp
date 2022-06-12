---
title: "sublime: "
---

  )\.--.       .-.     /(,-.   .')      .'(   )\   )\   )\.---.  
 (   ._.'  ,'  /  )  ,' _   ) ( /       \  ) (  ',/ /  (   ,-._( 
  `-.`.   (  ) | (  (  '-' (   ))       ) (   )    (    \  '-,   
 ,_ (  \   ) '._\ )  )  _   )  )'._.-.  \  ) (  \(\ \    ) ,-`   
(  '.)  ) (  ,   (  (  '-' /  (       )  ) \  `.) /  )  (  ``-.  
 '._,_.'   )/ ._.'   )/._.'    )/,__.'    )/      '.(    )..-.(  

```lisp
```

(quote
   (an (elegant (weapon 
       (for (a (more 
           (civilized age))))))))

```lisp
```

                    __   _        
 __   ___   _ _    / _| (_)  __ _ 
/ _| / _ \ | ' \  |  _| | | / _` |
\__| \___/ |_||_| |_|   |_| \__, |
                            |___/ 

```lisp

(defstruct cli key flag help value)
(defstruct options
  (help
    "sbcl --noinform --script expose.lisp [OPTIONS]
(c) 2022, Tim Menzies, MIT license

Lets have some fun.")
  (options
   (list
    (cli! 'cautious "-c" "about on any error"        t)
    (cli! 'enough   "-e" "enough items for a sample" 512)
    (cli! 'far      "-F" "far away                 " .9)
    (cli! 'file     "-f" "read data from file      " "../data/auto93.csv")
    (cli! 'help     "-h" "show help                " nil)
    (cli! 'license  "-l" "show license             " nil)
    (cli! 'p        "-p" "euclidean coefficient    " 2)
    (cli! 'seed     "-s" "random number seed       " 10019)
    (cli! 'todo     "-t" "start up action          " ""))))

(defmethod print-object ((c cli) s)
  (with-slots (key flag help value) c 
    (format s "   ~5a  ~a " flag  help)
    (if (member value '(t nil)) (terpri s) (format s "= ~a~%" value))))

(defmethod print-object ((o options) s)
  (with-slots (help options) o
    (format s "~a~%~%OPTIONS:~%" help)
    (dolist (x options) (print-object (cdr x) s))))

(defmethod item ((x number)) x)
(defmethod item ((x string))
  "Return a number or a trimmed string."
  (setf x (string-trim '(#\Space #\Tab) x))
  (unless (equal x "?") 
    (let ((y (ignore-errors (read-from-string x))))
      (if (numberp y) y x))))

(defun cli! (key flag help value)
  (let* ((args (cdr sb-ext:*posix-argv*))
         (it   (member flag args :test #'equal)))
    (if it (setf value (cond ((equal it t)   nil)
                             ((equal it nil) t)
                             (t (item (second it))))))
    (cons key (make-cli :key key :flag flag :help help :value value))))

(defvar *the* (make-options))
```

 _   _   _    
| | (_) | |__ 
| | | | | '_ \
|_| |_| |_.__/

```lisp

;; macros
(defmacro $   (x)  `(cli-value (cdr (assoc ',x (options-options *the*)))))
(defmacro aif (? y &optional n) `(let ((it ,?)) (if it ,y ,n)))
(defmacro ?   (p x &rest xs) (if (null xs) `(getf ,p ',x) `(? (getf ,p ',x),@xs)))

;; random
(defvar *seed* 10013)
(defun randi (&optional (n 1)) (floor (* n (/ (randf 1000.0) 1000))))
(defun randf (&optional (n 1.0)) 
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

;; lists
(defun nshuffle (lst)
  "Return a new list that randomizes over of lst"
  (let ((tmp (coerce lst 'vector)))
    (loop for i from (length tmp) downto 2
          do (rotatef (elt tmp (random i)) (elt tmp (1- i))))
    (coerce tmp 'list)))

(defun per (lst &optional (p .5)) (elt lst (floor (* p (length lst)))))

;; defthings
(defmacro defthing (x &rest slots &aux (id (gensym)))
  "Defines structs with uniq ids `_id` and a constuctor `(%make-x)`
   and a print method that hides privates slots (those starting with `_`)."
  (labels ((hidep (z) (equal (char (symbol-name z) 0) #\_))
           (name  (z) (if (consp z) (car z) z))
           (names ()  (remove-if #'hidep (mapcar #'name slots)))
           (%make ()  (intern (format nil "%MAKE-~a" (symbol-name x)))))
    `(let ((,id 0))
       (defstruct (,x (:constructor ,(%make))) (_id (incf ,id)) ,@slots)
       (defmethod print-object ((it ,x) s) (show-object it ',x ',(names) s)))))

(defun show-object (it klass slots s)
  (labels ((show (z)  (let* ((k (intern (symbol-name z) "KEYWORD"))
                             (v (slot-value it z)))
                        (if v `(,k ,v) k))))
    (print-object (cons klass (mapcar #'show slots)) s)))

;; files
(defmacro with-csv ((lst file &optional out) &body body)
  `(progn (%with-csv ,file (lambda (,lst) ,@body)) ,out))

(defun %csv (file &optional (fn 'print))
  "Run a function `fn` over file (sub-function of `with-csv`)."
  (with-open-file (str file)
