; vim:  ts=2 sw=2 et:

(defmacro aif (test yes &optional no)
  "Anaphoric if (the result of the condition ;is cached in `it`)."
  `(let ((it ,test)) (if it ,yes ,no)))

(defmacro whale (expr &body body) 
  "Anaphoric while (the current of the loop controller is cached in `a`)."
  `(do ((a ,expr ,expr)) ((not a)) ,@body))

(defmacro ? (p x &rest xs)
  "Recursive  plist accessor; e.g. `(? p :outer :inner)`."
  (if (null xs) `(getf ,p ,x) `(? (getf ,p ,x) ,@xs)))

(defmacro o (s x &rest xs)
  "Recurse struct accessor; e.g. `(o s address street number)`."
  (if (null xs) `(slot-value ,s ',x) `(o (slot-value ,s ',x) ,@xs)))

(defmacro want (x &rest y)
  "Simpler assert statement."
  `(assert ,x () ,@y))

(defun rnd (number &optional (places 0))
  "Return  number with `places` number of decimals."
  (let ((div (expt 10 places)))
    (float (/ (round (* number div)) div))))

(defvar *seed* 10013)

(defun randi (&optional (n 1)) 
  "Return a random integer 0.. n-1."
  (floor (* n (/ (randf 1000.0) 1000))))

(defun randf (&optional (n 1.0)) 
  "Return a random flaot 0..n-1."
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun args () 
  "different ways to access command line access"
  #+clisp *args* 
  #+sbcl *posix-argv*)  

(defun stop (&optional (n 1))
  "different ways to quit"
  #+sbcl (sb-ext:quit :code n)
  #+clisp (ext:exit n))

(defun cli (&key (plist  (deepcopy +config+)) 
                 (help   "")
                 (args   (cdr (deepcopy (args))))
                 (now    (getf plist :all)))
  "Given a plist with keywords, if  the command line has any
  of the same keywords, then update the plist with the new value."
  (whale (pop args)
    (when (consp a)
      (setf a (read-from-string a))
      (cond ((equalp a :H)  (format t "~a~%" help))
            ((getf plist a) (setf now (getf plist a)))
            ((getf now a)   (setf (getf now a) 
                                  (read-from-string (car args))))
            ((keywordp a)   (format t (red ";?? ignoring [~a]") a))))
    plist))

(defun cell? (x &optional looping)
  "Return a number (if we can)."
  (cond ((numberp x) x)
        ((stringp x) (if (equal "?" x)
                       #\?
                       (let ((y (read-from-string x)))
                         (if (numberp y) y x))))
        (t x)))

(defmacro inca (x a &optional (n  1))
  "A counter, implemented as an association list."
  `(incf (cdr (or (assoc ,x ,a :test #'equal)
                  (car (setf ,a (cons (cons ,x 0) ,a)))))
         ,n))

(defmacro geta (x a &optional (test #'equal)) 
  "assoc access" 
  `(cdr (assoc ,x ,a :test ,test)))

(defun powerset (lst)
  "Return all subsets of a list."
  (let ((out (list nil)))
    (dolist (x lst out)
      (dolist (tmp out)
        (push (cons x tmp) out)))))

(defun color (s c &optional (str t))
  "Return string `s`, surrounded by ANSI escape color sequences."
  (let ((all '((black . 30) (red . 31) (green . 32)  (yellow . 33) 
                            (blue . 34)  (magenta . 35) (cyan . 36) (white .37))))
    (format str "~c[~a;1m~a~c[0m" #\ESC (cdr (assoc c all)) s #\ESC)))

(defun red    (s) "red"    (color s 'red nil))
(defun green  (s) "green"  (color s 'green nil))
(defun yellow (s) "yellow" (color s 'yellow nil))

(defun str->words (s0 &optional (sep #\comma)) 
  "Kill white space, split string on `sep` (defaults to ',')."
  (labels ((whitep (c) (member c '(#\space #\tab)))
           (worker (str &optional (lo 0))
                   (aif (position sep str :start lo)
                     (cons (subseq str lo  it) (worker str (1+ it)))
                     (list (subseq str lo)))))
    (let ((s1 (remove-if  #'whitep s0)))
      (unless (zerop (length s1)) (worker  s1)))))

(defun file->words (f fn)
  "For each line  in file `f`, call a function `fn` on a list of words in each line."
  (with-open-file (s f) 
    (whale (read-line s nil)
      (aif (str->words a) (funcall fn  it)))))
