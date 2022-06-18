; vim:  ts=2 sw=2 et:
(defmacro ~  (x y) `(lambda ,x ,y))
(defmacro ~~ (x y) `(mapcar (lambda ,@x) ,y))
(defmacro is (spec &rest body) 
  "replace defun, let*, label, multiple-value-bind with one keyword 'is'"
  (labels 
    ((fun (x) (and (listp x) (> (length x) 2)))
     (mvb (x) (and (listp x) (listp (car x))))
     (%is (body xs)
          (if (null xs)
            `(progn  ,@body)
            (let ((x (pop xs)))
              (cond
                ((fun x) `(labels ((,(pop x) ,(pop x) ,@x))      ,(%is body xs)))
                ((mvb x) `(multiple-value-bind ,(pop x) ,(pop x) ,(%is body xs)))
                (t       `(let (,x)                         ,(%is body xs))))))))
    (if (listp spec) (%is body spec) `(defmethod ,spec ,@body))))

(defmacro aif (test yes &optional no) 
  "anaphoric 'if' (writes to 'it')"
  `(let ((it ,test)) (if it ,yes ,no)))

(defmacro whale (expr &body body) 
  "Anaphoric while (the current of the loop controller is cached in `a`)."
  `(do ((a ,expr ,expr)) ((not a)) ,@body))

(defmacro ? (s x &rest xs)
  "Recursive struct accessors; e.g. `(? s address street number)`."
  (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))

(defmacro want (x &rest y) "easy assert" `(assert ,x () ,@y))

(defvar *fails* 0)
(defmacro demo (msg &rest code)
  (let ((s '*error-output*))
  `(unless (progn (format ,s "~&") (princ ,msg ,s) ,@code)
     (format ,s "~&; FAIL: ~a ~%" ,msg)
     (incf *fails*))))

(IS rnd ((n number) &optional (places 0))
  "Return n with `places` decimals."
  (is ((div (expt 10 places))) (float (/ (round (* n div)) div))))

(defvar *seed* 10013)

(IS randi (&optional (n 1)) 
    "randint 0.. n-1." 
    (floor (* n (/ (randf 1000.0) 1000))))

(IS randf (&optional (n 1.0)) 
  "Return a random flaot 0..n-1."
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(IS args () 
  "different ways to access command line access"
  #+clisp *args* 
  #+sbcl *posix-argv*)  

(IS stop (&optional (n 1))
  "different ways to quit"
  #+sbcl (sb-ext:quit :code n)
  #+clisp (ext:exit n))

(defstruct (opt (:constructor %make-opt)) (help "") short long key value)
(defun opt! (key help &optional value)
  (cons key
        (%make-opt :help help :key key :value value
                   :short (format nil "-~(%s~)" (char (symbol-name key) 0))
                   :long  (format nil "--~(%s~)" key))))

(IS cli ((o opt))
 (with-slots (short long value) o
  (is ((cli1 (now) (cond ((equal value  t)   nil)
                               ((equal value  nil)  t)
                               (t   (str->thing now)))))
      (aif (member short (args)) (cli1  (second it)))
      (aif (member long  (args)) (cli1  (second it)))
      o)))

(defmethod print-object ((o opt) str)
  (with-slots (short long help value) o
    (format str "  ~s   ~s  ~s  = ~s~%"  short long  help value)))

; (defun cli (&key (alist  (deepcopy +config+)) 
;                  (help   "")
;                  (args   (cdr (deepcopy (args))))
;                  (now    (cdr (assooc plist :all)))
;   "Given a plist with keywords, if  the command line has any
;   of the same keywords, then update the plist with the new value."
;   (whale (pop args)
;       (setf a (read-from-string a))
;       (cond ((equalp a :H)  (format t "~a~%" help))
;             ((cdr (assoc  plist a) (setf now (getf plist a)))
;             ((getf now a)   (setf (getf now a) 
;                                   (read-from-string (car args))))
;             ((keywordp a)   (format t (red ";?? ignoring [~a]") a))))
;     plist))
;
; (let ((lst (args)))
;   (setf +config+
;         (cli2 (format nil "-%s" (char (symbol-name (car one)) 0))
;               (format nil "--%s" one)
;               (car one) (cdr val) (car args) (cdr args))))
;
; (defun cli2 (k1 k2 key val arg &rest args)
;   (if (member arg (list k1 k2))
;     (cons key (cond ((equal old t)   nil)
;                     ((equal old nil) t)
;                     (t   (str->thing (car args)))))
;     (and args (cli2 k1 k2 key val (car args) (cdr args))))))
;
(defun str->thing (x &aux (y (string-trim '(#\Space #\Tab #\Newline))))
    (cond ((string= y "?")     "?")
          ((string= y "true")  t)
          ((string= y "false") nil)
          (t (let ((z (ignore-errors (read-from-string y))))
               (if (numberp z) z y))))))

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

(defun tokens (str &optional (test (lambda (x) (member x (list #\Space #\Tab) :equal #'equal))) (start 0)) ;;; XXXXX here
  (let ((p1 (position-if test str :start start)))
    (if p1
      (let ((p2 (position-if #'(lambda (c) (not (funcall test c))) str :start p1)))
        (cons (subseq str p1 p2)
              (if p2
                (tokens str test p2)))))))

(defun file->words (f fn)
  "For each line  in file `f`, call a function `fn` on a list of words in each line."
  (with-open-file (s f) 
    (whale (read-line s nil)
      (aif (str->words a) (funcall fn  it)))))

(print (str->words "asdas,,asdasasd,asdas" #\comma))

      ; (print (mapcar (lambda (s) (str->words s #\Space))   (str->words "asdads
      ; as asdas  as  sasads das 
      ; asdas" (list #\Newline))))
