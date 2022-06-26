#| _               
  | |_  (_)  _ _    _  _ 
  |  _| | | | ' \  | || |
   \__| |_| |_||_|  \_, |
                     |__/  

(c) 2022, Tim Menzies, opensource.org/licenses/BSD-2-Clause.

   _   ._   _|_  o   _   ._    _ 
  (_)  |_)   |_  |  (_)  | |  _> 
       |                         

First we need some options. |#
(defvar *options 
  '((keep  256    "-K"  "items to keep            ")
    (k     1      "-k"  "nb low attributes classes")
    (m     2      "-n"  "nb low frequency classes ")
    (seed  10019  "-s"  "random number seed       ")))

#|._ _    _.   _  ._   _    _ 
  | | |  (_|  (_  |   (_)  _>  

Then we need some macros to handle some common short-cuts. |#

;; (?? x:atom):atom ; return an option
(defmacro ?? (x) `(second (assoc ',x *options)))

;; (? x:struct &rest slots:[atom]):atom ; nested slot access
(defmacro ? (s x &rest xs)
  (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))

;; (aif test yes no) ; anaphoric `if` (remembering test results in `it`)
(defmacro aif (test yes &optional no) 
  `(let ((it ,test)) (if it ,yes ,no)))

;   _  _|_  ._        _  _|_   _ 
;  _>   |_  |   |_|  (_   |_  _> 
;
; ROWs keeps 1 record in "cell" and  sets a "used" flag if we access the "y" vals.
(defstruct row cells used)
; ROWS holds many records in "rows"; summarized in "cols".
(defstruct rows rows cols)
; COLS summarize the goal and independent columns in "x" and "y".
(defstruct cols all x y names)

(defstruct col (n 0) (at 0) (txt "") (w 1) )
(defstruct (few (:include col)) kept ok (max (?? keep)))
(defstruct (num (:include col)) (kept (make-few)))
(defstruct (sym (:include col)) kept)
                  
(defun thing (x &aux (y (string-trim '(#\Space #\Tab #\Newline) x)))
  (cond ((string= y "?")     "?")
        ((string= y "true")  t)
        ((string= y "false") nil)
        (t (let ((z (ignore-errors (read-from-string y))))
             (if (numberp z) z y)))))

(defun args () #+clisp *args* #+sbcl *posix-argv*)

(defun cli (lst)
  (aif (member (third lst) (args) :test 'equal) 
    (setf (second lst)
          (cond ((equal (second lst) t)   nil)
                ((equal (second lst) nil) t)
                (t (thing (second it)))))))

(defvar *seed* (?? seed))
(defun randi (&optional (n 1)) (floor (* n (/ (randf 1000.0) 1000))))
(defun randf (&optional (n 1.0)) 
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(mapc 'cli *options)

(print *options)
; Recursive struct accessors; e.g. `(? s address street number)`."


; iterate `f` over all items in `file`
(defun reads (file f)
  (with-open-file (s file) 
    (labels ((there ()  (here (read s nil)))
             (here  (x) (when x (funcall f x) (there))))
      (there))))

; ## Cols                 
(defmethod complete ((c cols))
  (let ((at -1))
    (labels ((chars  (x) (if (stringp x) x (symbol-name x)))
             (charn  (x) (char x (1- (length x)))) 
             (char0  (x) (char x 0))
             (goalp  (x) (member (charn x) '(#\! #\- #\+)))
             (skipp  (x) (eql    (charn x) #\:))
             (klassp (x) (eql    (charn x) #\!))
             (what   (x) (if (uppercase-p (char0 x)) 'make-num 'make-sym))
             (make1  (txt &aux (col (funcall (what txt)) :at (incf at) :txt txt))
                       (if (eql #\- (charn txt)) (setf (? col w) -1))
                       (unless (skipp txt)
                         (if (klassp txt) (setf (? c klass) col))
                         (if (goalp txt)
                           (push col (? c y))
                           (push col (? c x))))
                       x))
      (setf (? c all) (mapcar 'make1 (mapcar 'chars (? c names))))
      c)))

(defmethod add ((c cols) (r row))
  (dolist (slot '(x y) r)
    (dolist (col (slot-value c slot)) (add col (elt (? row cells) (? col at))))))
                   
; ## rows
(defmethod add ((i rows) r)
  (if  (? i cols) 
    (if (consp r)
      (add i (make-row :cells r))
      (push (add (? i cols) r) (? i rows)))
   (seff (? i cols) (complete (make-cols :names r)))))

(defmethod add ((i rows) (r row))
  (if (? i cols)
      (push (mapcar #'add (? i cols) r) (? i kept))
      (setf (? i cols) (make-cols row))))
