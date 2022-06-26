;  ._    _.  ._ _    _    _ 
;  | |  (_|  | | |  (/_  _> 
;                           
(defvar *config* '(keep 256))
(defmacro ?? (x) `(getf *config* ',x))

(defstruct row cells used)
(defstruct cols all x y names)
(defstruct rows rows cols)

(defstruct col (n 0) (at 0) (txt "") (w 1) )
(defstruct (few (:include col)) kept ok (max (?? keep)))
(defstruct (num (:include col)) (kept (make-few)))
(defstruct (sym (:include col)) kept)
                  
#| shorthand macro for lambda.
|#
(defmacro λ (args &rest rest) `(lambda ,args ,@rest))

; Recursive struct accessors; e.g. `(? s address street number)`."
(defmacro ? (s x &rest xs)
  (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))

; anaphoric 'if' (writes to 'it')"
(defmacro aif (test yes &optional no) `(let ((it ,test)) (if it ,yes ,no)))

; iterate `f` over all items in `file`
(defun reads (file f)
  (with-open-file (s file) (labels ((hop  ()  (jump (read s nil)))
                                    (jump (x) (when x (funcall f x) (hop))))
                             (hop))))

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
             (make1  (txt &aux (col (funcall (whatp txt)) :at (incf at) :txt txt))
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

  
