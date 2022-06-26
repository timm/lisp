(defstruct row cells used)
(defstruct cols all x y name)
(defstruct rows kept cols)

; ~horthand macro for lambda.
(defmacro λ (args &rest rest) 
  `(lambda ,args ,@rest))

; Recursive struct accessors; e.g. `(? s address street number)`."
(defmacro ? (s x &rest xs)
  (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))

(defmacro aif (test yes &optional no) 
  "anaphoric 'if' (writes to 'it')"
  `(let ((it ,test)) (if it ,yes ,no)))


(defun charn ((x symbol)) (charn (symbol-name x)))
(defun charn ((x string)) (char x (1- (length x)))) 

(defun char0 ((x symbol)) (char0 (symbol-name x)))
(defun char0 ((x string)) (char x 0))

(defun goalp  (x) (member (charn x) '(#\! #\- #\+)))
(defun klassp (x) (eql   (charn x) #\!))
(defun nump   (x) (uppercase-p   (char0 x)))

; iterate `f` over all items in `file`
(defun reads (s file)
  (with-open-file (s file) (labels ((hop () (jump  (read s nil)))
                                    (jump (x) (when x (funcall f x) (hop))))
                             (hop))))

(defmethod add ((c cols) (r row))
  (dolist (slot '(x y) r)
    (dolist (col (slot-vaue c slot)) (add col (elt row (? col at))))))

(defmethod add ((i rows) (r cons))
  (add i (make row :cells r)))

(defmethod add ((i rows) (r row))
  (if (? i cols)
      (push (mapcar #'add (? i cols) r) (? i kept))
      (setf (? i cols) (make-cols row))))

  
