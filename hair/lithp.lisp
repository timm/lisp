; vim: set ts=2 sw=2 sts=2 et lisp cc=75 lispwords+=loop,defm,aif :
;;; lithp: lisp, with a lisp. General tricks; nothing here knows AI.
;;; (c) 2026, Tim Menzies <timm@ieee.org>, MIT license
(defpackage :hair (:use :cl)
  (:export #:*k* #:about #:make-about
           #:cols #:tbl #:add #:adds #:mid #:div #:norm
           #:distx #:disty #:near #:same #:rank #:xval
           #:tree #:leaf #:csv #:srand #:rand #:normal))
(in-package :hair)

(defparameter +tiny+ 1e-32)

;;;; macros ------------------------------------------------------------
(defmacro ? (x k &rest ks) "Nested slot access: (? x a b) = x.a.b"
  (if ks `(? (slot-value ,x ',k) ,@ks) `(slot-value ,x ',k)))

(defmacro aif (test ok &optional no) "Anaphoric if: `it` = test"
  `(let ((it ,test)) (if it ,ok ,no)))

(defmacro has (x lst)
  "Place for x's count in alist lst (if missing, add x with 0)"
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x 0) ,lst))))))

(defun charn (s &optional (n -1))
  "Char n of string (or symbol) s; negative = from the end"
  (let ((s (string s))) (char s (mod n (length s)))))

(defmacro defm (name (cls &rest args) &body body)
  "defmethod with `self` bound to the first arg, specialized on class
   cls (t = anything): (defm mid (sym) ...), (defm add (t v) ...).
   In body, $x = (slot-value self 'x)"
  (labels ((swap (x)
             (if (and (symbolp x) (eql #\$ (charn x 0)))
               `(slot-value self ',(intern (subseq (string x) 1)
                                           (symbol-package x)))
               x))
           (visit (x) (if (consp x)
                          (cons (visit (car x)) (visit (cdr x)))
                          (swap x))))
    `(defmethod ,name ((self ,cls) ,@args) ,@(visit body))))

;;;; misc --------------------------------------------------------------
(defun memo (f &aux (h (make-hash-table :test #'eq)))
  "f, caching each result by its (eq) argument"
  (lambda (x) (multiple-value-bind (v ok) (gethash x h)
                (if ok v (setf (gethash x h) (funcall f x))))))

(defun bisect (v x &optional (lt #'<) (lo 0) (hi (length v)))
  "#items in sorted vector v that are (lt item x)"
  (if (>= lo hi) lo
    (let ((mid (floor (+ lo hi) 2)))
      (if (funcall lt (aref v mid) x)
        (bisect v x lt (1+ mid) hi)
        (bisect v x lt lo mid)))))

(defun argmost (&optional (better #'>) &aux out b4)
  "Visitor: (f x n) offers x scored n; (f) = first x with the most n,
   and its n as a 2nd value. (argmost #'<) = argmin"
  (lambda (&optional x n)
    (when (and x (or (null b4) (funcall better n b4)))
      (setf out x b4 n))
    (values out b4)))

(defun thing (s &aux (*read-eval* nil) (*package* (find-package :hair)))
  "String to number, ?, or string"
  (let ((v (ignore-errors (read-from-string s nil nil))))
    (if (or (numberp v) (eql v '?)) v (string-trim " " s))))

(defun cells (s &optional (lo 0)) "Split one csv line into things"
  (let ((hi (position #\, s :start lo)))
    (cons (thing (subseq s lo hi)) (if hi (cells s (1+ hi))))))

(defun csv (file) "Rows of a csv file, each a vector; skips blanks"
  (with-open-file (s (expand file))
    (loop :for line := (read-line s nil) :while line
      :for txt := (string-trim '(#\Return #\Space #\Tab) line)
      :unless (equal txt "") :collect (coerce (cells txt) 'vector))))

(defun expand (file) "A leading ~ means home"
  (if (eql #\~ (charn file 0))
    (merge-pathnames (subseq file 2) (user-homedir-pathname))
    file))

;;;; random ------------------------------------------------------------
(defvar *seed* 1234567891)

(defun srand (n) (setf *seed* n))

(defun rand (&optional (n 1)) "Park-Miller: 0 <= x < n"
  (setf *seed* (mod (* 16807 *seed*) 2147483647))
  (* n (/ *seed* 2147483647.0d0)))

(defun normal (&optional (mu 0) (sd 1)) "Box-Muller"
  (+ mu (* sd (sqrt (* -2 (log (- 1 (rand)))))
           (cos (* 2 pi (rand))))))

(defun shuffle (lst &aux (v (coerce lst 'vector))) "New, shuffled list"
  (loop :for i :from (1- (length v)) :downto 1
    :do (rotatef (aref v i) (aref v (floor (rand (1+ i))))))
  (coerce v 'list))

(defun pick (n &optional m &aux (is (loop :for i :below n :collect i)))
  "0..n-1; or just m of them, at random (kept in order)"
  (if m (sort (subseq (shuffle is) 0 (min m n)) #'<) is))
