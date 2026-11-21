; vim: set ts=2 sw=2 sts=2 et lisp cc=75 lispwords+=loop,defm,aif :
;;; hr: hair minus ai. General lisp tricks; nothing here knows AI.
;;; (c) 2026, Tim Menzies <timm@ieee.org>, MIT license
(in-package :hair)

(defparameter +tiny+ 1e-32)

;;;; macros ------------------------------------------------------------
(defmacro ? (x k &rest ks) "Nested slot access: (? x a b) = x.a.b"
  (if ks `(? (slot-value ,x ',k) ,@ks) `(slot-value ,x ',k)))

(defmacro aif (test ok &optional no) "Anaphoric if: `it` = test"
  `(let ((it ,test)) (if it ,ok ,no)))

(set-macro-character #\$       ; $x = (slot-value self 'x)
  (lambda (s c) (declare (ignore c))
    `(slot-value self ',(read s t nil t))) t)

(defmacro defm (name (cls &rest args) &body body)
  "defmethod with `self` bound to the first arg, specialized on class
   cls (t = anything): (defm mid (sym) ...), (defm add (t v) ...)"
  `(defmethod ,name ((self ,cls) ,@args) ,@body))

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
  "Visitor: (f x n) offers x scored n; (f) = first x with the most n.
   (argmost #'<) = argmin"
  (lambda (&optional x n)
    (when (and x (or (null b4) (funcall better n b4)))
      (setf out x b4 n))
    out))

(defun o (&rest kvs) "A hash table from k1 v1 k2 v2 ..."
  (let ((h (make-hash-table :test #'equal)))
    (loop :for (k v) :on kvs :by #'cddr :do (setf (gethash k h) v))
    h))

(defun charn (s &optional (n -1)) "Char n of s; negative = from the end"
  (char s (mod n (length s))))

(defun thing (s &aux (*read-eval* nil)) "String to number, ?, or string"
  (let ((v (ignore-errors (read-from-string s nil nil))))
    (if (or (numberp v) (eql v '?)) v (string-trim " " s))))

(defun cells (s &optional (lo 0)) "Split one csv line into things"
  (let ((hi (position #\, s :start lo)))
    (cons (thing (subseq s lo hi)) (if hi (cells s (1+ hi))))))

(defun csv (file) "Rows of a csv file, each a vector"
  (with-open-file (s (expand file))
    (loop :for line := (read-line s nil) :while line
      :collect (coerce (cells line) 'vector))))

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
