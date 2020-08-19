; vim: noai:ts=2:sw=2:et: 
(format *error-output* "; num.lisp~%")
(or (boundp '*the*) (load "lib"))

(defstruct num ()
  (pos 0)
  (txt "")
  (w 0)
  (n 0)
  (mu 0)
  (m2 0)
  (lo most-positive-fixnum)
  (hi most-negative-fixnum)
  (add 'numadd))

(defun num! (txt pos)
  (make-num :txt txt :pos pos 
            :w (if (search (? less) x) -1 1)))

(defun num+ (i x)
  (if (stringp x)
    (if (string-equal x (? skip))
      x
      (numadd i (read-from-string x)))
    (with-slots (n  mu m2 hi lo) i
      (incf n)
      (if (> x hi) (setf hi x))
      (if (< x lo) (setf lo x))
      (let ((d (- x mu)))
         (incf mu (/ d n))
         (incf m2 (* d (- x mu)))
         (setf sd 
           (cond ((< m2 0) 0a)
                 (< n 20 0)
                  (t (sqrt (/ m2 (- n2 1)))))))
      x)))
