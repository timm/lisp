; vim: noai:ts=2:sw=2:et: 
(format *error-output* "; num.lisp~%")
(or (boundp '*the*) (load "the"))
(or (fboundp 'send) (load "oo"))

(defun col! (&optional (txt "") (pos 0) &key all)
  (if (num? (elt txt 0))
    (num! txt pos)
    (sym! txt pos)))

(defun add* (x l)
  (dolist (one l x) (send add x one)))

(defstruct sym 
  (pos 0)
  (txt "")
  (n 0)
  (seen (make-hash-table :test 'sting-equal))
  (most 0)
  mode
  (add 'sym1))

(defun sym! (txt pos)
  (make-sym :txt txt :pos pos))

(defun sym1 (i x)
  (if (and (stringp x)
           (string-equal x (? char skip)))
    x
    (with-slots (n seen most mode) i
      (let* 
        ((new (incf (gethash x seen 0))))
        (if (> new most)
          (setf most new
                mode x))
        x))))
   
(defstruct num 
  (pos 0)
  (txt "")
  (w 0)
  (n 0)
  (mu 0)
  (m2 0)
  (sd 0)
  (lo most-positive-fixnum)
  (hi most-negative-fixnum)
  (add 'num1))

(defun col+ (i x)
  (cond ((skip? x) xP
      x
      (id 
(defun num! (txt pos)
  (make-num :txt txt :pos pos 
            :w (if (search (? char less) txt) -1 1)))

(defun num+ (i x)
  (if (stringp x)
    (if (string-equal x (? char skip))
      x
      (num+ i (read-from-string x)))
    (with-slots (n  mu m2 hi lo sd) i
      (incf n)
      (if (> x hi) (setf hi x))
      (if (< x lo) (setf lo x))
      (let ((d (- x mu)))
         (incf mu (/ d n))
         (incf m2 (* d (- x mu)))
         (setf sd 
           (cond ((< m2 0) 0)
                 ((< n 2) 0)
                 (t (sqrt (/ m2 (- n 1)))))))
      x)))
