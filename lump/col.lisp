; vim: noai:ts=2:sw=2:et: 
(format *error-output* "; num.lisp~%")
(or (boundp '*the*) (load "the"))
(or (fboundp 'send) (load "oo"))

(defun add* (l &optional (k 'num!))
  (let ((out (funcall k)))
    (print out)
    (dolist (x l out) 
      (send out add x))))

(defun sym! (&key (txt " ") (pos 0))
  (make-sym :txt txt :pos pos ))

(defun num! (&key (txt " ") (pos 0))
  (make-num :txt txt :pos pos 
            :w (if (eql  (? char less) (elt txt 0)) -1 1)))

(defthing  sym thing
  (pos 0)
  (txt " ")
  (n 0)
  (seen (make-hash-table :test #'equalp))
  (most 0)
  mode)

(defmethod add ((i sym) x)
  (if (skip? x)
    x
    (with-slots (n seen most mode) i
      (let* 
        ((new (incf (gethash x seen 0))))
        (if (> new most)
          (setf most new
                mode x))
        x))))
   
(defthing num thing
  (pos 0)
  (txt " ")
  (w 0)
  (n 0)
  (mu 0)
  (m2 0)
  (sd 0)
  (lo most-positive-fixnum)
  (hi most-negative-fixnum)_

(defmethod add ((i  num) (x string))
  (add i (read-from-string x)))

(defmethod add ((i  num) (x number))
  (if (skip?  x)
    x
    (with-slots (n mu m2 hi lo sd) i
      (let ((d (- x mu)))
        (incf n)
        (if (> x hi) (setf hi x))
        (if (< x lo) (setf lo x))
        (incf mu (/ d n))
        (incf m2 (* d (- x mu)))
        (setf sd 
              (cond ((< m2 0) 0)
                    ((< n 2) 0)
                    (t (sqrt (/ m2 (- n 1)))))))
      x)))
