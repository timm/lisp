; vim: noai:ts=2:sw=2:et: 
(format *error-output* "; num.lisp~%")
(or (boundp '*the*) (load "the"))
(or (fboundp 'send) (load "oo"))

(defun add* (l &optional (k 'num))
  (let ((out (make-instance k)))
    (dolist (x l out) (add out x))))

(defun num! (&key (txt " ") (pos 0))
  (make-num :txt txt :pos pos 
            :w (if (eql  (? char less) (elt txt 0)) -1 1)))

(defthing col thing (w 1) (pos 0) (txt ""))

(defmethod initialize-instance :around 
  ((c col) &key (pos 0) (txt ""))
  (call-next-method  
   c :pos pos :txt ""
   :w (if (and (> (length txt) 0)
               (eql (? ch less ) (elt txt 0))) 
        -1 1)))

(defmethod add ((i col) x)
  (unless (skip? x) (add1 i x))
  x)

(defthing sym col (n 0) (most 0) (mode)
  (seen (make-hash-table :test #'equalp)))

(defmethod add1 ((i sym) x) 
  (with-slots (n seen most mode) i
    (let* ((new (incf (gethash x seen 0))))
      (if (> new most)
        (setf most new
              mode x)))))

(defthing num col (n 0) (mu 0) (m2 0) (sd 0)
  (lo most-positive-fixnum)
  (hi most-negative-fixnum))

(defmethod add1 ((i num) (x string)) 
  (add1 i (read-from-string x)))

(defmethod add1 ((i num) (x number))
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
                  (t (sqrt (/ m2 (- n 1)))))))))
