; vim: noai:ts=2:sw=2:et: 
(load "got")
(got "oo")

(defthing col thing (w 1) (n 0) (pos 0) (txt ""))

(defmethod initialize-instance :around 
  ((i col) &key (pos 0) (txt ""))
  (call-next-method  
    i :pos pos :txt txt
    :w (if (and (> (length txt) 0)
                (eql (my ch less ) (elt txt 0))) 
         -1 
         1)))

(defun add* (l &optional (k 'num))
  (let ((out (make-instance k)))
    (dolist (x l out) (add out x))))

(defmethod add ((i col) x)
  (cond ((skip? x) x)
        (t         (incf (? i n))
                   (add1 i x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defthing sym col (n 0) (most 0) (mode)
  (seen (make-hash-table :test #'equalp)))

(defmethod add1 ((i sym) x) 
  (with-slots (n seen most mode) i
    (let* ((new (incf (gethash x seen 0))))
      (if (> new most)
        (setf most new
              mode x))))
  x)

(defmethod ent ((i sym) &aux (out 0))
  (with-slots (seen n) i
    (dohash (_ v seen out)
      (let ((p (/ v n)))
        (if (> p 0)
          (decf out (* p (log p 2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defthing num col (n 0) (mu 0) (m2 0) (sd 0)
  (lo most-positive-fixnum)
  (hi most-negative-fixnum))

(defmethod add1 ((i num) (x string)) 
  (add1 i (read-from-string x)))

(defmethod add1 ((i num) (x number))
  (with-slots (n mu m2 hi lo sd) i
    (let ((d (- x mu)))
      (if (> x hi) (setf hi x))
      (if (< x lo) (setf lo x))
      (incf mu (float (/ d n)))
      (incf m2 (float (* d (- x mu))))
      (setf sd 
            (cond ((< m2 0) 0)
                  ((< n 2) 0)
                  (t (sqrt (/ m2 (- n 1))))))))
  x)


