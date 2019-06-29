;; vim: ts=2 sw=2 sts=2  et :
;-------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(got "oo/")

(fyi "A `col`umn is either a `num`ber or a `sym`bol.
`Col`s are places to store summaries about columns
of data in a `table`")

;-------- -------- -------- -------- -------- --------
(defun adds (c lst &key (filter #'identity))
  (dolist (x lst c) 
    (add c x :filter filter)))

(defun numbers (lst &key (filter #'identity)) 
  (adds (make-instance 'num) lst :filter filter))

(defun symbols (lst &key (filter #'identity)) 
  (adds (make-instance 'sym) lst :filter filter))

(defmethod xpect (x field n)
   (/ (? x field) n))

;-------- -------- -------- -------- -------- --------
(defthing col keeper (n 0))

(defmethod dist ((c col) x y)
  "Return a number 0 .. 1"
  (labels ((no (x) (eql x #\?)))
    (if (and (no x) (no y)) 
      1
      (progn 
        (if (no x) (setf y (norm    c y) 
                         x (faraway c y)))
        (if (no y) (setf x (norm    c x) 
                         y (faraway c x)))
        (delta c x y)))))

(defmethod add ((c col) x &key (filter #'identity))
  "Add numbers to column."
  (let ((y (funcall filter x)))
    (incf (? c 'n)) ; inc the total counts
    (add1 c y))   ; add it in
  x)

;-------- -------- -------- -------- -------- --------
(defthing num col
  (mu 0) (m2 0) (sd 0)
  (lo most-positive-fixnum)
  (hi most-negative-fixnum)
  )

(defmethod add1 ((nu num) x) 
  "New numbers update `min` and `max`."
  (with-slots (n all lo hi mu m2 sd) nu
    (let ((delta (- x mu)))
      (setf lo (min lo x)
            hi (max hi x)
            mu (+ mu (/ delta n))
            m2 (+ m2 (* delta (- x mu)))
            sd (if (< n 2)
                 sd
                 (sqrt (/ m2 (- n 1))))))))

(defmethod norm ((n num) x)  
  "Convert x to the range 0..1."
  (with-slots (lo hi) n
    (/ (- x lo)
       (+ (- hi lo)
          (/ 1 most-positive-fixnum)))))

(defmethod faraway ((n num) x) 
  (if (< x 0.5) 1 0))

(defmethod delta ((n num) x y)
  (pow (- x y) (the :col :p)))

;-------- -------- -------- -------- -------- --------
(defthing sym col
  (most 0) (mode) 
  (counts (make-hash-table)))

(defmethod add1 ((s sym) x)
  "Increment the symbols counts"
  (with-slots (most mode counts  n) s
    (let* 
      ((new (incf (gethash x counts 0))))
      (if (> new most)
        (setf most new
              mode x)))))

(defmethod norm ((s sym) x) 
  "Normalize symbols does nothing."
  x)

(defmethod faraway ((s sym) x) 
    (gensym))

(defmethod delta ((c col) x y)
  (if (eql x y) 0 1))

(defmethod ent ((s sym))
  (with-slots (counts n) s
    (let ((e 0))
      (do-hash (k v counts e)
        (let ((p (/ v n)))
          (decf e (* p (log p 2))))))))

