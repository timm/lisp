;;; attic: code hair no longer calls. Kept for reference; not loaded by
;;; hair.lisp or hair.asd. Move back if needed.
;;; (c) 2026, Tim Menzies <timm@ieee.org>, MIT license
(in-package :hair)

;;; from lithp.lisp
(defun o (&rest kvs) "A hash table from k1 v1 k2 v2 ..."
  (let ((h (make-hash-table :test #'equal)))
    (loop :for (k v) :on kvs :by #'cddr :do (setf (gethash k h) v))
    h))

;;; from ai.lisp
(defm clone (tbl &optional rows) (tbl (cons (? $cols names) rows)))

(defun rae (pairs &aux (mu (/ (loop :for (w) :in pairs :sum w)
                               (max 1 (length pairs)))))
  "Relative absolute error over (want . got): 100 = guessing the mean"
  (* 100 (/ (loop :for (w . g) :in pairs :sum (abs (- w g)))
            (+ +tiny+ (loop :for (w) :in pairs :sum (abs (- w mu)))))))

;;; from ai.lisp: forest before q=8 bins won. b4=nil cut on raw values
;;; (best-cut, all cols, no rf)
(defun forest (tb rows y &key (n 10) (cap 200) b4 (q 8) rf
                              (leaf 2) (maxd 30)
               &aux (rows (if cap (capped rows y cap) rows))
                    (cut (if b4 (binned tb rows :q q :rf rf :leaf leaf)
                             (lambda (rs) (best-cut (? tb cols x) rs y
                                                    #'make-sym
                                                    :leaf leaf)))))
  "n deep class trees (small leaves), each on a random half of rows
   (n=1: all of them), from <= cap rows per class. b4: cut on rows
   binned first (see binned)"
  (loop :repeat n
        :collect (tree (if (= n 1) rows
                           (subseq (shuffle rows) 0
                                   (floor (length rows) 2)))
                       y cut :make #'make-sym :maxd maxd)))

;;; from ai.lisp: exhaustive cuts (sort per col per node), before every
;;; tree went binned (see binned, bcut)
(defun xys (c rows y) "(x . y) for rows whose x in col c is known"
  (loop :for r :in rows :for x := (aref r (? c at))
        :unless (eql x '?) :collect (cons x (funcall y r))))

(defm cuts (sym rows y make fn &aux (xy (xys self rows y)))
  "Offer (fn self v yes no) per value v: v vs the rest"
  (dolist (k (remove-duplicates (mapcar #'car xy) :test #'equal))
    (let ((yes (funcall make)) (no (funcall make)))
      (loop :for (x . v) :in xy :do (add (if (equal x k) yes no) v))
      (funcall fn self k yes no))))

(defm cuts (num rows y make fn
            &aux (xy (sort (xys self rows y) #'< :key #'car)))
  "Offer (fn self x yes no) per split x <= v; sweep yes up, no down"
  (let ((yes (funcall make)) 
        (no  (adds (mapcar #'cdr xy) (funcall make))))
    (loop :for ((x . v) nxt) :on xy
          :do (add yes v) (add no v -1)
          :when (and nxt (/= x (car nxt))) 
          :do (funcall fn self x yes no))))

(defun best-cut (cols rows y make &key (leaf (? *k* leaf))
                                      &aux (best (argmost #'<)))
  "Lowest-cost (col v) over cols, both sides >= leaf rows; and its cost"
  (labels ((fun (c v yes no)            ; cuts passes its col as c
                (if (<= leaf (min (? yes n) (? no n)))
                  (funcall best (list c v) (xpect yes no)))))
    (dolist (c cols (funcall best))
      (cuts c rows y make #'fun))))

;;; from ai.lisp: class-count bcut, before bcut took any y summary
(defun coder (xs cs klass rows &aux (h (make-hash-table :test #'eq))
                                    (ks (mapcar #'car (? klass has))))
  "Hash: row -> vector, one bin (sym: value) index per col in xs, cut
   at cs (-1 = ?), then the class index. Built once; shared by all
   trees"
  (labels ((idx (c ch x)
             (cond ((eql x '?) -1)
                   ((sym-p c) (position x ch :test #'equal))
                   (t (bisect ch x)))))
    (dolist (r rows h)
      (setf (gethash r h)
            (coerce (append (loop :for c :in xs :for ch :in cs
                                  :collect (idx c ch (aref r (? c at))))
                            (list (position (aref r (? klass at)) ks
                                            :test #'equal)))
                    'vector)))))

(defun ent2 (yes all &aux (no (map 'vector #'- all yes))
                          (ny (reduce #'+ yes)) (nn (reduce #'+ no)))
  "Size-weighted entropy of class counts yes vs all-yes; and both sizes"
  (values (/ (+ (* ny (ent yes ny)) (* nn (ent no nn))) (max 1 (+ ny nn)))
          ny nn))

(defun bcut (xs cs vs nk &key m (leaf (? *k* leaf))
                         &aux (best (argmost #'<)) (at (length xs))
                              (is (loop :for i :below at :collect i)))
  "Lowest-cost (col v) over coded rows vs: per col, count classes per
   bin in one pass, then sweep the bins. Bins: x <= edge; syms: = v.
   m: try just m random cols (a random forest)"
  (dolist (i (if m (sort (subseq (shuffle is) 0 (min m at)) #'<) is)
             (funcall best))
    (let* ((c (nth i xs)) (ch (nth i cs))
           (n (make-array (list (1+ (length ch)) nk) :initial-element 0))
           (all (make-array nk :initial-element 0))
           (yes (make-array nk :initial-element 0)))
      (dolist (v vs)                     ; count classes per bin
        (let ((b (aref v i)) (k (aref v at)))
          (when (>= b 0) (incf (aref n b k)) (incf (aref all k)))))
      (dotimes (b (length ch))           ; bins: add up; syms: just this
        (dotimes (k nk)
          (setf (aref yes k) (+ (aref n b k)
                                (if (sym-p c) 0 (aref yes k)))))
        (multiple-value-bind (e ny nn) (ent2 yes all)
          (if (<= leaf (min ny nn))
            (funcall best (list c (aref ch b)) e)))))))
