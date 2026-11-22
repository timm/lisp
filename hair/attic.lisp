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
