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
