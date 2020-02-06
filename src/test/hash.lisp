;; vim: ts=2 sw=2 sts=2  et :
;-------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(got "lib/hash.lisp")

(let ((h (make-hash-table)))
  (dolist (f *features*)
    (setf (gethash f h) (length (string f))))
  (dolist (k (hash-keys h))
  	(print `(k ,k v ,(gethash k h)))))
