;; vim: ts=2 sw=2 sts=2  et :
;-------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(defmacro do-hash ((k v h &optional out) &body body )
  "Set key 'k' and value 'v' to items in hash"
  `(progn (maphash #'(lambda (,k ,v) ,@body) ,h) ,out))

(defun hash-keys (h &aux out)
  "return keys in hash"
  (do-hash (k _ h out) (push k out)))
