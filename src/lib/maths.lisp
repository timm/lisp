;; vim: ts=2 sw=2 sts=2  et :
;-------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(defun ish (a b &optional (epsilon 0.01))
  (< (/ (abs (- a b)) a) epsilon))
