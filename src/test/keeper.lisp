;; vim: ts=2 sw=2 sts=2  et :
;-------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(got "oo/")

(defthing fred keeper (a 1))

(defkept more ((x fred))
   (incf (? x 'a)))

(let ((f (make-instance 'fred)))
   (dotimes (_ 100) (more f))
   (print (? f 'a)))
