; vim: ts=2 sw=2 sts=2  et
(unless (fboundp 'got) (load "../got"))

(got "rows/")

(format t "~&~a~%" (make-data))
