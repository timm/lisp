;; vim: ts=2 sw=2 sts=2  et :
;-------- -------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(defmacro ? (obj first-slot &rest more-slots)
	"From https://goo.gl/dqnmvH:"
	(if (null more-slots)
		`(slot-value ,obj ,first-slot)
		`(? (slot-value ,obj ,first-slot) ,@more-slots)))


