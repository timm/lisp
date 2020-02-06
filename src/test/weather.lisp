;; vim: ts=2 sw=2 sts=2  et :
;-------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(got "data/weather.lisp" "data/weathernumerics.lisp" "data/auto93.lisp")

;(print (weather))
;(print (weather-numerics))
(auto93)
