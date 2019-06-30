;; vim: ts=2 sw=2 sts=2  et :
;--------- --------- --------- --------- --------- ---------
(unless (fboundp 'got) (load "../got"))

(got "sample/")

(defun weather-numerics ()
  (data
    :name 'weather-numerics
    :header  
          '(forecast temp ?humidty windy !play)
    :rows '((sunny 85 85 FALSE no)
            (sunny 80 90 TRUE no)
            (overcast 83 86 FALSE yes)
            (rainy 70 96 FALSE yes)
            (rainy 68 80 FALSE yes)
            (rainy 65 70 TRUE no)
            (overcast 64 65 TRUE yes)
            (sunny 72 95 FALSE no)    
            (sunny 69 70 FALSE yes)
            (rainy 75 80 FALSE yes)
            (sunny 75 70 TRUE yes)
            (overcast 72 90 TRUE yes)
            (overcast 81 75 FALSE yes)
            (rainy 71 91 TRUE no)
       )))
