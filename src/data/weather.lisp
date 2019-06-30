;; vim: ts=2 sw=2 sts=2  et :
;--------- --------- --------- --------- --------- ---------
(unless (fboundp 'got) (load "../got"))

(got "sample/")

(defun weather ()
  (data
   :name     'weather
   :header  '(forecast temp ?humidty windy !play)
   :rows     '((sunny    hot  high   FALSE no) 
              (sunny    hot  high   TRUE  no)
              (overcast hot  high   FALSE yes)
              (rainy    mild high   FALSE yes)
              (rainy    cool normal FALSE yes)
              (rainy    cool normal TRUE  no)
              (overcast cool normal TRUE  yes)
              (sunny    mild high   FALSE no)
              (sunny    cool normal FALSE yes)
              (rainy    mild normal FALSE yes)
              (sunny    mild normal TRUE  yes)
              (overcast mild high   TRUE  yes)
              (overcast hot  normal FALSE yes)
              (rainy    mild high   TRUE   no))))
