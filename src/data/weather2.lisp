
(defun weather2 ()
  (data
   :name     'weather2
   :columns  '(forecast temp humidty windy play)
   :egs     '((sunny    hot  high   FALSE no) 
              (sunny    hot  high   TRUE  no)
              (rainy    mild high   TRUE  no)              
              (sunny    mild high   FALSE no)
              (rainy    cool normal TRUE  no)
              (rainy    mild high   FALSE yes)
              (rainy    cool normal FALSE yes)
              (sunny    cool normal FALSE yes)
              (rainy    mild normal FALSE yes)
              (sunny    mild normal TRUE  yes)
              (overcast cool normal TRUE  yes) 
              (overcast hot  high   FALSE yes)
              (overcast mild high   TRUE  yes)
              (overcast hot  normal FALSE yes))))


