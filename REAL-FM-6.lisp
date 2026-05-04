(defparameter *real_fm_6*
  '(and cellphone
  (opt wireless
    (or infrared bluetooth))
  (and accu_cell
    (xor li_ion ni_mh ni_ca))
  (and display
    (xor color monochrome))))
