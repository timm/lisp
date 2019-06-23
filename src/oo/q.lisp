;-------- -------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load ".,/got"))

(defmacro ? (obj first-slot &rest more-slots)
  "From https://goo.gl/dqnmvH:"
  (if (null more-slots)
      `(slot-value ,obj ,first-slot)
      `(? (slot-value ,obj ,first-slot) ,@more-slots)))

