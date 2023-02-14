(defvar *settings* '((a . 1) (b . 2) (c . 3)))

(defmacro ? (x &optional (lst '*settings*))
  `(cdr (assoc ',x ,lst :test #'equal)))

(print (? a *settings*))
