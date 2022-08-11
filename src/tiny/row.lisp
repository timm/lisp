(defstruct+ row  cells _about)

(defun make-row (about l)   (%make-row :cells l :_about about))


