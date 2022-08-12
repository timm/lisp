; Hold one record.
(defstruct+ row cells     ; cells 
                _about)   ; pointer to someone who can say what are (e.g.) lo,hi

(defun make-row (about l) (%make-row :cells l :_about about))
