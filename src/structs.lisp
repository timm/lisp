(defstruct  col  (n 0) (txt "") (w -1) (at 0))
(defstruct (skip (:include col)))
(defstruct (sym  (:include col))  seen mode (most 0))
(defstruct (num  (:include col))
  (w 1)
  (_all (make-array 32 :fill-pointer 0 :adjustable t))
  sorted)

(defstruct row _rows cells)

(defstruct cols 
   all    ; all the cols
   x      ; just the indendeont columns
   y      ; just the dependent columns
   klass) ; the klass column (if it exists)

(defstruct rows rows (cols (make-cols)))

