(defstruct thing)

(defstruct (col  (:include thing))
  (n 0) (txt "") (w 1) (at 0))

(defstruct (skip (:include col)))

(defstruct (sym  (:include col))  
  seen mode (most 0))

(defstruct (num  (:include col))
  (_all (make-array 32 :fill-pointer 0 :adjustable t))
  sorted)

(defstruct (row (:include thing))
  _rows ; pointer to "rows" holding this
  cells); values in this row 

(defstruct (cols (:include thing))
   all    ; all the cols
   x      ; just the indendeont columns
   y      ; just the dependent columns
   klass) ; the klass column (if it exists)

(defstruct (rows (:include thing))
  rows               ; list of "row"
  (cols (make-cols)) ; column information
)

