(defstruct thing)

; abstract super-class for columns
(defstruct (col (:include thing))
  (txt "") ; column name
  (at 0)   ; column position
  (n 0)    ; number of summarizes itemd
  (w 1)    ; weight
  )

; columns we are going to count
(defstruct (skip (:include col)))

; columns where we cound symbols (and the most common symbol,
; a.k.a the mode)
(defstruct (sym (:include col))  
  seen mode (most 0))

(defstruct (num (:include col))
  (_all (make-array 32 :fill-pointer 0 :adjustable t))
  sorted)

(defstruct (row (:include thing))
  _rows   ; pointer to "rows" holding this
  cells)  ; values in this row 

(defstruct (cols (:include thing))
   all    ; all the cols
   x      ; just the indendeont columns
   y      ; just the dependent columns
   klass) ; the klass column (if it exists)

(defstruct (rows (:include thing))
  rows                ; list of "row"
  (cols (make-cols))) ; column information
