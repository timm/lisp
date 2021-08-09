; [aas](asda) [2121](asdaa)        
; [![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/Naereen/StrapDown.js/graphs/commit-activity)
; [![Ask Me Anything !](https://img.shields.io/badge/Ask%20me-anything-1abc9c.svg)](https://GitHub.com/Naereen/ama)
; [![GitHub license](https://img.shields.io/github/license/Naereen/StrapDown.js.svg)](https://github.com/Naereen/StrapDown.js/blob/master/LICENSE)
; [![GitHub release](https://img.shields.io/github/release/Naereen/StrapDown.js.svg)](https://GitHub.com/Naereen/StrapDown.js/releases/)
; [![Unlicense](https://img.shields.io/badge/License-Unlicense-blue.svg)](https://unlicense.org/)  
; 


;





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
