---
title: "structs: "
---

abstract super-class for columns

```lisp
(defstruct (col (:include thing))
  (txt "") ; column name
  (at 0)   ; column position
  (n 0)    ; number of summarizes itemd
  (w 1)    ; weight
  )
```

columns we are going to count

```lisp
(defstruct (skip (:include col)))
```

columns where we cound symbols (and the most common symbol,
a.k.a the mode)

```lisp
(defstruct (sym (:include col))  
  seen mode (most 0))

(defstruct (num (:include col))
  (_all (make-array 32 :fill-pointer 0 :adjustable t))
  sorted)

(defstruct xy all x y)

(defstruct (row (:include thing))
  _rows              ; pointer to "rows" holding this
  (cells (make-xy))) ; values in this row 

(defstruct (cols (:include thing))
   names           ; all the row1 names
   all
   x
   y
   klass)          ; the klass column (if it exists)

(defstruct (rows (:include thing))
  (txt "")            ; text description of source
  rows                ; list of "row"
  (cols (make-cols))) ; column information

```
