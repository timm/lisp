---
title: "col: "
---

Methods
-------
If `x` is a list, add everything in it.

```lisp
(defmethod add ((c col) (x cons)) (dolist (y x c) (add c y)))
```

Unless we are skipping  stuff, increment `n`.

```lisp
(defmethod add ((c col) x)
  (unless (eq #\? x) 
    (incf (? c n)) 
    (add1 c x))
  x)
```

Functions
---------

```lisp
(defmethod dist (c x y)
  (if (and (eq x #\?) (eq y #\?)) 1 (dist1 x y)))

```
