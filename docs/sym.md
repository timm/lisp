---
title: "sym: "
---

Methods
-------

```lisp
(defmethod initialize-instance :after ((s sym) &key) (print 1) s)
```

Add a symbo, update symbol counts,  update mode

```lisp
(defmethod add1 ((s sym) x)
  (let ((n (inca x (? s seen))))
    (when (> n (? s most))
      (setf (? s most) n
            (? s mode) x))))
```

Central tendency.

```lisp
(defmethod mid ((s sym)) (? s mode))
```

Variable around centrality.

```lisp
(defmethod var ((s sym)) (entropy (? s seen)))
```

Seperation of two items.

```lisp
(defmethod dist1 ((s sym) x y) (if (eql x y) 0 1))

```
