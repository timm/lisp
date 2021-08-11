---
title: "rows: "
---

Methods
-------

```lisp
(defmethod data ((rs rows) (x cons)) (data rs (coerce x 'vector)))
(defmethod data ((rs rows) (x row)) (data rs (? x cells0)))
(defmethod data ((rs rows) (x vector)) 
  (data rs (? x cells0)))

(defmacro cells ((cell col row &rest cols) &body)
  (let ((col1 (gensym)))
    `(dolist (,col1 ',cols)
       (dolist (,col (slot-value (? row _rows cols) ,col1))
         (let ((,cell (aref (? row cells) (? ,col at))))
           ,&body)))))
```

Functions
---------
create the  right  kind of column,
place it in  the  right kind  of places

```lisp
(defun column-factory (txt at rows)
  (let* ((what (if (upper-case-p (char txt 0)) 'num 'sum))
         (it   (make-instance what :txt txt :at at)))
    (if (has txt #\-) ; something to minimize
      (setf (? it w)  -1))
    (push it (? rows cols all)) 
    (when (not (has txt #\?)); not skipping
      (if (has txt #\!)  ; klass column
        (setf (? it rows cols klass) it))
      (if (has txt #\- #\+ #\!) ; goal column
        (push it (? rows cols y))
        (push it (? rows cols x))))
    it))

```
