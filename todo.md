# todo

## prototype-OO experiment

Alternative to `plus` macro: classless, prototype-based objects (Self/JS-style).
Each instance carries pointer to shared method table; runtime method swap, free
inheritance via parent chain. Cost: hash lookup per call vs CLOS dispatch cache.

```lisp
(defstruct proto slots methods parent)

(defun new (parent &rest kvs &aux (i (make-proto :parent parent
                                                  :slots (make-hash-table)
                                                  :methods (make-hash-table))))
  (loop for (k v) on kvs by #'cddr do (setf (gethash k $slots) v))
  i)

(defun pull (o k) (loop for p = o then (? p parent) while p
                    for v = (gethash k (? p slots) :nope)
                    unless (eq v :nope) return v))

(defun push! (o k v) (setf (gethash k (? o slots)) v))

(defun def!  (o m fn) (setf (gethash m (? o methods)) fn))

(defun send (m o &rest args)
  (loop for p = o then (? p parent) while p
    for fn = (gethash m (? p methods))
    when fn return (apply fn o args)))

(defmacro ~ (m o &rest args) `(send ',m ,o ,@args))   ; (~ add n 5)

;; --- usage ---
(defparameter *num* (new nil))
(def! *num* 'add (ff+ (push! _ 'mu (+ (or (pull _ 'mu) 0) __)) _))
(def! *num* 'mid (f+  (pull _ 'mu)))

(let+ ((n (new *num* 'mu 0)))
  (~ add n 5) (~ add n 3)
  (print (~ mid n)))                 ; => 8
```

Tradeoffs vs current `plus` (CLOS-based):
- pro: monkey-patch live, prototype chain, no fixed class hierarchy
- con: hash lookup per call, no compile-time arg checking, no CLOS tooling
- verdict: keep `plus`/CLOS for fri.lisp; prototype only earns keep when
  per-instance behavior diverges
