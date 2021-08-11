---
title: "sandbox: "
---


```lisp
(defun help-text (x) (print 111) (print (second x)) (print (fourth x)) x)
(reader #\! help-text)

!(defmacro aif (test yes &optional no) "asdas" `(let ((it ,test)) (if it ,yes ,no)))

(aif nil (print it) (print 'nl))

(format t "as ~s ~s ~%" 1 2)

```
