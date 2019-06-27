;; vim: ts=2 sw=2 sts=2  et :
;-------- -------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(got "lib/")

(fyi
"The standard LISP object syntax is very verbose.
My `defthing` macro is a simpler way to specify an object.
E.g. here is a subclass of `thing` that has two slots
which initialize to a `gensym` and `nil`, respectively.

```lisp
(defthing keeper thing (id (gensym \"kept\")) (_cache))
```

Also, all my `thing`s know how to print their public slots
(which is any slot whose name does not start with `_`)")

(defclass thing () ())

(defmacro defthing (x parent &rest slots)
  "Succinct class creation"
  `(defclass ,x (,parent)
     ,(loop for (slot form) in slots collect 
            (_defslot slot x form))))

(defmethod print-object ((it thing) out)
  "for things, print all public slots"
  (let ((lst (mapcar
               #'(lambda (s)
                   (list s (slot-value it s)))
               (sort (_public-slot-names it)
                     #'string<))))
    (format out "~a" 
            (cons (class-name (class-of it)) lst))))

;;; utils
(defun _defslot (slot x form)
  "Helper function for defthing"
  `(,slot
     :initarg  ,(intern (symbol-name slot) "KEYWORD")
     :initform ,form
     :accessor ,(intern (format nil "~a-~a" x slot))))


(defmethod _public-slot-names ((it thing))
  "return all thing slots that don't start with '_'"
  (remove-if
    #'(lambda (x)
        (and (symbolp x)
             (equal (elt (symbol-name x) 0) #\_)))
    (mapcar
      #'klass-slot-definition-name
      (klass-slots it))))


