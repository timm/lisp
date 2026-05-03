; vim: set ft=lisp ts=2 sw=2 et :

; glu.lisp -- tiny Common Lisp dialect: struct+methods,
;             anaphora, slot access, list comprehension.
; (c) 2026 Tim Menzies, timm@ieee.org, MIT license.
;
; Six macros and one rule: the subject is implicit.
;
; READER MACROS & ANAPHORA
;   $field  -> (slot-value i 'field)   ; i = self
;   (? x a) -> (slot-value x 'a); chains: (? x a b c)
;   (aif t x y)  binds `it` to t's value in x/y
;   (! f a b)    -> (funcall f a b)
;
; OO HELPERS
;   (glu name [doc] slots (m args body)...)
;     defstruct + defmethod each method on `i`.
;     Pseudo-methods:
;       (make ARGS BODY...)      body of make-NAME factory
;       (:opts ((opt ...) ...))  extra defstruct options
;     The macro always adds (:constructor %make-NAME).
;   (glu+ name (m args body)...)  add methods to a struct
;   (new cls k v ...)             -> (make-cls k v ...)
;
; LAMBDA SHORTCUTS
;   (fn   body)   = (lambda (_)        body)
;   (fnn  body)   = (lambda (_ __)     body)
;   (fnnn body)   = (lambda (_ __ ___) body)
;
; LIST COMPREHENSION
;   (for/ EXPR for var in lst [if TEST] ...)

;## simplify debugging
#+sbcl (declaim (sb-ext:muffle-conditions
                  warning style-warning))
#+sbcl (setf sb-ext:*invoke-debugger-hook*
             (lambda (c h) (declare (ignore h))
               (format *error-output* "~&[ERROR] ~a~%" c)
               (sb-ext:exit :code 1)))


(defmacro defread (name (stream) &body body)
  "Install BODY as reader-macro for char NAME."
  `(set-macro-character
     ,(character (symbol-name name))
     (lambda (,stream c) (declare (ignore c)) ,@body)
     t))

(defread $(s)
  `(slot-value i ',(read s t nil t)))

(defmacro ? (x &rest at)
  "Nested slot access: (? x a b)
   = (slot-value (slot-value x 'a) 'b)."
  (if at `(? (slot-value ,x ',(car at)) ,@(cdr at))
         x))

(defmacro aif (test then &optional else)
  "Anaphoric if: bind `it` to TEST in THEN/ELSE."
  `(let ((it ,test)) (if it ,then ,else)))

(defmacro fn   (&body b) `(lambda (_)         ,@b))
(defmacro fnn  (&body b) `(lambda (_ __)      ,@b))
(defmacro fnnn (&body b) `(lambda (_ __ ___)  ,@b))

(defmacro ! (f &rest args)
  "Funcall shortcut: (! f a b) = (funcall f a b)."
  `(funcall ,f ,@args))

(defun acc (c s) (intern (format nil "~A-~A" c s)))

(defmacro new (cls &rest kvs)
  "(new num :goal 0) -> (make-num :goal 0)."
  `(,(intern (format nil "MAKE-~A" cls)) ,@kvs))

(defmacro glu+ (name &body methods)
  "Add methods to an existing struct NAME.

   Each (mname (args...) body...) becomes
     (defmethod mname ((i NAME) args...) body...).

   make is NOT accepted here -- the constructor is a
   struct-time concern. Use GLU to define a struct with
   its make-NAME, then GLU+ to add more methods later."
  (when (find 'make methods :key #'car)
    (error "GLU+ does not accept make. Use GLU."))
  `(progn
     ,@(loop for (m args . body) in methods collect
             `(defmethod ,m ((i ,name) ,@args)
                ,@body))
     ',name))

(defmacro glu (name &rest rest)
  "Defstruct + methods on NAME.

   (glu NAME [DOC] SLOTS METHODS...)

   DOC (optional string) becomes the defstruct docstring.
   SLOTS is the slot list.
   Pseudo-methods:
     (make ARGS BODY...)      body of make-NAME factory.
     (:opts ((opt ...) ...))  extra defstruct options.
   The macro always adds (:constructor %make-NAME) so the
   private form is available; if no `make' is given, a thin
   wrapper (defun make-NAME (&rest args)
            (apply #'%make-NAME args)) is emitted.

   The method list (excluding `make') is forwarded to GLU+."
  (let* ((doc       (when (stringp (car rest)) (pop rest)))
         (slots     (pop rest))
         (methods   rest)
         (extra     (find :opts methods :key #'car))
         (methods   (remove :opts methods :key #'car))
         (make-spec (find 'make methods :key #'car))
         (methods   (remove 'make methods :key #'car))
         (priv      (intern (format nil "%MAKE-~A" name)))
         (pub       (intern (format nil  "MAKE-~A" name)))
         (make-defun
           (if make-spec
               `(defun ,pub ,(cadr make-spec) ,@(cddr make-spec))
               `(defun ,pub (&rest args) (apply #',priv args))))
         (opts (cons `(:constructor ,priv)
                     (and extra (cadr extra)))))
    `(progn
       (defstruct (,name ,@opts)
         ,@(when doc (list doc))
         ,@slots)
       ,make-defun
       (glu+ ,name ,@methods)
       ',name)))

(defmacro for/ (expr &rest cs)
  "List comprehension. Nil results are skipped.

   (for/ EXPR for var in lst [if TEST] ...)

   E.g.  (for/ (* x x) for x in '(1 2 3 4 5) if (oddp x))
                 ==> (1 9 25)"
  (labels ((walk (cs)
             (cond ((null cs)
                    `(let ((v ,expr)) (if v (list v) nil)))
                   ((eq (car cs) 'for)
                    `(loop for ,(cadr cs) in ,(cadddr cs)
                           append ,(walk (cddddr cs))))
                   ((eq (car cs) 'if)
                    `(if ,(cadr cs) ,(walk (cddr cs)) nil)))))
    (walk cs)))
