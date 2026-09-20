; vim: set ft=lisp ts=2 sw=2 et :
; lib.lisp -- general kit: access, macros, utils, cli.
; (c) 2026 Tim Menzies, timm@ieee.org, MIT license.
;
; Apps (see ezr.lisp) supply their own `defaults`, set
; *the* from it, define eg--NAME functions, then (cli).
;
; ACCESS
;   $k        -> (slot-value i 'k)      ; i = self
;   (? x a b) -> (slot-value (slot-value x 'a) 'b)
;   (! seed)  -> option value from *the*
;   both $ and ? are setf-able.
;
; SHORTCUTS
;   (aif t x y)      binds `it` to t's value in x/y
;   (-> body)        lambda; args are %1 .. %5
;   (has k alist)    alist counter; (incf (has k c))
;   (let+ ((a 1) ((b c) form) (f (x) body)) ...)
;
; CLI
;   *the* holds (key flag doc value) rows.
;   -s 42    sets that option.
;   --foo    runs (eg--foo) on fresh options, fresh seed.
;   cli exits with the number of failures.

#+sbcl (declaim (sb-ext:muffle-conditions
                  warning style-warning))

(defvar *the*  nil)
(defvar *seed* 1234567891)

;;; ---- macros ------------------------------------------------
(defmacro ! (x) `(fourth (assoc ',x *the*)))

(defmacro aif (test then &optional else)
  `(let ((it ,test)) (if it ,then ,else)))

(defmacro -> (&body b)
  "Short lambda; args %1 .. %5, all optional but %1."
  `(lambda (%1 &optional %2 %3 %4 %5)
     (declare (ignorable %1 %2 %3 %4 %5))
     ,@b))

(defmacro ? (x k &rest ks)
  "(? x a b) = (slot-value (slot-value x 'a) 'b)."
  (if ks `(? (slot-value ,x ',k) ,@ks)
         `(slot-value ,x ',k)))

(set-macro-character #\$
  (lambda (s c) (declare (ignore c))
    `(slot-value i ',(read s t nil t))))

(defmacro has (x lst)
  "Alist place for X, adding (X . 0) if missing."
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x 0) ,lst))))))

(defmacro let+ (((lhs &rest rhs) &rest rest) &body body)
  "Sequential bindings; entry shape picks the form:
     (var val)        -> let
     ((sym...) val)   -> multiple-value-bind
     ((pat) val)      -> destructuring-bind
     (name args body) -> labels"
  (let ((tail (if rest `((let+ ,rest ,@body)) body)))
    (cond
      ((and (consp lhs) (every #'symbolp lhs))
       `(multiple-value-bind ,lhs ,(car rhs) ,@tail))
      ((consp lhs)
       `(destructuring-bind ,lhs ,(car rhs) ,@tail))
      ((cdr rhs) `(labels ((,lhs ,@rhs)) ,@tail))
      (t `(let ((,lhs ,(car rhs))) ,@tail)))))

;;; ---- random ------------------------------------------------
(defun rand (&optional (n 1))
  "Reproducible float in [0,n). Advances *seed*."
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 100) &aux (base 1E10))
  "Reproducible integer in [0,n)."
  (floor (* n (/ (rand base) base))))

(defun shuffle (lst &aux (v (coerce lst 'vector)))
  "Fisher-Yates shuffle, seeded via *seed*."
  (loop for i from (1- (length v)) downto 1 do
    (rotatef (aref v i) (aref v (rint (1+ i)))))
  (coerce v 'list))

;;; ---- strings and io ----------------------------------------
(defun thing (str &aux (*read-eval* nil))
  "Coerce STR to number, t, nil or ?; else STR."
  (let ((v (ignore-errors (read-from-string str ""))))
    (if (or (numberp v) (member v '(t nil ?))) v str)))

(defun things (s &optional (sep #\,))
  "Split S on SEP, coercing each cell."
  (loop for start = 0 then (1+ end)
    for end = (position sep s :start start)
    collect (thing (subseq s start end))
    while end))

(defun read-csv (file)
  "Read FILE as CSV, coercing every cell."
  (with-open-file (s file)
    (loop for line = (read-line s nil) while line
      collect (things line))))

(defun kv (&rest kvs)
  "Print KEY VALUE pairs, one per line."
  (loop for (k v) on kvs by #'cddr
        do (format t "~&~(~a~)~10t~s~%" k v)))

(defun slot-names (x)
  "Slot names of instance or struct X."
  (let ((mop (or (find-package :sb-mop)
                 (find-package :clos))))
    (mapcar (find-symbol "SLOT-DEFINITION-NAME" mop)
            (funcall (find-symbol "CLASS-SLOTS" mop)
                     (class-of x)))))

;;; ---- cli ---------------------------------------------------
(defun egp (flag)
  "--foo -> EG--FOO, if such a function exists."
  (let ((f (intern (format nil "EG~:@(~a~)" flag))))
    (and (fboundp f) f)))

(defun run (f &optional (fails 0))
  "Reset seed, call F. On error: one line, 1+ FAILS."
  (setf *seed* (! seed))
  (handler-case (progn (funcall f) fails)
    (error (e)
      (format t "~&!! ~(~a~): ~a~%" f
              (substitute #\Space #\Newline
                          (princ-to-string e)))
      (1+ fails))))

(defun cli (&optional
            (av #+sbcl  (cdr sb-ext:*posix-argv*)
                #+clisp ext:*args*)
            &aux (saved (copy-tree *the*)) (bad 0))
  "-s 42 sets an option. --foo runs (eg--foo). Then exits."
  (loop for flag = (pop av) while flag do
    (let ((spec (find flag saved :key #'second
                      :test #'equal))
          (f    (egp flag)))
      (cond (spec (setf (fourth spec) (thing (pop av))))
            (f    (setf *the* (copy-tree saved))
                  (setf bad (run f bad)))
            (t    (format t "?? ~a~%" flag)))))
  (when (plusp bad) (format t "~&fails=~a~%" bad))
  #+sbcl  (sb-ext:exit :code bad)
  #+clisp (ext:exit bad))
