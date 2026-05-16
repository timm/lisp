; vim: set ft=lisp ts=2 sw=2 et lw+=defmethods,defeg,let+ :

; lib.lisp -- tiny CL helpers: reader macros, anaphora,
;             slot access, lambda shortcuts, utilities, CLI.
; (c) 2026 Tim Menzies, timm@ieee.org, MIT license.
;
; READER MACROS
;   $field  -> (slot-value i 'field)   ; i = self
;   @key    -> (the-of 'key)           ; app defines the-of
;
; ANAPHORA & SHORTCUTS
;   (? x a b c)   nested slot-value chain
;   (if+ t x y)   bind `it` to t's value in x/y
;   (! f a b)     funcall shortcut
;   (f+   body)   = (lambda (_)        body)
;   (ff+  body)   = (lambda (_ __)     body)
;   (fff+ body)   = (lambda (_ __ ___) body)
;
; METHOD GROUPING
;   (defmethods CLASS
;     (mname args body...) ...)
;   Each entry expands to (defmethod mname ((i CLASS) args) body).
;   For struct + constructor: write plain defstruct + defun.
;     (defstruct (foo (:constructor %make-foo)) ...)
;     (defun make-foo (... &aux (i (%make-foo ...))) ... i)
;
; SEQUENTIAL BINDINGS
;   (let+ ((var val)         ; let
;          ((a b) form)      ; multiple-value-bind (flat symbols)
;          ((a (b c)) v)     ; destructuring-bind  (nested pattern)
;          (name args body)) ; labels
;     ...)
;   Caveat: dbind on flat-symbol pattern conflicts; use plain
;   destructuring-bind in that case.
;
; UTILITIES
;   *seed*, rand, rint, shuffle
;   adds, sub, dist, lt, gt, aget, argm, stride
;   ch, cells, thing, read-csv
;
; CLI FRAMEWORK
;   App must define *the* + the-of; uses @key reader macro.
;   run, args, cli, defeg, eg--all

;## simplify debugging
#+sbcl (declaim (sb-ext:muffle-conditions
                  warning style-warning))
#+sbcl
(let ((die (lambda (c &rest _) (declare (ignore _))
             (format *error-output* "~&~a~%" c)
             (sb-ext:exit :code 1))))
  (setf sb-ext:*invoke-debugger-hook* die
        *debugger-hook* die)
  (sb-ext:without-package-locks
    (setf (symbol-function 'sb-debug::debugger-disabled-hook)
          die)))

;; =============================================
;; Reader macros
;; =============================================

(set-macro-character #\$
  (lambda (s c) (declare (ignore c))
    `(slot-value i ',(read s t nil t)))
  t)

(set-macro-character #\@
  (lambda (s c) (declare (ignore c))
    `(the-of ',(read s t nil t)))
  t)

;; =============================================
;; Macros
;; =============================================

(defmacro ? (x &rest at)
  "Nested slot access: (? x a b)
   = (slot-value (slot-value x 'a) 'b)."
  (if at `(? (slot-value ,x ',(car at)) ,@(cdr at))
         x))

(defmacro if+ (test then &optional else)
  "Anaphoric if: bind `it` to TEST in THEN/ELSE."
  `(let ((it ,test)) (if it ,then ,else)))

(defmacro f+   (&body b) `(lambda (_)        ,@b))
(defmacro ff+  (&body b) `(lambda (_ __)     ,@b))
(defmacro fff+ (&body b) `(lambda (_ __ ___) ,@b))

(defmacro ! (f &rest args)
  "Funcall shortcut: (! f a b) = (funcall f a b)."
  `(funcall ,f ,@args))

(defmacro defmethods (name &body methods)
  "Group defmethod forms with `i` bound to self.

   Each (mname (args...) body...) becomes
     (defmethod mname ((i NAME) args...) body...)."
  `(progn
     ,@(loop for (m args . body) in methods collect
             `(defmethod ,m ((i ,name) ,@args)
                ,@body))
     ',name))

(defmacro let+ (((lhs &rest rhs) &rest rest) &body body)
  "Sequential bindings; entry shape picks form:
     (var val)         -> let
     ((sym...) val)    -> multiple-value-bind
     ((pat) val)       -> destructuring-bind
     (name args body)  -> labels"
  (let ((tail (if rest `((let+ ,rest ,@body)) body)))
    (cond
      ((and (consp lhs) (every #'symbolp lhs))
       `(multiple-value-bind ,lhs ,(car rhs) ,@tail))
      ((consp lhs)
       `(destructuring-bind ,lhs ,(car rhs) ,@tail))
      ((cdr rhs)
       `(labels ((,lhs ,@rhs)) ,@tail))
      (t
       `(let ((,lhs ,(car rhs))) ,@tail)))))

;; =============================================
;; Utilities
;; =============================================

; ### Random
(defvar *seed* 1)

(defun rand (&optional (n 1))
  "Reproducible float in [0,n). Advances *seed*."
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 100) &aux (base 1E10))
  "Reproducible integer in [0,n)."
  (floor (* n (/ (rand base) base))))

(defun shuffle (lst &aux (v (coerce lst 'vector)))
  "Fisher-Yates shuffle of LST. Seeded via *seed*."
  (loop for i from (1- (length v)) downto 1 do
    (rotatef (aref v i) (aref v (rint (1+ i)))))
  (coerce v 'list))

; ### Stats
(defun adds (lst &optional (summary (make-num)))
  "Fold LST into SUMMARY via `add`."
  (dolist (v lst summary) (add summary v)))

(defun sub (it v) (add it v -1))

(defun dist (lst f &aux (d 0))
  "Minkowski @p-norm of (! f v) for v in LST."
  (dolist (v lst (expt (/ d (length lst)) (/ 1 @p)))
    (incf d (expt (! f v) @p))))

; ### Sort / select
(defun lt (key lst)
  "Sort LST ascending by KEY. Non-destructive."
  (sort (copy-list lst) #'< :key key))

(defun gt (key lst)
  "Sort LST descending by KEY. Non-destructive."
  (sort (copy-list lst) #'> :key key))

(defun aget (k alist &optional (default 0))
  "Alist lookup: cdr of (assoc k alist :test #'equal),
   or DEFAULT if missing. Default DEFAULT is 0."
  (let ((cell (assoc k alist :test #'equal)))
    (if cell (cdr cell) default)))

(defun argm (lst key &optional (cmp #'<))
  "Argmin (default) / argmax of LST under KEY.
   (argm rs #'cost)       = row with smallest cost.
   (argm rs #'cost #'>)   = row with largest cost.
   One pass, no allocation. Faster than (car (sort ...))."
  (let+ ((best (car lst)) (m (! key best)))
    (dolist (x (cdr lst) best)
      (let ((k (! key x)))
        (when (! cmp k m) (setf best x m k))))))

(defun stride (rows key &optional (n 30))
  "Sort ROWS by KEY, print every Nth row. For quick
   visual inspection of distribution along an axis."
  (loop for x in (sort rows #'< :key key)
        by (f+ (nthcdr n _)) do (print x)))

; ### Characters
(defun ch (s n)
  "Char at N of S. Negative N counts from end."
  (char s (if (minusp n) (+ (length s) n) n)))

; ### Strings / IO
(defun cells (s sep)
  "Split S on character SEP into substring list."
  (loop for start = 0 then (1+ end)
    for end = (position sep s :start start)
    collect (subseq s start end)
    while end))

(defun thing (str
              &aux (v (ignore-errors
                        (read-from-string str))))
  "Coerce STR to number; '? for \"?\"; else string."
  (cond ((numberp v) v)
        ((string= str "?") '?)
        (t str)))

(defun read-csv (file)
  "Read FILE as CSV; coerce cells via `thing`."
  (with-open-file (s file)
    (loop for line = (read-line s nil) while line
      collect (mapcar #'thing (cells line #\,)))))

;; =============================================
;; CLI framework
;; =============================================

(defmacro defeg (name doc &body body)
  "Define eg--NAME taking &optional file; bind rs+i (data)."
  `(defun ,name (&optional (file @file))
     ,doc
     (let+ ((rs (read-csv file)) (i (make-data rs)))
       (declare (ignorable rs i))
       ,@body)))

(defun eg--all (&optional (arg @file))
  (do-symbols (s *package*)
    (let ((n (symbol-name s)))
      (when (and (fboundp s) (not (eq s 'eg--all))
                 (> (length n) 4) (string= n "EG--" :end1 4))
        (run s arg)))))

(defun run (it &optional arg)
  "Dispatch --flag to EG-FLAG function."
  (let+ ((f (if (symbolp it) it
                (intern (format nil
                                "EG~:@(~a~)" it))))
         (n (symbol-name f)))
    (when (and (fboundp f)
               (> (length n) 3)
               (string= n "EG-" :end1 3))
      (setf *seed* @seed)
      (if arg (! f arg) (! f))
      t)))

(defun args ()
  "Argv as list of strings (SBCL/CLISP portable)."
  #+sbcl (cdr sb-ext:*posix-argv*)
  #+clisp ext:*args*)

(defun cli (lsts)
  "Walk argv (flag arg) pairs: dispatch or update."
  (loop for (flag arg) on (args) by #'cddr do
    (unless (run flag (thing arg))
      (if+ (find flag lsts
                 :key #'third :test #'equalp)
           (setf (second it) (thing arg))))))
