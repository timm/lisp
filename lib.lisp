; vim: set ft=lisp ts=2 sw=2 et :

; lib.lisp -- cool Common Lisp tricks, one place.
; (c) 2026 Tim Menzies, timm@ieee.org, MIT license.
;
; READER MACROS & ANAPHORA
;   $k      -> (at i 'k)   ; i = self
;   @key    -> (second (assoc 'key *the*))
;   (? x a b) -> (at (at x 'a) 'b); at = get by type (hash, list, vector, symbol, slot)
;   (aif t x y)  binds `it` to t's value in x/y
;   (! f a b)    -> (funcall f a b)
;
; LAMBDA SHORTCUTS
;   (fn   body)   = (lambda (_)        body)
;   (fnn  body)   = (lambda (_ __)     body)
;   (fnnn body)   = (lambda (_ __ ___) body)
;   (-> (a b) body) = (lambda (a b) body); (-> x body) ok too
;
; ITERATION & BINDING
;   (for+ EXPR for var in lst [if TEST] ...)   comprehension
;   (let+ ((var val) ((a b) mv) ((a (b)) d) (f args body)) ...)
;   (do-hash (k v h [result]) body)            maphash as loop
;   (incf (has key alist))                     alist counter
;
; UTILITIES
;   rand, rint, gauss, weibull, shuffle, near
;   sortby, argmin, argmax, slot-names, ch, cells, thing, things,
;   read-csv, mapcsv, wrap
;
; CLI FRAMEWORK
;   *the* config alist ((key default "-flag") ...); @key reads it.
;   run, args, cli, eg--all, eg-s
;   Apps: (load "lib"), set *the*, define eg-- functions,
;   call (cli *the*).

;## simplify debugging
#+sbcl (declaim (sb-ext:muffle-conditions
                  warning style-warning))
#+sbcl (setf sb-ext:*invoke-debugger-hook*
             (lambda (c h) (declare (ignore h))
               (format *error-output* "~&[ERROR] ~a~%" c)
               (sb-ext:exit :code 1)))

;; -- Macros ---------------------------------------------------
;; (at x k): one-step lookup, method picked by type. setf-able.
(defmethod at (x k)                  (slot-value x k))
(defmethod at ((x hash-table) k)     (gethash k x))
(defmethod at ((x cons) (k integer)) (nth k x))
(defmethod at ((x vector) k)         (aref x k))
(defmethod at ((x symbol) k)         (get x k))
(defmethod at ((x cons) k)
  (if (consp (car x))
    (cdr (assoc k x :test #'equal))
    (getf x k)))

(defmethod (setf at) (v x k)           (setf (slot-value x k) v))
(defmethod (setf at) (v (x hash-table) k) (setf (gethash k x) v))
(defmethod (setf at) (v (x cons) (k integer)) (setf (nth k x) v))
(defmethod (setf at) (v (x vector) k)        (setf (aref x k) v))
(defmethod (setf at) (v (x symbol) k)         (setf (get x k) v))
(defmethod (setf at) (v (x cons) k)
  (if (consp (car x))
    (setf (cdr (assoc k x :test #'equal)) v)
    (setf (getf x k) v)))

(defmacro ? (x &rest ks)
  "(? x a 0 :k) == (at (at (at x 'a) 0) :k). Bare symbols quoted. setf-able."
  (reduce (lambda (acc k) `(at ,acc ,(if (symbolp k) `',k k)))
          ks :initial-value x))

(set-macro-character #\$ (lambda (s c) (declare (ignore c))
                           `(at i ',(read s t nil t))))

(defmacro aif (test then &optional else)
  `(let ((it ,test)) (if it ,then ,else)))

(defmacro -> (&body b) `(lambda (%1 &optional %2 %3 %4 %5) ,@b))

(defun cli (&optional (x *the*))
  "update that option's default. --foo val: run eg--foo."
  (loop for (flag arg) on (args) by #'cddr do
    (aif (find flag (? x %meta) :key #'third :test #'equal)
         (progn (setf (second it) (thing arg)) (opts-reset x))
         (run (intern (format nil "EG~:@(~a~)" flag)) 
              (thing arg)))))

(defmacro defopts (&rest specs)
  `(progn
     (defstruct (opts (:predicate nil))
       (%meta (copy-tree ',specs))
       ,@(loop for (k v) in specs collect `(,k (copy-tree ',v))))
     (setf *the* (make-opts))))

(defun opts-reset (&optional (x *the*))
  (loop for (k v) in (? x %meta) do (setf (at x k) (copy-tree v))))

(defmacro has (x lst)
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x 0) ,lst))))))

(defmacro do-hash ((k v hash &optional result) &body body)
  "Loop over HASH binding K,V; return RESULT."
  `(progn (maphash (lambda (,k ,v) ,@body) ,hash) ,result))

(defmacro for+ (expr &rest cs)
  "List comprehension. Nil results are skipped.
   (for+ (* x x) for x in '(1 2 3 4 5) if (oddp x)) ==> (1 9 25)"
  (labels ((walk (cs)
             (cond ((null cs)
                    `(let ((v ,expr)) (if v (list v) nil)))
                   ((eq (car cs) 'for)
                    `(loop for ,(cadr cs) in ,(cadddr cs)
                           append ,(walk (cddddr cs))))
                   ((eq (car cs) 'if)
                    `(if ,(cadr cs) ,(walk (cddr cs)) nil)))))
    (walk cs)))

(defmacro let+ (((lhs &rest rhs) &rest rest) &body body)
  "Sequential bindings; entry shape picks form:
     (var val)         -> let
     ((sym...) val)    -> multiple-value-bind
     ((pat) val)       -> destructuring-bind  (pat has non-symbols)
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
(defun rand (&optional (n 1))
  "Reproducible float in [0,n). Advances *seed*."
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 100) &aux (base 1E10))
  "Reproducible integer in [0,n)."
  (floor (* n (/ (rand base) base))))

(defun gauss (&optional (mu 0) (sd 1))
  "Box-Muller normal sample."
  (+ mu (* sd (sqrt (* -2 (log (rand))))
           (cos (* 2 pi (rand))))))

(defun weibull (k lam)
  "Weibull sample, shape K, scale LAM."
  (* lam (expt (- (log (- 1 (rand)))) (/ 1 k))))

; ### Maths
(defun near (x y &optional (eps 0.01))
  "Close enough?"
  (< (abs (- x y)) eps))

; ### Lists
(defun shuffle (lst &aux (v (coerce lst 'vector)))
  "Fisher-Yates shuffle of LST. Seeded via *seed*."
  (loop for i from (1- (length v)) downto 1 do
    (rotatef (aref v i) (aref v (rint (1+ i)))))
  (coerce v 'list))

(defun sortby (key lst &optional (cmp #'<))
  "Non-destructive sort of LST under KEY."
  (sort (copy-list lst) cmp :key key))

(defun extremum-by (lst key cmp)
  "Item of LST whose KEY wins under CMP."
  (let+ ((best (car lst)) (m (! key best)))
    (dolist (x (cdr lst) best)
      (let ((k (! key x)))
        (when (! cmp k m) (setf best x m k))))))

(defun argmin (lst key) (extremum-by lst key #'<))
(defun argmax (lst key) (extremum-by lst key #'>))

; ### Structs / objects
#+sbcl
(defun slot-names (x)
  "Slot names of instance or struct X."
  (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of x))))

#+clisp
(defun slot-names (x)
  "Slot names of instance or struct X."
  (mapcar #'clos:slot-definition-name (clos:class-slots (class-of x))))

; ### Characters
(defun ch (s n)
  "Char at N of S (string or symbol). Negative N counts from end."
  (let ((s (string s)))
    (char s (if (minusp n) (+ (length s) n) n))))

; ### Strings / IO
(defun cells (s sep)
  "Split S on character SEP into substring list."
  (loop for start = 0 then (1+ end)
    for end = (position sep s :start start)
    collect (subseq s start end)
    while end))

(defun thing (str &aux (s (string-trim '(#\Space #\Tab) str)))
  "Coerce STR to number, t, nil, or ?; else trimmed string."
  (let ((v (ignore-errors
             (let ((*read-eval* nil)) (read-from-string s "")))))
    (if (or (numberp v) (member v '(t nil ?))) v s)))

(defun things (s &optional (sep #\,))
  "Split S on SEP, coercing each cell via `thing`."
  (mapcar #'thing (cells s sep)))

(defun read-csv (file)
  "Read FILE as CSV; coerce cells via `thing`."
  (with-open-file (s file)
    (loop for line = (read-line s nil) while line
      collect (things line))))

(defun mapcsv (fun &optional file)
  "Call FUN on each coerced row of FILE (or stdin), streaming."
  (let ((s (if file (open file) *standard-input*)))
    (unwind-protect
      (loop for line = (read-line s nil) while line
            do (! fun (things line)))
      (when file (close s)))))

(defun wrap (words &optional (w 60) &aux (c 0))
  "Print WORDS filled to width W."
  (dolist (x words (terpri))
    (let+ ((s (string-downcase (string x)))
           (n (length s)))
      (cond ((zerop c)         (princ s) (setf c n))
            ((> (+ c n 1) w)   (terpri) (princ s) (setf c n))
            (t (princ #\Space) (princ s) (incf c (1+ n)))))))

;; =============================================
;; CLI framework
;; =============================================

(defun run (it &optional arg)
  "Dispatch --flag to EG--FLAG function. Resets seed first."
  (let* ((f (if (symbolp it) it
                (intern (format nil "EG~:@(~a~)" it))))
         (n (symbol-name f)))
    (when (and (fboundp f)
               (> (length n) 3)
               (string= n "EG-" :end1 3))
      (setf *seed* @seed)
      (if arg (! f arg) (! f))
      t)))

(defun eg--all (&optional arg)
  "Run every eg-- function."
  (do-symbols (s *package*)
    (let ((n (symbol-name s)))
      (when (and (fboundp s) (not (eq s 'eg--all))
                 (> (length n) 4) (string= n "EG--" :end1 4))
        (run s arg)))))

(defun eg-s (&optional (seed @seed))
  "Set seed."
  (setf (second (assoc 'seed *the*)) seed
        *seed* seed))

(defun args ()
  "Argv as list of strings (SBCL/CLISP portable)."
  #+sbcl (cdr sb-ext:*posix-argv*)
  #+clisp ext:*args*)

(defun cli (lsts)
  (loop for (flag arg) on (args) by #'cddr do
    (unless (run flag (thing arg))
      (if+ (find flag lsts
                 :key #'third :test #'equalp)
           (setf (second it) (thing arg))))))

(defun slot-names (x)
  (let ((mop (or (find-package :sb-mop) (find-package :clos))))
    (mapcar (find-symbol "SLOT-DEFINITION-NAME" mop)
            (funcall (find-symbol "CLASS-SLOTS" mop) 
                     (class-of x))))
