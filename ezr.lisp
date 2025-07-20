#!/usr/bin/env sbcl --script
;; <!-- vim: set lispwords+=has,loop,format ts=2 sw=2 sts=2 et : -->
#+sbcl (declaim (sb-ext:muffle-conditions cl:style-warning))

(defvar *help* "
ezr.lisp: multi-objective explanation
(c) 2025, Tim Menzies <timm@ieee.org>, MIT license")

;; Options
(defvar  *options* '(
  (k     "-k"  "kth value"        2)
  (goal  "-g"  "start-up action"  "one")
  (seed  "-s"  "random number"    1234567891)
  (file  "-f"  "data file"        
         "../../moot/optimize/misc/auto93.csv")))

;; Access options
(defmacro ? (x) `(fourth (assoc ',x *options*)))

;; Structs
(defstruct data rows cols)
(defstruct cols x y all names klass)
(defstruct sym (n 0) (at 0) (txt " ") has)
(defstruct num (n 0) (at 0) (txt " ") (mu 0) (m2 0) 
               (sd 0) (lo 1e32) (hi -1e32) (goal 1))

;;------------------------------------------------------------------------------
;; ## Macros

;; Ensure `lst` has a counter for `x`   
;; (so `(incf (has x lst))` can increment).
(defmacro has (x lst) 
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x 0) ,lst))))))

;; Short cut for slot access within `self`.
(set-macro-character #\$
  (lambda (stream char) `(slot-value self ',(read stream t nil t))))

;; Nested access to slots."
(defmacro o (x f &rest fs)
  (if fs `(o (slot-value ,x ',f) . ,fs)
    `(slot-value ,x ',f)))

(defmacro run(&body body)
  #-sbcl `(progn ,@body)
  #+sbcl `(handler-case 
            (progn ,@body) (error (e) (format t "❌ Error: ~A~%" e))))


;;-----------------------------------------------------------------------------
;; ## Make nu things

;; Constructor for somwhere to store symbols.
(defun nuSym (&key inits (at 0) (txt " "))
  (adds inits (make-sym :at at :txt txt)))

;; Constructor for somwhere to store num.
(defun nuNum (&key inits (at 0) (txt " "))
  (adds inits (make-num :at at :txt txt :goal (if (chrp txt -1 #\-)  0 1))))

;; Constructor for somwhere to store rows, summarizeed  in cols.
(defun nuData (&optional (inits nil)  &aux (self (make-data)))
  (if (stringp inits) 
    (mapcsv (-> x (add self x)) inits)
    (mapcar (-> x (add self x)) inits))
  self)

;; Constructor that converts list of strings into NUMs or SYMs.
(defun nuCols (names &aux x y all klass)
  (dolist (txt names)
    (let* ((aa   (chr txt 0))
           (zz   (chr txt -1))
           (what (if (upper-case-p aa)  #'nuNum #'nuSym))
           (col  (funcall what :txt txt :at (length all))))
      (push col all)
      (unless (eql zz  #\X)
        (if (eql zz #\!) (setf klass col))
        (if (member zz '(#\! #\- #\+)) 
          (push col y)
          (push col x)))))
  (make-cols :names names :klass klass 
             :x (reverse x) :y (reverse y) :all (reverse all)))

;;------------------------------------------------------------------------------
;; ## Update

;; Multiple updates.
(defun adds (lst &optional it)
  (dolist (x lst it)
    (setf it (or it (if (numberp x) (make-num) (make-sym))))
    (add it x)))

;; Subtraction is just adding "-1".
(defmethod sub (self v &key zap)  (add self v :zap zap :inc -1))

;; Updating SYMs.
(defmethod add ((self sym) v &key (inc 1)) 
  (when (not (eq v '?))
    (incf $n inc)
    (incf (has v $has) inc))
  v)

;; Updating NUMs.
(defmethod add ((self num) v &key (inc 1))
  (unless (eq v '?)
    (incf $n inc)
    (setf $lo (min v $lo)
          $hi (max v $hi))
    (if (and (< inc 0) (< $n 2))
      (setf $mu 0 $m2 0 $sd 0 $n 0)
      (let* ((d (- v $mu)))
        (setf $mu (+ $mu (* inc (/ d $n)))
              $m2 (+ $m2 (* inc (* d (- v $mu))))
              $sd (if (< $n 2) 0 (sqrt (/ (max 0 $m2) (1- $n))))))))
  v)

;; Updating DATA.
(defmethod add ((self data) (row cons) &key (inc 1) zap )
  (cond ((not $cols)
         (setf $cols (nuCols row)))
        ((> inc 0)
         (push row $rows)
         (add $cols row :inc inc))
        (zap
          (setf $rows (remove row $rows :test #'equal))
          (add $cols row :inc inc))))

;; Updating COLS.
(defmethod add ((self cols) row &key (inc 1))
  (mapcar (-> (col x) (add col x :inc inc)) $all row))

;;------------------------------------------------------------------------------
;; ## Query

;; ### Central Tendancy

;; Mean
(defmethod mid ((self num)) $mu)

;; Mode
(defmethod mid ((self sym))
  (car (reduce (-> (a b) (if (> (cdr a) (cdr b)) a b)) $has)))

;; ### Diversity
;; "Diversity" = variation away from central tendancy

;; Standard deviation
(defmethod div ((self num)) $sd)

;; Entropy
(defmethod div ((self sym))
  (- (loop :for (_ . n) :in $has :sum  (* (/ n $n) (log (/ n $n) 2)))))


;;------------------------------------------------------------------------------
;; ## Functions

;; ### Misc

;; Access command line
(defun args () (cdr #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*))

;; Get i-th item of string/symbol (e.g. `(chr -1)` is the last item).
(defun chr (s i) (char (string s) (if (minusp i) (+ (length s) i) i)))

;; check if the ith item of  string/smpbol is a particular character
(defun chrp (s i c) (char= (chr s i) c))

;; ### Maths

;; Close enough
(defun near (x y &optional (eps 0.01)) (< (abs (- x y)) eps))

;; ### Random Numbers

;; seed
(defvar *seed* 1234567891)

;; Random floats.
(defun rand (&optional (n 1)) 
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

;; Random integers.
(defun rint (&optional (n 100) &aux (base 1E10)) 
  (floor (* n (/ (rand base) base))))

;; Sample from a Gaussian
(defun gauss (m sd)
  (+ m (* sd (sqrt (* -2 (log (rand 1.0)))) (cos (* 2 pi (rand 1.0))))))

;; ### String 2 Thing

;; String -> atom
(defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s))) 
  (let ((x (let ((*read-eval* nil)) (read-from-string s1 ""))))
    (if (or (numberp x) (member x '(t nil ?))) x s1)))

;; string -> list of atoms (dividing on comma)
(defun things (s &optional (sep #\,) (here 0)) 
  (let ((there (position sep s :start here)))
    (cons (thing (subseq s here there))
          (if there (things s sep (1+ there))))))

;; csv file -> list of list of atom.
(defun mapcsv (fun file)
  (with-open-file (s (or file *standard-input*))
    (loop (funcall fun (things (or (read-line s nil) 
                                   (return)))))))

;;---------------------------------------------------------------------------
;; ## Examples

(defun eg-h (_) 
  (format t "~a~%~%Options:~%" *help*)
  (loop :for (key flag help default) :in *options* :do
    (format t "~26a ~a~%"
      (format nil "   ~a  ~(~a~)=~a" flag key default) help)))

(defun eg--csv (_) (mapcsv #'print (? file)))
(defun eg--the (_) (print *options*))
(defun eg--rand(_)
  (setf *seed* (? seed)) 
  (let ((a (rand)) (b (rand  10)))
    (print b)
    (setf *seed* (? seed)) 
    (let ((c (rand))) (assert (and (eql a c) (not (eql a b)))))))

(defun eg--gauss(_)
  (let ((self (adds (loop repeat 1000 collect (gauss 10 1)))))
    (assert (and (near 10 $mu 0.02) (near 1 $sd)))))

(defun eg--thing (_)
  (loop for (s isa) in '(("10.1" float) ("3" integer) ("abc" string))
       do (format t "~a ~a ~a~%" s (thing s) (eql isa (type-of (thing s))))))

(defun eg--sym (_) 
  (let ((self (adds '(a a a a b b c))))
    (assert (near 1.38 (div self)))))

(defun eg--data(_) 
  (let ((self (nuData (? file))))
    (mapcar 'print (o $cols y))))

;;-----------------------------------------------------------------------------
;; ## Main

;; Update *options* from command-line.
(defun cli (options &aux it)
  (loop :for (key flag help b4) :in options :collect
    (list key flag help (if (setf it (member flag (args) :test #'string=))
                          (cond ((eq b4 t) nil)
                                ((eq b4 nil) t)
                                (t (thing (second it))))
                          b4))))

;; Only called if we are the top-level. And when we run, do not show long taces.
(defmacro when-main (&body body)
  `(when (and *load-pathname* *load-truename*
              (equal (truename *load-pathname*) *load-truename*))
     #-sbcl (progn ,@body)
     #+sbcl (handler-case 
              (progn ,@body) (error (e) (format t "❌ Error: ~A~%" e)))))

;;-----------------------------------------------------------------------------
;; ## Start
  
(setf *seed* (? seed))
(when-main
  (setf *options* (cli *options*))
  (loop :for (flag arg) :on (args) :by #'cdr :do
    (let ((com (intern (format nil "EG~:@(~a~)" flag))))
      (when (fboundp com)
        (format *error-output*  "% ~a~%" flag)
        (setf *seed* (? seed))
        (funcall com (if arg (thing arg)))))))
