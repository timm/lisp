#!/usr/bin/env sbcl --script
;; <!-- vim: set lispwords+=has,loop,format ts=2 sw=2 sts=2 et : -->
#+sbcl (declaim (sb-ext:muffle-conditions cl:style-warning))

(defvar *help* "
ezr.lisp: multi-objective explanation
(c) 2025, Tim Menzies <timm@ieee.org>, MIT license")

(defvar *options* '(
  (k     "-k"  "kth value"        2)
  (goal  "-g"  "start-up action"  "one")
  (seed  "-s"  "random number"    1234567891)
  (file  "-f"  "data file"        
         "../moot/optimize/misc/auto93.csv")))

;-------------------------------------------------------------------------------
(defstruct data rows cols)
(defstruct cols x y all names klass)
(defstruct sym (n 0) (at 0) (txt " ") has)
(defstruct num (n 0) (at 0) (txt " ") (mu 0) (m2 0) 
               (sd 0) (lo 1e32) (hi -1e32) (goal 1))

;-------------------------------------------------------------------------------
(defmacro ? (x) `(fourth (assoc ',x *options*)))

(defmacro run (&body src)
  #-sbcl `(progn ,@bsrc)
  #+sbcl `(handler-case (progn ,@src) (error (e) (format t "❌ bad: ~A~%" e))))

(set-macro-character #\$
  (lambda (stream char) `(slot-value self ',(read stream t nil t))))

;-------------------------------------------------------------------------------
(defmethod new ((self sym) &key inits (at 0) (txt " "))
  (setf $at at $txt txt)
  (adds inits self))

(defmethod new ((self num) &key inits (at 0) (txt " "))
  (setf $at at $txt txt $goal (if (chrp txt -1 #\-)  0 1))
  (adds inits self))

(defmethod new ((self data) &key inits)
  (if (stringp inits) 
    (mapcsv (lambda (x) (add self x)) inits)
    (adds inits self))
  self)

(defmethod new ((self cols) &key names)
  (dolist (txt names)
    (let* ((zz   (chr txt -1))
           (what (if (upper-case-p (chr txt 0))  #'make-num #'make-sym))
           (col  (new (funcall what :txt txt :at (length $all)))))
      (push col $all)
      (unless (eql zz  #\X)
        (if (eql zz #\!) (setf $klass col))
        (if (member zz '(#\! #\- #\+)) (push col $y) (push col $x)))))
  (setf $names names $x (reverse $x) $y (reverse $y) $all (reverse $all))
  self)

;------------------------------------------------------------------------------
(defun adds (lst &optional it)
  (dolist (x lst it)
    (setf it (or it (new (make-num))))
    (add it x)))

(defmethod sub (self v &key zap)  (add self v :zap zap :inc -1))

(defmethod add ((self sym) v &key (inc 1)) 
  (unless (eql v "?")
    (incf $n inc)
    (incf (cdr (or (assoc v $has :test #'equal)
                   (car (setf $has (cons (cons v 0) $has))))) 
          inc))
  v)

(defmethod add ((self num) v &key (inc 1))
  (unless (eql v '?)
    (incf $n inc)
    (setf $lo (min v $lo)
          $hi (max v $hi))
    (if (and (< inc 0) (< $n 2))
      (setf $mu 0 $m2 0 $sd 0 $n 0)
      (let* ((d (- v $mu)))
        (incf $mu (* inc (/ d $n)))
        (incf $m2 (* inc (* d (- v $mu))))
        (setf $sd (if (< $n 2) 0 (sqrt (/ (max 0 $m2) (1- $n))))))))
  v)

(defmethod add ((self data) (row cons) &key (inc 1) zap )
  (cond ((not $cols) (setf $cols (new (make-cols) :names row)))
        ((> inc 0)   (push row $rows)
                     (add $cols row :inc inc))
        (zap         (setf $rows (remove row $rows :test #'equal))
                     (add $cols row :inc inc))))

(defmethod add ((self cols) row &key (inc 1))
  (mapcar (lambda (col x) (add col x :inc inc)) $all row))

;;------------------------------------------------------------------------------
(defmethod mid ((self data)) 
  (mapcar #'mid (cols-all $cols)))

(defmethod mid ((self num)) $mu)
(defmethod mid ((self sym))
  (car (reduce (lambda (a b) (if (> (cdr a) (cdr b)) a b)) $has)))

(defmethod div ((self data)) 
  (mapcar #'div (cols-all $cols)))

(defmethod div ((self num)) $sd)
(defmethod div ((self sym))
  (- (loop :for (_ . n) :in $has :sum  (* (/ n $n) (log (/ n $n) 2)))))

;;------------------------------------------------------------------------------
;; ## Functions
(defun args () (cdr #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*))

(defun chr (s i) (char (string s) (if (minusp i) (+ (length s) i) i)))

(defun chrp (s i c) (char= (chr s i) c))

(defun near (x y &optional (eps 0.01)) (< (abs (- x y)) eps))

(defvar *seed* 1234567891)

(defun rand (&optional (n 1)) 
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 100) &aux (base 1E10)) 
  (floor (* n (/ (rand base) base))))

(defun gauss (m sd)
  (+ m (* sd (sqrt (* -2 (log (rand 1.0)))) (cos (* 2 pi (rand 1.0))))))

(defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s))) 
  (let ((x (let ((*read-eval* nil)) (read-from-string s1 ""))))
    (if (or (numberp x) (member x '(t nil ?))) x s1)))

(defun things (s &optional (sep #\,) (here 0)) 
  (let ((there (position sep s :start here)))
    (cons (thing (subseq s here there))
          (if there (things s sep (1+ there))))))

(defun mapcsv (fun file)
  (with-open-file (s (or file *standard-input*))
    (loop (funcall fun (things (or (read-line s nil) 
                                   (return)))))))

;;---------------------------------------------------------------------------
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
  (loop :for (s isa) :in '(("10.1" 10.1) ("3" 3) ("abc" "abc"))
    :do (format t "~a ~a ~a~%" s (thing s) 
          (assert (equal (type-of isa) (type-of (thing s)))))))

(defun eg--sym (_) 
  (let ((self (adds '(a a a a b b c) (make-sym))))
    (assert (near 1.38 (div self)))))

(defun eg--mids(_) 
  (print (mid (new (make-data) :inits (? file)))))

(defun eg--divs(_) 
  (print (div (new (make-data) :inits (? file)))))

;;-----------------------------------------------------------------------------
;; ## Main
(defun cli (options &aux it)
  (loop :for (key flag help b4) :in options :collect
    (list key flag help 
          (if (setf it (member flag (args) :test #'string=))
            (cond ((eq b4 t) nil)
                  ((eq b4 nil) t)
                  (t (thing (second it))))
            b4))))

;;-----------------------------------------------------------------------------
(setf *seed* (? seed))
(when (equal *load-truename* (truename *load-pathname*))
  (setf *options* (cli *options*))
  (loop :for (flag arg) :on (args) :by #'cdr :do
    (let ((com (intern (format nil "EG~:@(~a~)" flag))))
      (when (fboundp com)
        (setf *seed* (? seed))
        (run (funcall com (if arg (thing arg))))))))
