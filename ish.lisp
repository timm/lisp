#!/usr/bin/env sbcl --script
#+sbcl (declaim (sb-ext:muffle-conditions cl:style-warning))

(defvar *the* '(seed 42 file "auto93.csv"))

;;;; macros ----------------------------------------------------------
(defmacro ? (x) `(getf *the* ',x))

(defmacro o (x f &rest fs) 
  (if fs `(o (slot-value ,x ',f) . ,fs) `(slot-value ,x ',f)))

(set-macro-character #\$
  (lambda (s _) `(slot-value self ',(read s t nil t))))

(defmacro has (x lst) 
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x 0) ,lst))))))

;;;; structs ---------------------------------------------------------
(defstruct data rows cols)
(defstruct cols x y all names klass)
(defstruct sym (n 0) (at 0) (txt " ") has)
(defstruct num (n 0) (at 0) (txt " ") (mu 0) (m2 0) (goal 1))

;;;; create ----------------------------------------------------------
(defmethod new (x) x)

(defmethod new ((self num))
  (if (eq #\- (chr $txt -1)) (setf $goal -1))
  self)

(defmethod new ((self cols) &aux (at -1))
  (labels 
    ((what  (s) (if (upper-case-p (chr s 0)) #'make-num #'make-sym))
     (one   (s) (new (funcall (what s) :txt s :at (incf at))))
     (skipc (c) (chrs (o c txt) -1 #\- #\+ #\X))
     (goalc (c) (chrs (o c txt) -1 #\- #\+)))
    (setf $all (mapcar        #'one   $names)
          $x   (remove-if     #'skipc $all)
          $y   (remove-if-not #'goalc $all))
    self))

(defun adds (lst &optional (it (make-num)))
  (dolist (x lst it) (add it x)))

(defun add (self v &key (inc 1))
  (unless (eq v '?) (incf $n inc) (add1 self v))
  v)

(defmethod add1 ((self data) row &key (inc 1))
  (labels ((plus (col v) (add col v in)))
    (mapcar #'plus (o self cols all) row) 
    (push row $rows)))

(defmethod add1 ((self sym) v &key (inc 1)) 
  (incf (has v $has) inc))

(defmethod add1 ((self num) v &key (inc 1))
  (if (and (< inc 0) (< $n 2))
    (setf $mu 0 $m2 0 $sd 0 $n 0)
    (let* ((d (- v $mu)))
      (incf $mu (* inc (/ d $n)))
      (incf $m2 (* inc (* d (- v $mu)))))))

(defmethod div ((self num))
  (if (< $n 2) 0 (sqrt (/ (max 0 $m2) (1- $n)))))

;;;; lib ------------------------------------------------------------
(defun chr (s i) (char (string s) (if (minusp i) (+ (length s) i) i)))

(defun chrs (s i &rest lst) (member (chr s i) lst :test #'equal))

(defun thing (s &aux (s1 (string-trim '(#\Space #\Tab) s))) 
  (let ((x (let ((*read-eval* nil)) (read-from-string s1 ""))))
    (if (or (numberp x) (member x '(t nil ?))) x s1)))

(defun things (s &optional (sep #\,) (here 0)) 
  (let ((there (position sep s :start here)))
    (cons (thing (subseq s here there)) 
          (if there (things s sep (1+ there))))))

(defun mapcsv (fun file)
  (with-open-file (s (or file *standard-input*))
    (loop (funcall fun (things (or (read-line s nil) (return)))))))

;;;; main ------------------------------------------------------------
(print (new (make-num :txt "asdas-")))
(print (new (make-cols :names  '("asdas-" "age" "Aasda"))))

;(mapcsv #'print (? file))
