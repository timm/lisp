; vim: noai:ts=2:sw=2:et: 

(defstruct about
  (who  "Tim Menzies")
  (why  "es.py /əˈspī/ verb LITERARY to glimpse what is hidden, or obscure")
  (what "data mining used for optimizing")
  (version 0.21)
  (copyleft "(c) 2021 Tim Menzies, MIT License"))

(defstruct options
  (about (make-about))
  (dir  "data")
  (data "auto93.csv")
  (seed 10013)
  (keep 1024)
  (no   "?")
  (sep  ","))

;-------- --------- --------- --------- --------- --------- --------- ----------
; macros and constants
(defconstant +lo+ most-negative-fixnum)
(defconstant +hi+ most-positive-fixnum)

(defvar *demos* nil)
(defvar *fails* 0)
(defvar *tries* 0)

(defmacro setor (x &rest body)
  `(or ,x (setf ,x (progn ,@body))))

(defmacro ? (obj f &rest fs)
  (if fs `(? (slot-value ,obj ',f) ,@fs) `(slot-value ,obj ',f)))

(defmacro defdemo (name args &optional (doc "") &body body)
  (pushnew name *demos*)
  `(defun ,name ,args ,doc
     (format t "~&~%;;; ~a~%; ~a~%" ',name ,doc)
     ,@body))

(defmacro ok (want got &optional (msg "") &rest txt)
  `(progn (incf *tries*)
          (handler-case
            (if (not (equalp ,want ,got))
              (error (format nil ,msg ,@txt)))
            (t (c)
               (incf *fails*)
               (format t "; E> ~a~%" c)))))

;-------- --------- --------- --------- --------- --------- --------- ----------
(defstruct span (lo +lo+) (hi +hi+) also)

(defmethod has ((s span) x)
  (with-slots (lo hi) s
    (if (equal lo hi) (equal x lo) (<= lo x hi))))

;-------- --------- --------- --------- --------- --------- --------- ----------
(defstruct col (n 0) (pos 0) (txt "") w  bins all)

(defmethod add ((c col) (lst cons)) 
  (dolist (x lst) (add c x)))

(defmethod add ((c col) x) 
  (unless (equal x "?") (incf (? c n)) (add1 c x))
  x)

(defmethod bins ((c col) tab)
  (setor (? c bins) (bins1 c tab)))

(defmethod w ((c col) &aux (s (? c txt)))
  (setor (? c w) (if (eql #\- (char s (1- (length s)))) -1 1)))

(defmethod bins1 ((c col) x) x)

;-------- --------- --------- --------- --------- --------- --------- ----------
(defstruct (num (:include col)) ok)

(defmethod add1 ((n num) x)
  (push x (? n all))
  (setf (? n ok) nil))

(defmethod all ((n num))
  (unless (? n ok)
      (setf (? n all) (sort (? n all) #'<)))
  (setf (? n ok) t)
  (? n all))

(defmethod norm ((n num) x) 
  (if (equal x "?")
      x
      (let ((lst (all n)))
        (/ (- x (nth 0 lst)) (- (cdr (last lst)) (nth 0 lst))))))

(defmethod per ((n num) p) (nth (1- (* p (length (all n)))) (all n)))
(defmethod mid ((n num))   (per n .5))
(defmethod sd  ((n num))   (/ (- (per n .9) (per n .1)) 2.56))

;-------- --------- --------- --------- --------- --------- --------- ----------
(defun espy (o) o)

;-------- --------- --------- --------- --------- --------- --------- ----------
;;;; lib
(let* ((seed 10013))
  (labels ((park-miller-randomizer ()
              (setf seed (mod (* 16807.0d0 seed) 2147483647.0d0))
                              (/ seed            2147483647.0d0)))
    (defun srand (o)  (setf seed (? o seed)))
    (defun randf (&optional (n 1)) (* n (- 1.0d0 (park-miller-randomizer))))
    (defun randi (n) (floor (* n (/ (randf 1000.0) 1000))))))

(defun is (x &aux (y (read-from-string x))) 
  (if (typep y 'number) y x))

(defun has (needle haystack &key (test 'char=))
  (not (null (search (string needle) (string haystack) :test test))))

;-------- --------- --------- --------- --------- --------- --------- ----------
(defdemo aa() "aa" (ok 2 1 "cheching not eq"))

;-------- --------- --------- --------- --------- --------- --------- ----------
; start up
(let (x 
     (o (make-options))) 
  (dolist (y (mapcar #'is sb-ext:*posix-argv*))
    (if (equal x "-keep")  (setf (? o keep) y))
    (if (equal x "-data")  (setf (? o data) y))
    (if (equal x "-data")  (setf (? o data) y))
    (if (equal x "-dir" )  (setf (? o dir)  y))
    (if (equal y "-demos") (mapcar #'funcall (reverse *demos*)))
    (if (equal x "-demo" ) (dolist (f (reverse *demos*))
                             (if  (has (string-upcase y) f) (funcall f))))
    (setf x y))
  (espy o)
  (sb-ext:exit :code (if (< *fails* 2) 0 1)))
