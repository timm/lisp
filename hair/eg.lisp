; vim: set ts=2 sw=2 sts=2 et lisp cc=75 lispwords+=loop,defm,aif,labels :
;;; eg: the dialog. Everything that reads csv files, prints, or runs
;;; experiments; ai.lisp is the model it talks to. eg-NAME runs from
;;; the command line: sbcl --script hair.lisp [--key=val ...] NAME
;;; (c) 2026, Tim Menzies <timm@ieee.org>, MIT license
(in-package :hair)
(export '(show cli))

;;;; helpers -----------------------------------------------------------
(defun data (&optional (f (? *k* file))) "A tbl from csv file f"
  (tbl (csv f)))

(defun show (tr) "Print tree tr: per node, mid, n, then its cut"
  (loop :for (lvl test mu n) :in (walk tr)
        :do (format t "~6,2f ~5d  ~{~a~}~:[all~;~:*~{~a ~a ~a~}~]~%"
                    mu n (make-list lvl :initial-element "|  ") test)))

(defun timed (fn &aux (t0 (get-internal-real-time)))
  "(fn)'s result; and its secs"
  (values (funcall fn) (/ (- (get-internal-real-time) t0) 1.0
                          internal-time-units-per-second)))

;;;; egs ---------------------------------------------------------------
(defun eg-config () (format t "~a~%" *k*))

(defun eg-tbl (&aux (tb (data)))
  (format t "~a rows, ~a x, ~a y~%"
          (length (? tb rows))
          (length (? tb cols x)) (length (? tb cols y)))
  (dolist (c (? tb cols all)) (format t "~10a ~,2f~%" (? c txt) (mid c))))

(defun eg-dist (&aux (tb (data))
                     (rows (sort (copy-list (? tb rows)) #'<
                                 :key (lambda (r) (disty tb r)))))
  (dolist (r (subseq rows 0 3)) (format t "~,2f ~a~%" (disty tb r) r))
  (format t "nearest to first: ~a~%" (second (near tb (car rows)))))

(defun eg-same ()
  (let ((xs (loop :repeat 50 :collect (normal 10 1))))
    (dolist (d '(0 .1 .2 .3 .4 .5 1 2))
      (format t "~a ~a~%" d (same xs (mapcar (lambda (x) (+ x d)) xs))))))

(defun eg-rank ()
  (labels ((g (mu) (loop :repeat 30 :collect (normal mu 1))))
    (print (rank `((a . ,(g 10)) (b . ,(g 10.1))
                   (c . ,(g 14)) (d . ,(g 14.2)))))))

(defun eg-tree (&aux (tb (data))
                     (y (memo (lambda (r) (disty tb r))))) ; tb frozen
  (show (tree (? tb rows) y (binned tb (? tb rows) y :make #'make-num))))

(defparameter *ens* ; 39 classify sets: num, sym, mixed, big mixed
  '("balance.scale" "blood-transfusion-service-center" "breast.w"
    "diabetes" "ecoli" "glass" "ionosphere" "sonar" "vehicle" "wdbc"
    "vote" "soybean" "tic-tac-toe" "german" "car" "solar.flare2"
    "primary.tumor" "breast.cancer" "spect.test" "audiology"
    "credit.a" "eucalyptus" "ilpd" "heart.c" "hepatitis" "colic" "autos"
    "cylinder.bands" "credit.g" "cmc" "hypothyroid" "sick"
    "Internet-Advertisements" "churn" "COMPAS53" "nomao" "bank-marketing"
    "electricity" "adult"))

(defparameter *treatments*            ; name, then forest's keys
  '(("1:q8" :n 1) ("q8" :rf t)))

(defun eg-ens ()
  "Per *ens* data, 5x2 xval. Prints a #names line, then per fold:
   data|aucs|secs|test row #s, one auc and secs per *treatments*
   forest. Table (plus lightgbm, same folds): python3 lgbm.py FILE"
  (format t "#~{~a ~}~%" (mapcar #'car *treatments*))
  (dolist (f *ens*)
    (let* ((tb (data (format nil "~~/gits/moot/classify/~a.csv" f)))
           (at (? tb cols klass at))
           (y  (lambda (r) (aref r at)))
           (ix (rownums tb)))
      (xval (? tb rows)
            (lambda (train test) (fold f tb y ix train test))))))

(defun rownums (tb &aux (h (make-hash-table :test #'eq)))
  "Hash: row -> its csv row # (rows are stored newest first)"
  (loop :for r :in (reverse (? tb rows)) :for i :from 0
        :do (setf (gethash r h) i))
  h)

(defun fold (f tb y ix train test)
  "One eg-ens line: f|aucs|secs|test row #s, per *treatments* forest"
  (loop :for (nil . keys) :in *treatments*
        :for (a secs) := (multiple-value-list
                          (timed (lambda ()
                                   (aucs (votes (apply #'forest tb train
                                                       y keys)
                                                test y)))))
        :collect a :into as :collect secs :into ss
        :finally (format t "~a|~{~,3f ~}|~{~,3f ~}|~{~a ~}~%" f as ss
                         (mapcar (lambda (r) (gethash r ix)) test))
                 (finish-output)))

(defun eg-all ()
  (dolist (f '(eg-config eg-tbl eg-dist eg-same eg-rank eg-tree))
    (format t "~&~%-- ~(~a~)~%" f) (srand (? *k* seed)) (funcall f)))

;;;; main --------------------------------------------------------------
;;; sbcl --script hair.lisp --leaf=20 tree   (--key=val, then eg-NAMEs)
(defun cli (a &aux (i (position #\= a)))
  "--key=val sets that slot of *k*; NAME runs eg-NAME"
  (if (and i (eql 0 (search "--" a)))
    (let ((k (find-symbol (string-upcase (subseq a 2 i)) :hair)))
      (if (slot-exists-p *k* k)            ; k=nil is fine: no such slot
        (setf (slot-value *k* k) (thing (subseq a (1+ i))))
        (format t "?? ~a~%" a)))
    (aif (find-symbol (string-upcase (format nil "eg-~a" a)) :hair)
      (progn (srand (? *k* seed)) (funcall it))
      (format t "?? ~a~%" a))))
