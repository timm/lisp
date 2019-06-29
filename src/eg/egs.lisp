;; vim: ts=2 sw=2 sts=2  et :
;-------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(got "oo/" "lib/macros.lisp" "eg/col.lisp")

;-------- -------- -------- -------- -------- --------
(defun skip?  (x) (col x #\?))
(defun more?  (x) (col x #\>))
(defun less?  (x) (col x #\<))
(defun klass? (x) (col x #\!))
(defun goal?  (x) (or (klass? x) (less? x) (more? x)))
(defun col (x y) 
  (eql (char (symbol-name x) 0) y))

;-------- -------- -------- -------- -------- --------
(defthing eg keeper (egs) (cells))

(defmethod cell ((e eg) col)
  (aref (? e 'cells) (? col 'pos)))
   
(defkept klass ((e eg))
  (aref
    (? e 'cells)
    (? (klass  (? e 'egs)) 'pos)))

;-------- -------- -------- -------- -------- --------
(defthing egs keeper (name) (cols) (egs))

(defkept mores  ((e egs)) (names e #'more?))
(defkept lesss  ((e egs)) (names e #'less?))
(defkept klasss ((e egs)) (names e #'klass?))
(defkept goals  ((e egs)) (names e #'goal?))
(defkept nums   ((e egs)) (loop for c in (? e 'cols)
                             when #'nump collect c))
(defkept syms   ((e egs)) (loop for c in (? e 'cols) 
                             when #'symp collect c))

(defmethod klass ((e egs)) (car (klasss e)))

(defmethod names ((e egs) fn)
  (remove-if-not 
    #'(lambda (x) (funcall fn (?  x 'name))) (? e 'cols)))

;-------- -------- -------- -------- -------- --------
(defthing holder keeper  
  (has) (name) (pos))

(defmethod add ((h holder) x &key (filter #'identity))
  (with-slots (has) h
    (when (and x (not (eql x #\?)))
      (setf has (or has
                    (make-instance
                      (if (numberb x) 'num 'sum))))
      (add has x :filter filter))))

(defmethod nump ((h holder)) (typep (? h 'has) 'num))
(defmethod symp ((h holder)) (typep (? h 'has) 'sym))

;-------- -------- -------- -------- -------- --------
(defun data (&key name columns egs
             &aux (out 
                    (make-instance 'egs :name name)))
  "Build table for name, col, egs"
  (labels 
    ((ok (lst width) 
         (assert (eql width (length lst)) 
                 (lst) "not of size ~a" width))
     (col+ (pos txt)
         (unless (skip? txt) (col+ val (incf pos))))
           (make-instance 'holder :pos pos :name name))
     (eg+ (cols vals 
                &optional (eg (make-instance 'eg :egs egs)))
          (if (not cols)
            eg
            (let ((val (pop vals))
                  (col (pop cols)))
              (if col
                (add col val))
              (eg+ cols vals)))))
    ;------------------------------------
    (with-slots (cols egs) out
      (let ((pos 0))
        (setf cols
              (mapcar #'(lambda (txt) 
              columns)))
      (dolist (vals egs out)
        (ok lst (length cols))
        (push (eg+ cols vals) egs)))))
