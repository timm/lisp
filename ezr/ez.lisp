;; ez.lisp: incremental Bayes  (c) 2026 Tim Menzies  MIT
(defparameter *the* '(:k 1 :m 2 :p 2 :decs 2 :seed 1))

(defmacro ? (x) `(getf *the* ,(intern (string x) "KEYWORD")))
(set-macro-character #\$
  (lambda (s c) (declare (ignore c)) `(slot-value i ',(read s t nil t))))
(defmacro aif (test then) `(let ((it ,test)) (when it ,then)))

;── structs ──────────────────────────────────────────────────────────
(defstruct col  (at 0) (txt "") goal (n 0d0))
(defstruct (num (:include col)) (mu 0d0) (m2 0d0))
(defstruct (sym (:include col)) (has (make-hash-table :test 'equal)))
(defstruct cols names all x y)
(defstruct data (n 0) rows cols mids)

;── lib ──────────────────────────────────────────────────────────────
(defun unk       (v)     (equal v "?"))
(defun last-char (s)     (char s (1- (length s))))
(defun cell      (c row) (elt row (col-at c)))
(defun say (x)
  (if (floatp x) (format nil "~,vf" (? decs) x) (princ-to-string x)))

(defun cast (s &aux (s (string-trim " " s)))
  (cond ((string-equal s "true")  1)
        ((string-equal s "false") 0)
        (t (let ((n (read-from-string s nil nil)))
             (if (numberp n) n s)))))

(defun atoms (ch str &aux (b (position ch str)))
  (cons (cast (string-trim " " (subseq str 0 (or b (length str)))))
          (when b (atoms ch (subseq str (1+ b))))))

(defun mapcsv (&optional (fun #'print) file end)
  (with-open-file (s (or file *standard-input*))
      (loop (funcall fun (atoms (or (read-line s nil) (return end)))))))

(defun align (m &aux (ss (mapcar (lambda (r) (mapcar #'say r)) m)))
  (let ((ws (loop for i below (length (car ss))
                  collect (loop for r in ss maximize (length (nth i r))))))
    (dolist (r ss)
      (format t "~{~a~^, ~}~%"
              (mapcar (lambda (v w) (format nil "~v@a" w v)) r ws)))))

(defun eachn (lst &optional (n 30))
  (loop for x in lst for i from 0 when (zerop (mod i n)) collect x))

;── structs ──────────────────────────────────────────────────────────
(defun col! (at txt &aux (g (char/= (last-char txt) #\-)))
  (if (upper-case-p (char txt 0)) (make-num :at at :txt txt :goal g)
                                  (make-sym :at at :txt txt :goal g)))

(defun cols! (names &aux (at -1))
  (labels ((end   (c) (last-char (col-txt c)))
           (make! (s) (col! (incf at) s)))
    (let ((all (mapcar #'make! names)))
      (make-cols :names names :all all
        :x (remove-if     (lambda (c) (find (end c) "-+!X")) all)
        :y (remove-if-not (lambda (c) (find (end c) "-+!"))  all)))))

(defun data! (file &aux (d (make-data)))
  (csv file (lambda (r) (add d r))) d)

;── update ───────────────────────────────────────────────────────────
(defmethod add ((i sym) v &optional (w 1d0))
  (unless (unk v) (incf $n w) (incf (gethash v $has 0d0) w)))

(defmethod add ((i num) v &optional (w 1d0))
  (unless (unk v)
    (incf $n w)
    (if (<= $n 0) (setf $mu 0d0 $m2 0d0)
        (let ((d (- v $mu)))
          (incf $mu (* w (/ d $n))) (incf $m2 (* w d (- v $mu)))))))  )

(defmethod add ((i data) row &optional (w 1d0))
  (if $cols (addRow i row w) (setf $cols (cols! row))) row)

(defun addRow (i row w)
  (setf $mids nil $n (+ $n w))
  (dolist (c (cols-all $cols)) (add c (cell c row) w))
  (if (plusp w) (push row $rows)
      (setf $rows (remove row $rows :test #'equal :count 1))))

(defun sub (i v) (add i v -1d0))

;── query ────────────────────────────────────────────────────────────
(defmethod mid ((i num)) $mu)

(defmethod mid ((i sym) &aux bk (bv -1))
  (maphash (lambda (k v) (when (> v bv) (setf bk k bv v))) $has)
  bk)

(defun mids (i) (or $mids (setf $mids (mapcar #'mid (cols-all $cols)))))

(defmethod spread ((i num))
  (if (< $n 2) 0d0 (sqrt (/ (max 0d0 $m2) (1- $n)))))

(defmethod spread ((i sym) &aux (e 0d0))
  (maphash (lambda (_ v)
    (when (plusp v) (let ((p (/ v $n))) (decf e (* p (log p 2)))))) $has)
  e)

(defun z (i v) (max -3d0 (min 3d0 (/ (- v $mu) (+ (spread i) 1d-30)))))

(defmethod norm ((i sym) v) v)
(defmethod norm ((i num) v)
  (if (unk v) v (/ 1d0 (+ 1d0 (exp (* -1.7d0 (z i v)))))))

;── distance ─────────────────────────────────────────────────────────
(defun minkowski (vals &aux (n 0) (s 0d0) (p (? p)))
  (dolist (x vals (if (zerop n) 0d0 (expt (/ s n) (/ 1d0 p))))
    (incf n) (incf s (expt x p))))

(defun disty (i r)
  (minkowski (mapcar (lambda (y &aux (v (norm y (cell y r))))
                       (- (if (unk v) 0d0 v) (if (col-goal y) 1d0 0d0)))
                     (cols-y $cols))))

(defun distx (i r1 r2)
  (labels ((aha (i u v)
             (cond ((and (unk u) (unk v)) 1d0)
                   ((sym-p i) (if (equal u v) 0d0 1d0))
                   (t (let ((nu (norm i u)) (nv (norm i v)))
                        (when (unk nu) (setf nu (if (> nv 0.5) 0d0 1d0)))
                        (when (unk nv) (setf nv (if (> nu 0.5) 0d0 1d0)))
                        (abs (- nu nv)))))))
    (minkowski (mapcar (lambda (x) (aha x (cell x r1) (cell x r2)))
                       (cols-x $cols)))))

(defun nearest  (d r rows) (car      (order d r rows)))
(defun furthest (d r rows) (car (last (order d r rows))))
(defun order (d r rows)
  (sort (copy-list rows) #'< :key (lambda (r2) (distx d r r2))))

(defmethod nearby ((i sym) v &aux
    (n (* (random 1d0) (loop for v being the hash-values of $has sum v))))
  (maphash (lambda (k v) (when (<= (decf n v) 0) (return-from nearby k)))
           $has))

(defmethod nearby ((i num) v)
  (+ (if (unk v) $mu v)
     (* 2 (spread i) (- (loop repeat 3 sum (random 1d0)) 1.5d0))))

;── bayes ────────────────────────────────────────────────────────────
(defmethod like ((i num) v prior &aux (var (+ (expt (spread i) 2) 1d-30)))
  (* (/ 1d0 (sqrt (* 2 pi var)))
     (exp (/ (- (expt (- v $mu) 2)) (* 2 var)))))

(defmethod like ((i sym) v prior)
  (max 1d-32 (/ (+ (gethash v $has 0d0) (* (? k) prior)) (+ $n (? k)))))

(defun prior (i n-all n-h) (/ (+ $n (? m)) (+ n-all (* (? m) n-h))))

(defun likes (i row n-all n-h &aux (p (prior i n-all n-h)))
  (+ (log p)
     (loop for x in (cols-x $cols) for v = (cell x row)
           unless (unk v) sum (let ((l (like x v p)))
                                (if (plusp l) (log l) 0d0)))))

;── demos ────────────────────────────────────────────────────────────
(defun eg_the (&optional f) (declare (ignore f)) (format t "~a~%" *the*))

(defun eg_csv (f &aux out)
  (csv f (lambda (r) (push r out)))
  (align (eachn (nreverse out))))

(defun eg_data (f &aux (d (data! f)))
  (align (list* (cols-names (data-cols d)) (mids d) (eachn (data-rows d)))))

(defun eg_disty (f &aux (d (data! f)))
  (align (cons (cols-names (data-cols d))
               (eachn (sort (copy-list (data-rows d)) #'<
                            :key (lambda (r) (disty d r)))))))

(defun eg_addsub (f &aux (d (data! f))
                         (rows (copy-list (data-rows d))) one two)
  (dolist (r (reverse rows))
    (sub d r) (when (= (data-n d) 50) (setf one (mids d))))
  (dolist (r rows)
    (add d r) (when (= (data-n d) 50) (setf two (mids d))))
  (assert (every (lambda (a b) (or (not (numberp a)) (< (abs (- a b)) 1e-5)))
                 one two))
  (format t "ok~%"))

(defun eg_bayes (f &aux (d (data! f)) (n (data-n d)))
  (dolist (r (eachn (data-rows d)))
    (format t "~a~%" (say (/ (round (* (likes d r n 1) 100d0)) 100d0)))))

;── main ─────────────────────────────────────────────────────────────
(defun cli (args)
  (loop while args for k = (string-trim "-" (pop args))
        for sym = (find-symbol (format nil "EG_~:@(~a~)" k))
        when sym do (funcall sym (when args (cast (pop args))))))

(cli #+sbcl  (cdr sb-ext:*posix-argv*)
     #+clisp (rest ext:*args*))
