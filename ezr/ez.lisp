i;; ez.lisp: incremental Bayes  (c) 2026 Tim Menzies  MIT
(defparameter *the* '(:k 1 :m 2 :p 2 :decs 2 :seed 1))

<<<<<<< HEAD
(defmacro ? (x) `(getf *the* ,(intern (string x) "KEYWORD")))

(defmacro o (x &rest slots) 
   (if (null slots) x `(o (slot-value ,x ',(car slots)) ,@(cdr slots))))

=======
(defmacro ?  (x)                           `(getf *the* ,(intern (string x) "KEYWORD")))
(defmacro sq (x)                           `(* ,x ,x))
(defmacro ht ()                            '(make-hash-table :test 'equal))
(defmacro aif (test then &optional else)   `(let ((it ,test)) (if it ,then ,else)))
(defmacro with-val ((v w) &body body)      `(unless (unk ,v) (incf $n ,w) ,@body))
(defmacro safe-log (x)                     `(let ((it ,x)) (if (plusp it) (log it) 0d0)))
(defmacro collect ((var lst) &body body)   `(mapcar (lambda (,var) ,@body) ,lst))
(defmacro dohash ((k v tbl) &body body)    `(maphash (lambda (,k ,v) ,@body) ,tbl))
(defmacro sortby (lst &body key)           `(sort (copy-list ,lst) #'< :key (lambda (r) ,@key)))
>>>>>>> c487a54e127337546c9f7652c2b57724555a63d8
(set-macro-character #\$
  (lambda (s c) (declare (ignore c)) `(slot-value i ',(read s t nil t))))

;── structs ──────────────────────────────────────────────────────────
<<<<<<< HEAD
=======
(defstruct col  (at 0) (txt "") goal (n 0d0))
(defstruct (num (:include col)) (mu 0d0) (m2 0d0))
(defstruct (sym (:include col)) (has (ht)))
(defstruct cols names all x y)
>>>>>>> c487a54e127337546c9f7652c2b57724555a63d8
(defstruct data (n 0) rows cols mids)
(defstruct num  (n 0) (at 0) (txt "") (mu 0) (m2 0) (goal 0))
(defstruct sym  (n 0) (at 0) (txt "") (has (make-hash-table :test 'equal)))
(defstruct cols names all x y)

;── lib ──────────────────────────────────────────────────────────────
(defun unk  (v) (equal v "?"))
(defun cell (c row) (elt row (col-at c)))
(defmethod say (x) 
  (if (floatp x) (format nil "~,vf" (? decs) x) (princ-to-string x)))

(defun cast (s &aux (s (string-trim " " s)) (*read-eval* nil)) 
  (labels ((fn (s n) (case n
                       (true  1)
                       (false 0)
                       (t (if (numberp n) n s)))))
    (fn s (read-from-string s nil nil))))

(defun atoms (ch str &aux (b (position ch str)))
<<<<<<< HEAD
  (cons (cast (subseq str 0 (or b (length str))))
        (if b (atoms ch (subseq str (1+ b))))))
=======
  (cons (cast (string-trim " " (subseq str 0 (or b (length str)))))
        (when b (atoms ch (subseq str (1+ b))))))
>>>>>>> c487a54e127337546c9f7652c2b57724555a63d8

(defun mapcsv (fun file &optional end &key (sep #\,))
  (with-open-file (s (or file *standard-input*))
<<<<<<< HEAD
      (loop (funcall fun (atoms sep (or (read-line s nil) (return end)))))))
=======
    (loop (funcall fun (atoms #\, (or (read-line s nil) (return end)))))))
>>>>>>> c487a54e127337546c9f7652c2b57724555a63d8

(defun align (m &aux (ss (collect (r m) (collect (x r) (say x)))))
  (let ((ws (loop for i below (length (car ss))
                  collect (loop for r in ss maximize (length (nth i r))))))
    (dolist (r ss)
      (format t "~{~a~^, ~}~%"
              (collect (v r) (format nil "~v@a" (nth (position v r) ws) v))))))

(defun eachn (lst &optional (n 30))
  (loop for x in lst for i from 0 when (zerop (mod i n)) collect x))

<<<<<<< HEAD
(defun end (x &aux (s (if (stringp x) x (col-txt x))))
  (char s (1- (length s))))

;── structs ──────────────────────────────────────────────────────────
(defun col! (&optional (at 0) (txt " "))
  (if (upper-case-p (char txt 0))
    (make-num :at at :txt txt :goal (if (char= (end txt) #\-) 0 1))
    (make-sym :at at :txt txt)))

(defun cols! (names)
  (let ((all (loop for s in names for at from 0 collect (col! at s))))
    (make-cols :names names :all all
               :x (remove-if     (lambda (c) (find (end c) "-+!X")) all)
               :y (remove-if-not (lambda (c) (find (end c) "-+!"))  all))))

(defun data! (src &aux (data (make-data)))
  (let ((fn (if (stringp src) #'mapcsv #'mapc)))
    (funcall fn (lambda (row) add src) src)
    data))

;── update ───────────────────────────────────────────────────────────
(defun sub (i v) (add i v -1))

(defmethod add (i x &optional (w 1))
  (unless (equal x "?")
    (incf $n w)
    (add1 i x w))
  x)
=======
;── cols ─────────────────────────────────────────────────────────────
(defun col! (at txt &aux (g (char/= (last-char txt) #\-)))
  (if (upper-case-p (char txt 0)) (make-num :at at :txt txt :goal g)
                                  (make-sym :at at :txt txt :goal g)))

(defun cols! (names &aux (at -1))
  (labels ((end   (c) (last-char (col-txt c)))
           (make! (s) (col! (incf at) s)))
    (let ((all (collect (s names) (make! s))))
      (make-cols :names names :all all
        :x (remove-if     (lambda (c) (find (end c) "-+!X")) all)
        :y (remove-if-not (lambda (c) (find (end c) "-+!"))  all)))))

(defun data! (file &aux (d (make-data)))
  (mapcsv (lambda (r) (add d r)) file) d)

;── update ───────────────────────────────────────────────────────────
(defmethod add ((i sym) v &optional (w 1d0))
  (with-val (v w) (incf (gethash v $has 0d0) w)))

(defmethod add ((i num) v &optional (w 1d0))
  (with-val (v w)
    (if (<= $n 0) (setf $mu 0d0 $m2 0d0)
        (let ((d (- v $mu)))
          (incf $mu (* w (/ d $n))) (incf $m2 (* w d (- v $mu)))))))
>>>>>>> c487a54e127337546c9f7652c2b57724555a63d8

(defmethod add1 ((i sym) x w)
  (incf (gethash x $has 0) w))

(defmethod add1 ((i num) x w &aux (d (- x $mu)))
  (incf $mu (/ (* w d) $n))
  (incf $m2 (* w d (- x $mu))))

(defmethod add1 ((i data) row w)
  (if $cols (add1Row2Data i row w) (setf $cols (cols! row))))
  
(defun add1Row2Data (i row w)
  (setf $mids nil)
  (dolist (c (cols-all $cols)) (add c (cell c row) w))
  (if (> w 0)
      (push row $rows)
      (setf $rows (remove row $rows :test #'equal :count 1))))

;── query ────────────────────────────────────────────────────────────
(defmethod mid ((i num)) $mu)

(defmethod mid ((i sym) &aux bk (bv -1))
  (dohash (k v $has) (when (> v bv) (setf bk k bv v)))
  bk)

(defun mids (i) (or $mids (setf $mids (collect (c (cols-all $cols)) (mid c)))))

(defmethod spread ((i num)) (if (< $n 2) 0 (sqrt (/ (max 0 $m2) (1- $n)))))

<<<<<<< HEAD
(defmethod spread ((i sym) &aux (e 0))
  (maphash (lambda (_ v)
    (if (> v 0) (let ((p (/ v $n))) (decf e (* p (log p 2)))))) $has)
=======
(defmethod spread ((i sym) &aux (e 0d0))
  (dohash (_ v $has) (when (plusp v) (let ((p (/ v $n))) (decf e (* p (log p 2))))))
>>>>>>> c487a54e127337546c9f7652c2b57724555a63d8
  e)

(defun z (i v) (max -3 (min 3 (/ (- v $mu) (+ (spread i) 1e-30)))))

(defmethod norm ((i sym) v) v)
(defmethod norm ((i num) v)
  (if (unk v) v (/ 1 (+ 1 (exp (* -1.7 (z i v)))))))

;── distance ─────────────────────────────────────────────────────────
(defun minkowski (vals &aux (n 0) (s 0) (p (? p)))
  (dolist (x vals (if (zerop n) 0 (expt (/ s n) (/ 1 p))))
    (incf n) (incf s (expt x p))))

(defun disty (i r)
<<<<<<< HEAD
  (labels ((fn (y) (abs (- (norm y (cell y r)) (o y goal)))))
    (minkowski (mapcar #'fn (o i cols y)))))

(defun distx (i r1 r2)
  (labels ((aha (c) (let ((u (cell c r1))
                          (v (cell c r2)))
                      (cond ((and (unk u) (unk v)) 1)
                            ((sym-p c) (if (equal u v) 0 1))
                            (t (let ((nu (norm c u)) (nv (norm c v)))
                                 (if (unk nu) (setf nu (if (> nv 0.5) 0 1)))
                                 (if (unk nv) (setf nv (if (> nu 0.5) 0 1)))
                                 (abs (- nu nv))))))))
    (minkowski (mapcar #'aha (o i cols x)))))
=======
  (minkowski (collect (y (cols-y $cols))
               (let ((v (norm y (cell y r))))
                 (- (if (unk v) 0d0 v) (if (col-goal y) 1d0 0d0))))))

(defun unk-resolve (u v) (if (unk u) (if (> v 0.5) 0d0 1d0) u))

(defun distx (i r1 r2)
  (labels ((aha (i u v)
             (cond ((and (unk u) (unk v)) 1d0)
                   ((sym-p i) (if (equal u v) 0d0 1d0))
                   (t (let ((nu (norm i u)) (nv (norm i v)))
                        (setf nu (unk-resolve nu nv) nv (unk-resolve nv nu))
                        (abs (- nu nv)))))))
    (minkowski (collect (x (cols-x $cols)) (aha x (cell x r1) (cell x r2))))))
>>>>>>> c487a54e127337546c9f7652c2b57724555a63d8

(defun nearest  (d r rows) (car      (order d r rows)))
(defun furthest (d r rows) (car (last (order d r rows))))
(defun order    (d r rows) (sortby rows (distx d r r2)))

(defmethod nearby ((i sym) v &aux
<<<<<<< HEAD
    (n (* (random 1) (loop for v being the hash-values of $has sum v))))
  (maphash (lambda (k v) (when (<= (decf n v) 0) (return-from nearby k)))
           $has))
=======
    (n (* (random 1d0) (loop for v being the hash-values of $has sum v))))
  (dohash (k v $has) (when (<= (decf n v) 0) (return-from nearby k))))
>>>>>>> c487a54e127337546c9f7652c2b57724555a63d8

(defmethod nearby ((i num) v)
  (+ (if (unk v) $mu v)
     (* 2 (spread i) (- (loop repeat 3 sum (random 1)) 1.5))))

;── bayes ────────────────────────────────────────────────────────────
<<<<<<< HEAD
(defmethod like ((i num) v prior &aux (var (+ (expt (spread i) 2) 1e-30)))
  (* (/ 1 (sqrt (* 2 pi var)))
     (exp (/ (- (expt (- v $mu) 2)) (* 2 var)))))
=======
(defmethod like ((i num) v prior &aux (var (+ (sq (spread i)) 1d-30)))
  (* (/ 1d0 (sqrt (* 2 pi var)))
     (exp (/ (- (sq (- v $mu))) (* 2 var)))))
>>>>>>> c487a54e127337546c9f7652c2b57724555a63d8

(defmethod like ((i sym) v prior)
  (max 1e-32 (/ (+ (gethash v $has 0) (* (? k) prior)) (+ $n (? k)))))

(defun prior (i n-all n-h) (/ (+ $n (? m)) (+ n-all (* (? m) n-h))))

(defun likes (i row n-all n-h &aux (p (prior i n-all n-h)))
  (+ (log p)
     (loop for x in (cols-x $cols) for v = (cell x row)
<<<<<<< HEAD
           unless (unk v) sum (let ((l (like x v p)))
                                (if (plusp l) (log l) 0)))))
=======
           unless (unk v) sum (safe-log (like x v p)))))
>>>>>>> c487a54e127337546c9f7652c2b57724555a63d8

;── demos ────────────────────────────────────────────────────────────
(defun eg_the (&optional f) (declare (ignore f)) (format t "~a~%" *the*))

(defun eg_csv (f &aux out)
  (mapcsv (lambda (r) (push r out)) f)
  (align (eachn (nreverse out))))

(defun eg_data (f &aux (d (data! f)))
  (align (list* (cols-names (data-cols d)) (mids d) (eachn (data-rows d)))))

(defun eg_disty (f &aux (d (data! f)))
  (align (cons (cols-names (data-cols d))
               (eachn (sortby (data-rows d) (disty d r))))))

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
    (format t "~a~%" (say (/ (round (* (likes d r n 1) 100)) 100)))))

;── main ─────────────────────────────────────────────────────────────
(defun cli (args)
  (loop while args for k = (string-trim "-" (pop args))
        for sym = (find-symbol (format nil "EG_~:@(~a~)" k))
        when sym do (funcall sym (when args (cast (pop args))))))

(cli #+sbcl  (cdr sb-ext:*posix-argv*)
     #+clisp (rest ext:*args*))
