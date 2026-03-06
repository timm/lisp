;; ez.lisp: incremental Bayes  (c) 2026 Tim Menzies  MIT
(defparameter *the* '(:k 1 :m 2 :p 2 :decs 2 :seed 1))

(defmacro ? (x) `(getf *the* ,(intern (string x) "KEYWORD")))

(defmacro o (x &rest slots) 
   (if (null slots) x `(o (slot-value ,x ',(car slots)) ,@(cdr slots))))

(set-macro-character #\$
  (lambda (s c) (declare (ignore c)) `(slot-value i ',(read s t nil t))))

;── structs ──────────────────────────────────────────────────────────
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
  (cons (cast (subseq str 0 (or b (length str))))
        (if b (atoms ch (subseq str (1+ b))))))

(defun mapcsv (fun file &optional end &key (sep #\,))
  (with-open-file (s (or file *standard-input*))
      (loop (funcall fun (atoms sep (or (read-line s nil) (return end)))))))

(defun align (m &aux (ss (mapcar (lambda (r) (mapcar #'say r)) m)))
  (let ((ws (loop for i below (length (car ss))
                  collect (loop for r in ss maximize (length (nth i r))))))
    (dolist (r ss)
      (format t "~{~a~^, ~}~%"
              (mapcar (lambda (v w) (format nil "~v@a" w v)) r ws)))))

(defun eachn (lst &optional (n 30))
  (loop for x in lst for i from 0 when (zerop (mod i n)) collect x))

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
  (maphash (lambda (k v) (when (> v bv) (setf bk k bv v))) $has)
  bk)

(defun mids (i) (or $mids (setf $mids (mapcar #'mid (cols-all $cols)))))

(defmethod spread ((i num)) (if (< $n 2) 0 (sqrt (/ (max 0 $m2) (1- $n)))))

(defmethod spread ((i sym) &aux (e 0))
  (maphash (lambda (_ v)
    (if (> v 0) (let ((p (/ v $n))) (decf e (* p (log p 2)))))) $has)
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

(defun nearest  (d r rows) (car      (order d r rows)))
(defun furthest (d r rows) (car (last (order d r rows))))
(defun order (d r rows)
  (sort (copy-list rows) #'< :key (lambda (r2) (distx d r r2))))

(defmethod nearby ((i sym) v &aux
    (n (* (random 1) (loop for v being the hash-values of $has sum v))))
  (maphash (lambda (k v) (when (<= (decf n v) 0) (return-from nearby k)))
           $has))

(defmethod nearby ((i num) v)
  (+ (if (unk v) $mu v)
     (* 2 (spread i) (- (loop repeat 3 sum (random 1)) 1.5))))

;── bayes ────────────────────────────────────────────────────────────
(defmethod like ((i num) v prior &aux (var (+ (expt (spread i) 2) 1e-30)))
  (* (/ 1 (sqrt (* 2 pi var)))
     (exp (/ (- (expt (- v $mu) 2)) (* 2 var)))))

(defmethod like ((i sym) v prior)
  (max 1e-32 (/ (+ (gethash v $has 0) (* (? k) prior)) (+ $n (? k)))))

(defun prior (i n-all n-h) (/ (+ $n (? m)) (+ n-all (* (? m) n-h))))

(defun likes (i row n-all n-h &aux (p (prior i n-all n-h)))
  (+ (log p)
     (loop for x in (cols-x $cols) for v = (cell x row)
           unless (unk v) sum (let ((l (like x v p)))
                                (if (plusp l) (log l) 0)))))

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
    (format t "~a~%" (say (/ (round (* (likes d r n 1) 100)) 100)))))

;── main ─────────────────────────────────────────────────────────────
(defun cli (args)
  (loop while args for k = (string-trim "-" (pop args))
        for sym = (find-symbol (format nil "EG_~:@(~a~)" k))
        when sym do (funcall sym (when args (cast (pop args))))))

(cli #+sbcl  (cdr sb-ext:*posix-argv*)
     #+clisp (rest ext:*args*))
