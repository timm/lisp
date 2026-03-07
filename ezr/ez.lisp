;; ez.lisp: incremental Bayes (c) 2026 Tim Menzies MIT
; vim: set et ts=2 sw=2 lisp:
#+sxbcl (setf sb-ext:*invoke-debugger-hook*
             (lambda (c h) (declare (ignore h))
               (format *error-output* "~a~%" c) (sb-ext:exit :code 1)))

(defparameter *the*
  '(:neighbors 1 :min 2 :power 2 :decimals 2 :seed 1))

(defmacro ? (x) `(getf *the* ,(intern (string x) "KEYWORD")))

(defmacro dohash ((k v hash &optional result) &body body)
  `(progn (maphash (lambda (,k ,v) ,@body) ,hash) ,result))

(defmacro collect ((v lst) &body body) 
  `(mapcar (lambda (,v) ,@body) ,lst))

(defmacro sortby (var lst &body body)
  `(sort ,lst #'< :key (lambda (,var) ,@body)))

(set-macro-character #\$ (lambda (s c) (declare (ignore c))
                           `(slot-value i ',(read s t nil t))))

;; structs ---
(defstruct col  (n 0) (at 0) (txt ""))
(defstruct (num (:include col)) (mu 0) (m2 0) (goal 0))
(defstruct (sym (:include col)) (has (make-hash-table :test 'equal)))
(defstruct data (n 0) rows cols mids)
(defstruct cols names all x y)

;; create ---
(defun col! (&optional (at 0) (txt " "))
  (if (upper-case-p (char txt 0))
    (make-num :at at :txt txt :goal (if (char= (end txt) #\-) 0 1))
    (make-sym :at at :txt txt)))

(defun cols! (names)
  (let ((all (loop for s in names for at from 0 collect (col! at s))))
    (make-cols :names names :all all
      :x (remove-if (lambda (c) (find (end c) "-+!X")) all)
      :y (remove-if-not (lambda (c) (find (end c) "-+!")) all))))

(defun data! (src &aux (data (make-data)))
  (funcall (if (stringp src) #'mapcsv #'mapc)
           (lambda (x) (add data x)) src)
  data)

;; update ---
(defun unk (v) (equal v "?"))
(defun cell (c row) (elt row (col-at c)))

(defun sub (i v) (add i v -1))

(defmethod add (i x &optional (w 1))
  (unless (unk x) (add1 i x w)) x)

(defmethod add1 ((i sym) x w)
  (incf $n w)
  (incf (gethash x $has 0) w))

(defmethod add1 ((i num) x w &aux (d (- x $mu)))
  (incf $n w)
  (cond ((and (< w 0) (< $n 1)) (setf $n 0 $mu 0 $m2 0))
        (t (incf $mu (/ (* w d) $n))
           (incf $m2 (* w d (- x $mu))))))

(defmethod add1 ((i data) row w)
  (if $cols (add1Row2Data i row w) 
    (setf $cols (cols! row))))

(defun add1Row2Data (i row w)
  (incf $n w)
  (setf $mids nil)
  (dolist (c (cols-all $cols)) (add c (cell c row) w))
  (if (> w 0) (push row $rows)
    (setf $rows
      (remove row $rows :test #'equal :count 1))))

;; query ---
(defmethod mid ((i num)) (float $mu))

(defmethod mid ((i sym) &aux m (mx 0))
  (dohash (k v $has m)
    (if (> v mx) (setf m k mx v))))

(defun mids (i)
  (or $mids (setf $mids (collect (c (cols-all $cols)) (mid c)))))

(defmethod spread ((i num))
  (if (< $n 2) 0 (sqrt (/ (max 0 $m2) (1- $n)))))

(defmethod spread ((i sym) &aux (e 0))
  (dohash (k v $has e)
    (let ((p (/ v $n))) (decf e (* p (log p 2))))))

(defun z (i v)
  (max -3 (min 3
    (/ (- v $mu) (+ (spread i) 1e-30)))))

(defmethod norm ((i sym) v) v)
(defmethod norm ((i num) v)
  (if (unk v) v
    (/ 1 (+ 1 (exp (* -1.7 (z i v)))))))

;; distance ---
(defun minkowski (vals &aux (n 0) (s 0))
  (dolist (x vals (expt (/ s n) (/ 1 (? power))))
    (incf n) (incf s (expt x (? power)))))

(defun disty (i r)
  (minkowski (collect (y (cols-y $cols))
    (abs (- (norm y (cell y r)) (num-goal y))))))

(defmethod distx ((i data) r1 r2)
  (minkowski (collect (x (cols-x $cols))
    (distx x (cell x r1) (cell x r2)))))

(defmethod distx ((c sym) u v)
  (if (and (unk u) (unk v)) 1
      (if (equal u v) 0 1)))

(defmethod distx ((c num) u v)
  (if (and (unk u) (unk v)) 1
    (let ((nu (norm c u)) (nv (norm c v)))
      (if (unk nu) (setf nu (if (> nv 0.5) 0 1)))
      (if (unk nv) (setf nv (if (> nu 0.5) 0 1)))
      (abs (- nu nv)))))

(defun orderx (d r rows) (sortby r2 rows (distx d r r2)))
(defun nearest (d r rows) (car (orderx d r rows)))
(defun furthest (d r rows) (car (last (orderx d r rows))))

;; stats ---
(defmethod pick ((i sym) &optional _)
  (let ((r (* (random 1.0) $n)))
    (dohash (k v $has)
            (if (<= (decf r v) 0) (return-from pick k)))))

(defmethod pick ((i num) &optional v)
  (+ (if (or (null v) (unk v)) $mu v)
     (* 2 (spread i)
       (- (loop repeat 3 sum (random 1.0)) 1.5))))

(defmethod like ((i num) v prior
  &aux (var (+ (expt (spread i) 2) 1e-30)))
  (* (/ 1 (sqrt (* 2 pi var)))
     (exp (/ (- (expt (- v $mu) 2)) (* 2 var)))))

(defmethod like ((i sym) v prior)
  (max 1e-32 (/ (+ (gethash v $has 0) (* (? neighbors) prior))
                (+ $n (? neighbors)))))

(defun prior (i n-all n-h)
  (/ (+ $n (? min)) (+ n-all (* (? min) n-h))))

(defun likes (i row n-all n-h &aux (p (prior i n-all n-h)))
  (+ (log p)
     (loop for c in (cols-x $cols)
       for v = (cell c row)
       for l = (if (unk v) 0 (like c v p))
       if (plusp l) sum (log l))))

;; lib ---
(defmethod say (x)
  (if (floatp x) (format nil "~,vf" (? decimals) x)
      (princ-to-string x)))

(defun convert (s &aux (s (string-trim " " s)) (*read-eval* nil)
                    (n (read-from-string s nil nil)))
  (case n (true 1) (false 0) (t (if (numberp n) n s))))

(defun atoms (ch str &aux (b (position ch str)))
  (cons (convert (subseq str 0 (or b (length str))))
        (if b (atoms ch (subseq str (1+ b))))))

(defun mapcsv (fn file &optional end &key (sep #\,))
  (with-open-file (s (or file *standard-input*))
    (loop (funcall fn (atoms sep
            (or (read-line s nil) (return end)))))))

(defun align (m)
  (let* ((ss (collect (r m) (collect (x r) (say x))))
         (ws (loop for i below (length (car ss))
               collect (loop for r in ss
                         maximize (length (nth i r))))))
    (dolist (r ss)
      (format t "~{~a~^, ~}~%"
              (collect (v r) (format nil "~v@a"
                                     (nth (position v r) ws) v))))))

(defun eachn (lst &optional (n 30))
  (loop for x in lst for i from 0 if (zerop (mod i n)) collect x))

(defun end (x &aux (s (if (stringp x) x (col-txt x))))
  (char s (1- (length s))))

(defun rseed (&optional (seed 1))
  #+sbcl (setf *random-state* (sb-ext:seed-random-state (? seed)))
  #+clisp (setf *random-state* (make-random-state (? seed))))

;; demos ---
(defun eg_all (&optional f) 
  (eg_the)
  (dolist (fn '(eg_csv eg_data eg_disty eg_abdsub eg_bayes))
    (funcall fn f)))

(defun eg_the (&optional f) f
  (format t "~a~%" *the*))

(defun eg_rand (&optional f) f
  (loop repeat 2 do
    (rseed (? seed))
    (print (loop repeat 6 collect (random 1.0)))))

(defun eg_csv (f &aux out)
  (mapcsv (lambda (r) (push r out)) f)
  (align (eachn (nreverse out))))

(defun eg_data (f &aux (d (data! f)))
  (align (list* (cols-names (data-cols d))
                (mids d) (eachn (data-rows d)))))

(defun eg_disty (f &aux (d (data! f)))
  (align (cons (cols-names (data-cols d))
    (eachn (sortby r (data-rows d) (disty d r))))))

(defun eg_addsub (f &aux (d (data! f))
                    (rows (copy-list (data-rows d))) one two)
  (dolist (r (reverse rows))
    (sub d r)
    (if (= (data-n d) 50) (setf one (mids d))))
  (dolist (r rows)
    (add d r)
    (if (= (data-n d) 50) (setf two (mids d))))
  (assert (every (lambda (a b)
             (or (not (numberp a))
                 (< (abs (- a b)) 0.01))) one two)))

(defun eg_bayes (f &aux (d (data! f)) (n (data-n d)))
  (print (sort (collect (r (eachn (data-rows d)))
                        (likes d r n 1)) #'<)))

;; main ---
(defun cli (args)
  (loop for (k v) on args by #'cddr do
    (let* ((k    (string-trim "-" k))
           (fn   (intern (string-upcase (format nil "EG_~a" k))))
           (flag (intern (string-upcase k) "KEYWORD")))
      (cond ((fboundp fn) (funcall fn (convert v)))
            ((member flag *the*) (setf (getf *the* flag) (convert v)))))))

(cli #+sbcl (cdr sb-ext:*posix-argv*)
     #+clisp (rest ext:*args*))
