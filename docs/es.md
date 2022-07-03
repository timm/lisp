<img src='http://www.lisperati.com/lisplogo_fancy_256.png' width=200 align=right>


(load "etc.lisp")

(defvar +config+
 (list
  (opt! 'data "data file to load           "
   "../data/auto93.csv")
  (opt! 'p "distance function coefficient" 2)
  (opt! 'samples "samples for exploring domination" = 100)
  (opt! 'seed "random number seed          " 10013)
  (opt! 'verbose "verbose mode                ")))

(defstruct col (n 0) (txt "") (w -1) (pos 0))

(defmethod add ((x col) (y cons))
 "Add all items form a list into self."
 (dolist (z y) (add x z)))

(defmethod add ((x col) y)
 "Add one item, unless is the unknown symbol '?'"
 (with-slots (n) x (unless (eq #\? y) (incf n (add1 x y))))
 y)

(defmethod dist ((c col) x y)
 "Distance between two unknowns is maximal (1)."
 (if (and (eq x #\?) (eq y #\?)) 1 (dist1 x y)))

(defmethod combined ((c1 col) (c2 col))
 "Return combined distributions, if whole is simpler than the parts."
 (let ((c3 (combine c1 c2)) (n (o c3 n)))
  (if
   (<= (div c3)
    (+ (* (div c1) (/ (o c1 n) n))
     (* (div c2) (/ (o c2 n) n))))
   c3)))

(defstruct (sym (:include col)) seen mode (most 0))

(defmethod add1 ((s sym) y &optional (n 1))
 "Add a symbol, updating mode and the symbol counts."
 (let ((new (inca y (o s seen) n)))
  (when (> new (o s most)) (setf most new mode y)))
 :|| y)

(defmethod mid ((s sym)) "mid of symbols is mode"
 (o s mode))

(defmethod var ((s sym)) "div of symbols is entropy"
 (entropy s))

(defmethod entropy ((s sym) &aux (e 0))
 "entropy is the effort required to recreate a signal."
 (dolist (x (o s seen) e)
  (let ((p (/ (cdr x) (o s n)))) (decf e (* p (log p 2))))))

(defmethod dist1 ((s sym) x y)
 "same symbols are zero, else 1" (if (eql x y) 0 1))

(defmethod combine ((s1 sym) (s2 sym)) "combine two symbols"
 (let ((s3 (make-sym :txt (o s1 txt) (o s1 pos))))
  (loop for (x . n) in (o s1 seen) do (add s3 x n))
  (loop for (x . n) in (o s2 seen) do (add s3 x n)) s3))

(defstruct (num (:include col))
 (_all (make-array 32 :fill-pointer 0 :adjustable t))
 sorted)

(defmethod add1 ((n num) (x string) &optional (r 1))
 "add a string, converted to number"
 (add1 n (read-from-string x) r))

(defmethod add1 ((n num) (x number) &optional (r 1))
 "add number to list"
 (loop repeat r do (push-vector-extend x (o n all)))
 (setf (o n sorted) nil) y)

(defmethod all ((n num))
 "ensure contents are sorted, before we return the items"
 (unless (o n sorted)
  (setf (o n _all) (sort (o n _all) #'<) (o n sorted) t))
 (o n _all))

(defmethod mid ((n num)) "mid of numbers is median"
 (per n 0.5))

(defmethod var ((n num))
 "div of numbers is standard deviation" (sd n))

(defmethod sd ((n num)) "sd"
 (/ (- (per n 0.9) (per n 0.1)) 2.56))

(defmethod per ((n num) &optional (p 0.5))
 "return the p-th within the sorted items"
 (let* ((v (all n)) (s (length v)))
  (svref v (floor (* p s)))))

(defmethod lo ((n num)) "smallest items" (svref (all n) 0))

(defmethod hi ((n num) &aux (a (all n))) "biggest item"
 (svref a (1- (length a))))

(defmethod dist1 ((n num) a b)
 "Normalize numbers before doing distance. For unknowns, assume max distance."
 (cond ((eq a #\?) (setf b (norm n b) a (if (> b 0.5) 1 0)))
  ((eq b #\?) (setf a (norm n a) b (if (> a 0.5) 1 0)))
  (t (setf a (norm n a) b (norm n b))))
 (abs (- a b)))

(defmethod norm ((n num) x) "normalize numbers"
 (if (eq x #\?) x
  (let ((n1 (lo n)) (n2 (hi n)))
   (if (< (abs (- n1 n2)) 1.0E-9) 0
    (max 0 (min 1 (/ (- x n1) (- n2 n1 1.0E-32))))))))

(defmethod combine ((n1 num) (n2 num)) "Combine numerics"
 (let ((n3 (make-num :txt (o n1 txt) (o n1 pos))))
  (loop for n across (o n1 _all) do (add n3 n)) n3))

````


; W? DO DOC  FAIL
