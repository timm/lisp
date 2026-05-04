(load "plus")

(defparameter *grammar3*
  '((Sentence -> (NP VP))
    (NP -> (Art Noun))
    (VP -> (Verb NP))
    (Art -> the) (Art -> a)
    (Noun -> man) (Noun -> ball) (Noun -> woman) (Noun -> table)
    (Noun -> noun) (Noun -> verb)
    (Verb -> hit) (Verb -> took) (Verb -> saw) (Verb -> liked)))

(defparameter *grammar4*
  '((S -> (NP VP))
    (NP -> (D N))
    (NP -> (D A+ N))
    (NP -> (NP PP))
    (NP -> (Pro))
    (NP -> (Name))
    (VP -> (V NP))
    (VP -> (V))
    (VP -> (VP PP))
    (PP -> (P NP))
    (A+ -> (A))
    (A+ -> (A A+))
    (Pro -> I) (Pro -> you) (Pro -> he) (Pro -> she)
    (Pro -> it) (Pro -> me) (Pro -> him) (Pro -> her)
    (Name -> John) (Name -> Mary)
    (A -> big) (A -> little) (A -> old) (A -> young)
    (A -> blue) (A -> green) (A -> orange) (A -> perspicuous)
    (D -> the) (D -> a) (D -> an)
    (N -> man) (N -> ball) (N -> woman) (N -> table) (N -> orange)
    (N -> saw) (N -> saws) (N -> noun) (N -> verb)
    (P -> with) (P -> for) (P -> at) (P -> on) (P -> by) (P -> of) (P -> in)
    (V -> hit) (V -> took) (V -> saw) (V -> liked) (V -> saws)))

(defun pick (xs) (nth (random (length xs)) xs))

(defun gen (s g)
  (if+ (for+ r for r in g if (eq (car r) s))
       (let+ ((rhs (third (pick it))))
         (if (listp rhs)
             (loop for x in rhs append (gen x g))
             (gen rhs g)))
       (list s)))

(defun wrap (ws &optional (w 60) &aux (c 0))
  (dolist (x ws (terpri))
    (let+ ((s (string-downcase (string x)))
           (n (length s)))
      (cond ((zerop c)         (princ s) (setf c n))
            ((> (+ c n 1) w)   (terpri) (princ s) (setf c n))
            (t (princ #\Space) (princ s) (incf c (1+ n)))))))

(defun story (&optional (g *grammar4*) (top 'S))
  (wrap (gen top g)))

;;; --- Cellphone feature model (FM-6) ------------------------
;;; Tree = nested list. First atom of each list = operator:
;;;   and  : include all children
;;;   opt  : 50/50; if in, include all children
;;;   xor  : pick exactly 1 child
;;;   or   : pick 1+ children
;;; Bare atoms = leaves (keys into payload hashtable).

(plus leaf
  "Per-feature metadata. cost/defect/fam ~ random 0..4.
   ben fixed at 1. bit in {nil, 0, 1}."
  (cost defect fam ben bit)
  (make ()
    (setf $cost   (random 5)
          $defect (random 5)
          $fam    (random 5)
          $ben    1
          $bit    nil)))

(defun atoms-of (sx)
  "All bare atoms inside SX (skips first atom of each list = op)."
  (cond ((atom sx) (list sx))
        (t (loop for k in (cdr sx) append (atoms-of k)))))

(plus fmtree
  "Stochastic feature tree. ROOT = nested sexpr.
   PAY = hashtable atom -> leaf struct."
  (root pay)
  (make (root &aux (i (%make-fmtree :root root)))
    (setf $pay (make-hash-table))
    (dolist (a (atoms-of root))
      (setf (gethash a $pay) (make-leaf)))))

(plus+ fmtree
  (show ()
    (format t "fmtree: ~a~%" $root)
    (maphash (lambda (k v)
               (format t "  ~12a c=~a d=~a f=~a b=~a bit=~a~%"
                       k (leaf-cost v) (leaf-defect v)
                       (leaf-fam v) (leaf-ben v) (leaf-bit v)))
             $pay)))

(defparameter *cellphone*
  '(and cellphone
     (opt wireless (or infrared bluetooth))
     (and accu_cell (xor li_ion ni_mh ni_ca))
     (and display  (xor color monochrome))))
