;; sbcl --script ens30.lisp: eg-ens, 30 trees, 16 bins, sqrt cols.
;; SETS="se/defect/ant ..." overrides *ens* (same trick as se.lisp).
(dolist (f '("lithp" "ai" "eg"))
  (load (merge-pathnames f *load-truename*)))
(in-package :bald)
(setf (about-bins *k*) 16)
(setf *treatments* '(("n30" :n 30 :rf t)))
(let ((s (sb-ext:posix-getenv "SETS")))
  (when s
    (setf *ens* (loop :for i := 0 :then (1+ j)
                      :for j := (position #\Space s :start i)
                      :collect (subseq s i j) :while j))))
(srand (? *k* seed))
(eg-ens)
