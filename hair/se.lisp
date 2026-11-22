;; sbcl --script se.lisp: eg-ens over moot/classify/se sets in env var SETS
(dolist (f '("lithp" "ai" "eg"))
  (load (merge-pathnames f *load-truename*)))
(in-package :hair)
(setf *ens* (let ((s (sb-ext:posix-getenv "SETS")))
              (loop :for i := 0 :then (1+ j) :for j := (position #\Space s :start i)
                    :collect (subseq s i j) :while j)))
(srand (? *k* seed))
(eg-ens)
