;; replay1: the one-tree arm, on the folds an earlier eg-ens recorded.
;; DAT=runs/xxx.dat sbcl --script replay1.lisp > n1.dat
;; Reads each `data|aucs|secs|test row #s` line, rebuilds that very
;; test set from the row #s, trains forest :n 1 on the rest, and emits
;; the same line format (so the dats can be pasted into one table).
(dolist (f '("lithp" "ai" "eg"))
  (load (merge-pathnames f *load-truename*)))
(in-package :bald)
(setf (about-bins *k*) 16)
(srand (? *k* seed))

(defun chop (s ch)
  (loop :for i := 0 :then (1+ j)
        :for j := (position ch s :start i)
        :collect (subseq s i j) :while j))

(let ((file (sb-ext:posix-getenv "DAT")) tb v name)
  (format t "#1:q16 ~%")
  (with-open-file (in file)
    (read-line in nil)                              ; the #names header
    (loop :for line := (read-line in nil) :while line :do
      (destructuring-bind (f as ss ids) (chop line #\|)
        (declare (ignore as ss))
        (unless (equal f name)
          (setf name f
                tb   (data (format nil "~~/gits/moot/classify/~a.csv" f))
                v    (coerce (reverse (? tb rows)) 'vector)))
        (let* ((at   (? tb cols klass at))
               (y    (lambda (r) (raw r at)))
               (test (mapcar (lambda (s) (aref v (parse-integer s)))
                             (remove "" (chop (string-trim " " ids) #\Space)
                                     :test #'equal)))
               (h    (make-hash-table :test #'eq)))
          (dolist (r test) (setf (gethash r h) t))
          (let ((train (remove-if (lambda (r) (gethash r h)) (? tb rows))))
            (multiple-value-bind (a secs)
                (timed (lambda ()
                         (aucs (votes (forest tb train y :n 1) test y))))
              (format t "~a|~,3f |~,3f |~a~%" f a secs ids)
              (finish-output))))))))
