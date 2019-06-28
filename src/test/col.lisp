;; vim: ts=2 sw=2 sts=2  et :
;-------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(got "eg/col.lisp" "lib/ok.lisp")

(deftest _num () 
  "Compute number."
  (let ((nu (nums '(9 2 5 4 12 7 8 11 9 3
                   7 4 12 5 4 10 9 6 9 4))))
    (ok 3.0607877 (? nu 'sd))
    (ok 7         (? nu 'mu))))

(deftest _sym ()
  "Compute symbols"
  (ok t (ish 1.3787836 
             (ent 
               (syms '(a b b c c c c))))))

(tests)
