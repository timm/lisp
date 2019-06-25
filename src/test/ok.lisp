(unless (fboundp 'got) (load "../got"))

(got  "lib/ok.lisp")

(deftest someMayDie()
  "Except three tests to run, one succeed, one give error."
  (ok 1 2 "not int ~a" 'k)
  (ok (/ 3 0)  2 "not int ~a" 'k)
  (ok (/ 3 3)  2 "not int ~a" 'k)
)

(deftest success()
  "Expect allt ests to pass"
  (ok 1 1 "not int ~a" 'k))


(tests)
