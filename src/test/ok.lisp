(unless (fboundp 'got) (load "../got"))

(got  "lib/ok.lisp")

(deftest aa()
  "asdas"
  (test 1 2 "not int ~a" 'k)
  (test (/ 3 0)  2 "not int ~a" 'k))
