#-ish(load "../lib/ish")

(needz  "lib/ok")

(deftest 
  aa()
  "asdas"
  (test 1 2 "not int ~a" 'k)
  (test (/ 3 0)  2 "not int ~a" 'k))

;(tests)

