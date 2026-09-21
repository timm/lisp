; vim: set ft=lisp ts=2 sw=2 et :
; ezr.lisp -- options and examples, built on lib.lisp.
; (c) 2026 Tim Menzies, timm@ieee.org, MIT license.
;
;   sbcl --script ezr.lisp -s 42 --all
;
; -s 42 sets an option; --foo runs (eg--foo) with fresh
; options and a fresh seed. Exit code = failure count.

(load (merge-pathnames "lib.lisp" *load-truename*))

;;; ---- options -----------------------------------------------
(defun defaults ()
  '((seed "-s" "random number seed" 1234567891)
    (p    "-p" "distance coeffecient" 2)))

(setf *settings* (defaults))

;;; ---- examples ----------------------------------------------
(defun eg--the ()
  "Show options."
  (dolist (o *settings*)
    (destructuring-bind (key flag doc val) o
      (format t "~&  ~8a ~(~a~)=~a~28t~a~%"
              flag key val doc)))
  (assert (numberp (! seed))))

(defun eg--rand ()
  "Random numbers are reproducible per test."
  (let ((a (rint 100)) (b (rint 100)))
    (kv :rints (list a b) :seed (! seed))
    (assert (and (<= 0 a 99) (<= 0 b 99) (/= a b)))))

(defun eg--macros ()
  "Exercise the macro kit."
  (let (c)
    (let+ ((counts (dolist (x '(a b a) c) (incf (has x c))))
           (evens  (loop for x in '(1 2 3)
                         if (oddp x) collect (* x x)))
           (arrow  (funcall (-> (+ %1 1)) 1))
           ((q r)  (floor 7 2))
           (cells  (things "1, ?,hi")))
      (kv :has counts :loop evens :arrow arrow
          :let+ (list q r) :things cells)
      (assert (equal counts '((b . 1) (a . 2))))
      (assert (equal evens '(1 9)))
      (assert (eql arrow 2))
      (assert (and (eql q 3) (eql r 1)))
      (assert (equal cells '(1 ? "hi"))))))

(defun eg--all (&aux (fails 0))
  "Run every other example."
  (dolist (f '(eg--the eg--rand eg--macros) fails)
    (setf fails (run f fails))))

(cli (defaults))
