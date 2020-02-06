;; vim: ts=2 sw=2 sts=2  et :
;-------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(got "lib/rand.lisp")

(progn
  (reset-seed)
  (let* ((n 20)
          z
         (x (loop for x from 1 to n collect (randi)))
         (y (loop for x from 1 to n collect (randi))))
    (reset-seed)
    (setf z (loop for x from 1 to n collect (randi)))
    (print y)
    (print x)
    (print z)
    (print (mapcar #'(lambda (a b) (- a b)) x z))
    (print (mapcar #'(lambda (a b) (- a b)) x y))))
