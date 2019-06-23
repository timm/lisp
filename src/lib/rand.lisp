#-------- -------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(let* ((seed0      10013)
       (seed       seed0)
       (multiplier 16807.0d0)
       (modulus    2147483647.0d0))
  (defun reset-seed (&optional (n seed0))  
    (setf seed n))
  (defun randf (&optional (n 1)) 
    (* n (- 1.0d0 (park-miller-randomizer))))
  (defun randi (&optional (n 1)) 
    (floor (* n (/ (randf 1000.0) 1000))))
  (defun park-miller-randomizer ()
    "cycle= 2,147,483,646 numbers"
    (setf seed (mod (* multiplier seed) modulus))
    (/ seed modulus)))
