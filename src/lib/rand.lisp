;; vim: ts=2 sw=2 sts=2 et:
;-------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(fyi "The LISP random number generator does not let me
easily set the same seeds on mutilple plaforms. Hemce,
these 12 lines of code.

`reset-seed (&optional n)`

<ul>Reset seed to `n` (default = `(my :rand :seed)`</ul>

`randf (&optional (n 100))

<ul>Generate a random float in the range 0.. n. </ul>

`randi (&optional (n 1))

<ul>Generate a random integer in the range 0.. n. </ul>")

(let* ((seed       (the :rand :seed))
       (multiplier 16807.0d0)
       (modulus    2147483647.0d0))
  (defun reset-seed (&optional (n (the :rand :seed)))  
    (setf seed n))
  (defun randf (&optional (n 1)) 
    (* n (- 1.0d0 (_park-miller-randomizer))))
  (defun randi (&optional (n 100)) 
    (floor (* n (/ (randf 1000.0) 1000))))
  (defun _park-miller-randomizer ()
    "cycle= 2,147,483,646 numbers"
    (setf seed (mod (* multiplier seed) modulus))
    (/ seed modulus)))
