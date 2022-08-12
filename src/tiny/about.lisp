(defstruct+ about names all x y klass)

(defun make-about (lst)
  (let (all x y kl (at -1))
    (dolist (str lst (%make-about :names lst :x x :y y :klass kl
                                  :all (reverse all)))
      (incf at)
      (let ((col (if (eq #\$ (char str 0)) (make-num str at) (make-sym str at))))
        (push col all)
        (unless (eq #\~ (charn str))
          (if (member (charn str) '(#\! #\- #\+)) (push col y) (push col x))
          (if (eq #\! (charn str)) (setf kl col)))))))
