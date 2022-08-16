(defstruct+ cols
  "Factory for making nums or syms." 
  names  ; list of column names
  all    ; all the generated columns
  x      ; just the independet columns
  y      ; just the dependent columns
  klass) ; just the klass col (if it exists)

(defun make-cols (lst)
  "Upper/lowercase words ==> nums/syms. Kept in 'all' and maybe elsewhere."
  (let (all x y kl (at -1))
    (dolist (str lst (%make-cols 
                       :names lst :x x :y y :klass kl :all (reverse all)))
      (let* ((what (if (upper-case-p (char str 0)) #'make-num #'make-sym))
             (col  (funcall what str (incf at))))
        (push col all)
        (unless (eq #\~ (charn str))
          (if (member (charn str) '(#\! #\- #\+)) (push col y) (push col x))
          (if (eq #\! (charn str)) (setf kl col)))))))
