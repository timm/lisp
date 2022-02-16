; Macros
(defmacro reader (com fun)
  (let ((fun1 (gensym)))
    `(progn (defun ,fun1 (stream char)
              (declare (ignore char))
              (,fun (read stream t nil t)))
            (set-macro-character ,com #',fun1))))

(defun help-text (x) (print 111) (print (second x)) (print (fourth x)) x)
(reader #\! help-text)

!(defmacro aif (test yes &optional no) "asdas" `(let ((it ,test)) (if it ,yes ,no)))

(aif nil (print it) (print 'nl))

(format t "as ~s ~s ~%" 1 2)

(defstruct (foo (:constructor %make-foo)) 
  y x)

(defun make-foo (&key x y) 
  ;; non-trivial initial values 
  (%make-foo :x (+ x y) :y 9 ))


(print (make-foo :x 10 :y 20))
; (defun point0 (&key (x 0) (y 0) (z 0))
;   (make-instance 'point :x x :y y :z z))
;
; (print (point0 :z 23))
