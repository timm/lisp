;; vim: set ts=2 sts=2 et :
(unless (fboundp 'got) (load "../got"))

(got "macros.lisp")

(defmacro doread ((it f  &key out (take #'read)) &body body)
  "Iterator for running over files or strings."
  (let ((str (gensym)))
    `(with-open-file (,str f)
       (loop for ,it = (funcall ,take ,str nil)
             while ,it do 
             (progn ,@body))
       ,out)))

(defun para1 (f)
  "Read everything up to first blank line."
  (with-output-to-string (str)
    (doread (x f :take #'read-line)
      (if (equalp "" (string-trim '(#\Space #\Tab) x))
        (return)
        (format str "~a~%" x)))))

(defun lines (x &optional (s (make-string-input-stream x)))
  "Convert a string to a list of lines"
  (aif (read-line s nil)
       (cons it (lines nil s))))

(defun words (s &key (sep '(#\, #\space #\tab #\newline)))
  (with-input-from-string (str s)
    (let (tmp out)
      (labels 
        ((complete () 
            (when tmp
              (push (concatenate 'string (reverse tmp)) out) 
              (setf tmp nil) 
              out)))
        (awhile (read-char str nil)
          (if (member it sep :test #'eq)
            (complete)
            (push it tmp)))
        (reverse (complete))))))

(print (words "   asd,as as,das asd  asd as"))
