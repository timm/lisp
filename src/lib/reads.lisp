;; vim: ts=2 sw=2 sts=2 et:
;-------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(got "lib/macros.lisp")

(defmacro doread ((it f &optional out 
                      &key (take #'read)) &body body)
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
    (doread (x f nil :take #'read-line)
      (if (equalp "" (string-trim '(#\Space #\Tab) x))
        (return)
        (format str "~a~%" x)))))

(defun s->lines 
  (x &optional (s (make-string-input-stream x)))
  "Convert a string to a list of lines"
  (af (read-line s nil)
       (cons a (s->lines nil s))))

(defun s->words 
  (s &optional (sep '(#\, #\space #\tab #\newline)))
  "Convert a string to a list of words"
  (with-input-from-string (str s)
    (let (tmp out)
      (labels 
        ((end-of-word () 
            (when tmp
              (push (concatenate 'string (reverse tmp)) out) 
              (setf tmp nil) 
              out)))
        (whale (read-char str nil)
          (if (member a sep :test #'eq)
            (end-of-word)
            (push a tmp)))
        (reverse (end-of-word))))))
