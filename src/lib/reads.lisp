; vim: ts=2 slw=2 sts=2 et
(unless (fboundp 'got) (load "../got"))

(got "macros.lisp")

(defmacro doread ((it f &key out (take #'read)) &body body)
  "Iterator for running over files or strings."
  (let ((str (gensym)))
    `(with-open-file (,str f)
       (loop for ,it = (funcall ,take ,str nil)
             while ,it do 
             (progn ,@body))
       ,out)))

(defun para1 (f)
  "Read everything up to first blank line."
  (with-output-to-string (out)
    (doread (x f :take #'read-line)
      (if (equalp "" (string-trim '(#\Space #\Tab) x))
        (return)
        (format t "~a~%" x)))))

(defun lines (x &optional (s (make-string-input-stream x)))
  "Convert a string to a list of lines"
  (aif (read-line s nil)
       (cons it (lines nil s))))

(defun delimiterp (c) (or (char= c #\Space) (char= c #\,)))

(defun slice (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
        :then      (position-if-not delimiterp string :start (1+ end))
        :for end = (and beg (position-if delimiterp string :start beg))
        :when beg :collect (subseq string beg end)
        :while end))

(print (slice "  ad,as, asdas,sda  asdas  "))
