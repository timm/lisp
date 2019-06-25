; vim: ts=2 sw=2 sts=2  et
(unless (fboundp 'got) (load "../got"))

(defun string-lines (str)
  "Convert a string to a list of lines."
  (labels
    ((nl    (z)    (char= z #\Newline))
     (where (pos0) (position-if #'nl str :start pos0))
     (worker (pos0 &aux pos)
        (if (setf pos (where pos0))
            (cons (subseq str pos0 pos)
                  (worker (1+ pos)))
            (list (subseq str pos0)))))
    (worker 0)))

(defun lines (f &optional (fn #'print))
  (with-open-file (s f)
    (loop for line = (read-line s nil)
          while line do 
          (funcall fn line))))      

(defun para1 (f &aux tmp)
  (labels (
     (worker (s)
       (let ((line (string-trim '(#\Space #\Tab) s)))
         (if (equalp line "")
           (return-from para1 
              (format nil "~{~a~^~%~}" (reverse tmp)))
           (push s tmp)))))
    (lines f #'worker)))

(defun reads (f &optional  (fn #'print) (str t))
  "Read  a file, calling 'fn' on each s-expression. "
  (with-open-file (s f)
    (labels
      ((worker (&optional (one (read s nil :eof)))
              (unless (eq  one :eof)
                (funcall fn  one str)
                (worker))))
      (worker))))
