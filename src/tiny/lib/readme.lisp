(defun reads (file fun)
  "For every s-expression in `file`, call `fun`."
  (with-open-file (s file)
    (loop (funcall fun (or (read s nil nil) (return))))))

(defun read-lines (file fun)
  "For every line in `file`, call `fun`."
  (with-open-file (s file)
    (loop (funcall fun (or (read-line s nil nil) (return))))))

(defun para1(file fun)
  (read-lines file (lambda(line) 
     (if (zerop (length line)) (return-from para1)) (funcall fun line))))

(defun defp  (x) 
   "is this  a thing we wwant?"
   (member (first x) '(defun defmacro defmethod)))

(defun secret (x) 
  "is this a thing to hide?"
  (char= #\_ (elt (symbol-name (second x)) 0)))

(defun docp (x)  
  "got doc?"
  (and    (> (length x) 3)
          (stringp (fourth x))
          (not (equal "" (fourth x)))))

(defun readme(&optional (s t))
  "Generate README.md from all doco strings
  form all LISP code in a directory."
  (para1 "../README.md" (lambda (x) (format t "~a~%" x)))
  (terpri)
  (terpri)
  (dolist (f (sort (directory "*.lisp")
                   #'(lambda (x y) (string< (pathname-name x)
                                            (pathname-name y)))))
    (let ((name (pathname-name f)))
      (format t "~%~%## [~a.lisp](~a.lisp)~%~%" name name)
      (format t "|Name |Args | Doc|~%|--:|--|---|~%")
      (reads f 
        (lambda (x)
          (when (and (defp x) (docp x) (not (secret x)))
            (format s "|`~(~a~)` | `~(~a~)` |~a |~%"
                    (second x) (or (third x) "") 
                    (substitute #\SPACE #\NEWLINE (string (fourth x))))))))))

(readme)
