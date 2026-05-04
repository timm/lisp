;## simplify debugging
#+sbcl (declaim (sb-ext:muffle-conditions
                  warning style-warning))
#+sbcl (setf sb-ext:*invoke-debugger-hook*
             (lambda (c h) (declare (ignore h))
               (format *error-output* "~&[ERROR] ~a~%" c)
               (sb-ext:exit :code 1)))

;## Macros
(defmacro defread (name (stream) &body body)
  "Install BODY as reader-macro for char NAME."
  `(set-macro-character
     ,(character (symbol-name name))
     (lambda (,stream c) (declare (ignore c)) ,@body)
     t))

(defread !(s)
  `(second (assoc ',(read s t nil t) *the*)))

(defread $(s)
  `(slot-value i ',(read s t nil t)))

(defmacro ? (x &rest at)
  "Nested slot access: (? x a b)
   = (slot-value (slot-value x 'a) 'b)."
  (if at `(? (slot-value ,x ',(car at)) ,@(cdr at))
         x))

(defmacro if+ (test then &optional else)
  "Anaphoric if: bind `it` to TEST in THEN/ELSE."
  `(let ((it ,test)) (if it ,then ,else)))

; ## utilities
; ### Random
(defvar *seed* !seed)

(defun rand (&optional (n 1))
  "Reproducible float in [0,n). Advances *seed*."
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 100) &aux (base 1E10))
  "Reproducible integer in [0,n)."
  (floor (* n (/ (rand base) base))))

; ## lists
(defun shuffle (lst &aux (v (coerce lst 'vector)))
  "Fisher-Yates shuffle of LST. Seeded via *seed*."
  (loop for i from (1- (length v)) downto 1 do
    (rotatef (aref v i) (aref v (rint (1+ i)))))
  (coerce v 'list))

; ## characters
(defun ch (s n)
  "Char at N of S. Negative N counts from end."
  (char s (if (minusp n) (+ (length s) n) n)))

; ## strings
(defun split (s sep)
  "Split S on character SEP into substring list."
  (loop for start = 0 then (1+ pos)
    for pos = (position sep s :start start)
    collect (subseq s start (or pos (length s)))
    while pos))

(defun thing (str
              &aux (v (ignore-errors
                        (read-from-string str))))
  "Coerce STR to number; '? for \"?\"; else string."
  (cond ((numberp v) v)
        ((string= str "?") '?)
        (t str)))

(defun read-csv (file)
  "Read FILE as CSV; coerce cells via `thing`."
  (with-open-file (s file)
    (loop for line = (read-line s nil) while line
      collect (mapcar #'thing (split line #\,)))))

; ## Tests
(defun run (it &optional arg)
  "Dispatch --flag to EG-FLAG function."
  (let* ((f (if (symbolp it) it
                (intern (format nil
                                "EG~:@(~a~)" it))))
         (n (symbol-name f)))
    (when (and (fboundp f)
               (> (length n) 3)
               (string= n "EG-" :end1 3))
      (setf *seed* !seed)
      (if arg (funcall f arg) (funcall f))
      t)))

; ## os
(defun args ()
  "Argv as list of strings (SBCL/CLISP portable)."
  #+sbcl (cdr sb-ext:*posix-argv*)
  #+clisp ext:*args*)

(defun cli (lsts)
  "Walk argv (flag arg) pairs: dispatch or update."
  (loop for (flag arg) on (args) by #'cddr do
    (unless (run flag (thing arg))
      (if+ (find flag lsts
                 :key #'third :test #'equalp)
           (setf (second it) (thing arg))))))
