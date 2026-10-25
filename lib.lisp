; vim: set ft=lisp ts=2 sw=2 et lispwords+=loop :
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
  "Reproducible float in [0,n). Advances *seed*.
   Integer arithmetic, so SBCL and CLISP agree."
  (setf *seed* (mod (* 16807 *seed*) 2147483647))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 100))
  "Reproducible integer in [0,n)."
  (floor (* n (rand))))

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

(defun thing (str &aux v pos)
  "Coerce STR to number; '? for \"?\"; else string.
   Trims blanks/CR. Reader runs with *read-eval* nil."
  (let ((s (string-trim '(#\Space #\Tab #\Return #\Newline)
                        str)))
    (cond ((string= s "?") '?)
          (t (multiple-value-setq (v pos)
               (ignore-errors
                 (let ((*read-eval* nil))
                   (read-from-string s))))
             (if (and (numberp v) (eql pos (length s))) v s)))))

(defun cells (line &optional (sep #\,)
              &aux (out nil) (in nil)
                   (buf (make-string-output-stream)))
  "Split LINE into CSV cells. Honours \"quoted,fields\"
   and \"\" as an escaped quote."
  (loop for i from 0 below (length line)
    for c = (char line i) do
    (cond ((and in (char= c #\")
                (< (1+ i) (length line))
                (char= (char line (1+ i)) #\"))
           (write-char #\" buf) (incf i))
          ((char= c #\") (setf in (not in)))
          ((and (not in) (char= c sep))
           (push (get-output-stream-string buf) out))
          (t (write-char c buf))))
  (push (get-output-stream-string buf) out)
  (nreverse out))

(defun read-csv (file)
  "Read FILE as CSV; coerce cells via `thing`."
  (with-open-file (s file)
    (loop for line = (read-line s nil) while line
      collect (mapcar #'thing (cells line)))))

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

(defun cli (lsts &aux (av (args)))
  "Walk argv. Setting flags eat a value, eg- flags do not."
  (loop for flag = (pop av) while flag do
    (let ((entry (find flag lsts :key #'third :test #'equalp)))
      (cond (entry (setf (second entry)
                         (thing (or (pop av)
                                    (error "cli: ~a needs a value"
                                           flag)))))
            ((run flag))
            (t (format t "?? ~a~%" flag))))))
