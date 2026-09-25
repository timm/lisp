;; check.lisp -- compile a .lisp, report warnings as file:line.
;;
;;     sbcl --script ,/check.lisp lib.lisp
;;
;; The file's own `muffle-conditions` declaim would hide every
;; one of these, so compile a copy with that line commented
;; out.  Same line count, so the numbers still point at yours.

(defvar *src* (second sb-ext:*posix-argv*))
(defvar *tmp* "/tmp/check-src.lisp")
(defvar *txt*
  (with-open-file (in *src*)
    (let ((s (make-string (file-length in))))
      (subseq s 0 (read-sequence s in)))))

(defun line-of (pos)
  "Character POS ==> a line number.  The compiler points just
   before a form, so walk on to that form's own paren."
  (1+ (count #\Newline *txt* :end
             (or (position #\( *txt* :start
                           (min pos (length *txt*)))
                 0))))

(with-open-file (out *tmp* :direction :output
                     :if-exists :supersede)
  (with-input-from-string (in *txt*)
    (loop for s = (read-line in nil) while s do
      (write-line (if (search "muffle-conditions" s) ";;" s)
                  out))))

(handler-bind
    ((warning
       (lambda (c &aux (x (sb-c::find-error-context nil)))
         (format t "~&~a:~a: ~a~%" (file-namestring *src*)
           (if x (line-of
                   (sb-c::compiler-error-context-file-position
                     x))
               "?")
           (substitute #\Space #\Newline (princ-to-string c)))
         (if (find-restart 'muffle-warning c)
           (muffle-warning c)))))
  (with-compilation-unit ()
    (compile-file *tmp* :output-file "/tmp/check.fasl"
                  :verbose nil :print nil)))
