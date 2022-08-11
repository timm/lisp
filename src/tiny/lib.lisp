;; hell
;;; macros
; ? obj x y z) == (slot-value (slot-value (slot-value obj 'x) 'y) 'z)
(defmacro ? (s x &rest xs)
 (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))

;;; accessors
(defun ! (l x) (cdr (assoc x l)))

;;; string
; Last thing from a string
(defun charn (x) (char x (1- (length x))))

; Kill leading tailing whitespace.
(defun trim (x) (string-trim '(#\Space #\Tab #\Newline) x))

; Turn `x` into a number or string or "?"
(defmethod thing (x) x)
(defmethod thing ((x string))
  (let ((y (trim x)))
    (if (string= y "?") y
      (let ((z (ignore-errors (read-from-string y))))
        (if (numberp z) z y)))))

; Divide `str` on `char`, filtering all items through `filter`.
(defun splits (str &key (char #\,) (filter #'identity))
  (loop for start = 0 then (1+ finish)
    for        finish = (position char str :start start)
    collecting (funcall filter (trim (subseq str start finish)))
    until      (null finish)))

; String to lines or cells of things
(defun lines (string) (splits string :char   #\Newline))
(defun cells (string) (splits string :filter #'thing))

; Call `fun` for each line in `file`.
(defun with-lines (file fun)
  (with-open-file (s file)
    (loop (funcall fun (or (read-line s nil) (return))))))

;;; maths
; Random number control (since reseeding in LISP is... strange).
(defvar *seed* 10013)
(defun randf (&optional (n 1.0)) 
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))
(defun randi (&optional (n 1)) (floor (* n (/ (randf 1000000000.0) 1000000000))))

;;; settings
; Update `default` from command line (if it contains `flag` or `key`).
; CLI flags for booleans flip the setting (so they need no following arg).
(defun cli (key.flag.help.default)
  (destructuring-bind (key flag help default) key.flag.help.default
    (let* ((args #+clisp ext:*args* 
                 #+sbcl sb-ext:*posix-argv*)
           (it (member flag args :test 'equalp)))
      (cons key (cond ((not it)            default)
                      ((equal default t)   nil)
                      ((equal default nil) t)
                      (t                   (thing (second it))))))))

; Update settings. If  `help` is set, print help.
(defun settings (header options)
  (let ((tmp (mapcar #'cli options)))
    (when (! tmp 'help)
     (format t "~&~%~{~a~%~}~%OPTIONS:~%" (lines header))
     (dolist (one options)
       (format t "  ~a   ~a = ~a~%" (second one) (third one) (fourth one))))
    tmp))

;;;  defstruct+ 
; Creates %x for base constructor, enables pretty print, hides private slots
; (those starting with "_").
(defmacro defstruct+ (x &body body) 
  (let* ((slots  (mapcar    (lambda (x) (if (consp x) (car x) x))          body))
         (public (remove-if (lambda (x) (eq #\_ (char (symbol-name x) 0))) slots)))
    `(progn
       (defstruct (,x (:constructor ,(intern (format nil "%MAKE-~a" x)))) ,@body)
       (defmethod print-object ((self ,x) str)
         (labels ((fun (y) (format nil ":~(~a~) ~a" y (slot-value self y))))
           (format str "~a" (cons ',x (mapcar #'fun ',public))))))))

;;; demos
; Define one demos.
(defvar *demos* nil)
(defmacro defdemo (what arg doc &rest src) 
  `(push (list ',what ',doc (lambda ,arg ,@src)) *demos*))

; Run `one` (or `all`) the demos. Reset globals between each run.
; Return to the operating systems the failure count (so fails=0 means "success").
(defun demos (settings all &optional one)
  (let ((fails 0)
        (resets (copy-list settings)))
    (dolist (trio all)
      (let ((what (first trio)) (doc (second trio)) (fun (third trio)))
        (when (member what (list 'all one))
          (loop for (key . value) in resets do 
            (setf (! settings key) value))
          (setf *seed* (or (! settings 'seed) 10019))
          (unless (eq t (funcall fun ))
            (incf fails)
            (format t "~&FAIL [~a] ~a ~%" what doc)))))
    #+clisp (exit fails)
    #+sbcl  (sb-ext:exit :code fails)))
