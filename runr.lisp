(defpackage :runr (:use :cl))
(in-package :runr)

(defvar *help* "
runr: simple lisp
(c)2032 Tim Menzies <timm@ieee.org> BSD-2

USAGE: lisp runr.lisp [OPTIONS]

  -h help  show help          = nil
  -s seed  random number seed = 10019")

(dolist (trio settings) 
  (if (member (third 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  _   _   _    
; | | (_) | |__ 
; | | | | | '_ \
; |_| |_| |_.__/
(defun l() (load "runr"))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro ? (s x &rest xs) 
  "recursive slot-value access"
  (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))

(defun charn (s c &optional (n 0))
  "is `s` a string holding `c` at position `n`?"
  (and (stringp s) (< n (length s)) (eql c (char s n))))

(defun trim (s) 
 "kill leading,trailing whitespace"
  (string-trim '(#\Space #\Tab #\Newline) s))

(defun thing (s &aux (s1 (trim s)))
  "coerce `s` into a number or string or t or nil or #\?"
  (cond ((equal s1 "?") #\?)
        ((equal s1 "t") t)
        ((equal s1 "nil") nil)
        (t (let ((n (read-from-string s1 nil nil))) 
             (if (numberp n) n s1)))))

(defun words (s &optional (char #\,) (filter #'thing) (here 0))
  "generate words, filtered, from`s`"
  (let* ((there (position char s :start here))
         (word  (funcall filter (subseq s here there))))
    (labels ((tail () (if there (words s char filter (1+ there)))))
      (if (equal word "") (tail) (cons word (tail))))))

(defun with-lines (file fun)
  "Call `fun` for each line in `file`"
  (with-open-file (s file) 
    (loop (funcall fun (or (read-line s nil) (return))))))

(defmacro defstruct+ (x doco &body body)
  "Creates %x for constructor, enables pretty print, hides slots with '_' prefix."
  (let* ((slots (mapcar    (lambda (x) (if (consp x) (car x) x))          body))
         (show  (remove-if (lambda (x) (eq #\_ (char (symbol-name x) 0))) slots)))
    `(progn (defstruct (,x (:constructor ,(intern (format nil "%MAKE-~a" x)))) ,doco ,@body)
            (defmethod print-object ((self ,x) str)
              (labels ((fun (y) (format nil ":~(~a~) ~a" y (slot-value self y))))
                (format str "~a" (cons ',x (mapcar #'fun ',show))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ako (s) kind)
  "given a column header, comment on its the propertoes of that column"
  (if (upper-case-p (char s 0)) 
    'num
    (first (member (char s (1- (length s))) 
                   (cdr (assoc kind '((ignore #\X) (klass #\!) (less #\-)
                                                   (more #\+) (goal #\+ #\- #\!))))))))

(defstruct+ num (at 0) (txt "") (n 0) (mu 0) (m2 0) (w 1) (lo 1E31) (hi -1E21))

(defun num! (&optional (at 0) (txt ""))
  "summarizes streams of numbers"
  (%make-num :at at :txt txt :w (if (ako txt 'less) -1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *egs* nil)

(defun make-settings (s)
  "for lines like '  -Key Flag ..... Default', return (KEY flag (thing Default))"
  (loop :for (flag key . rest) 
        :in  (words s #\NewLine (lambda (s1) (words s1 #\Space #'trim)))
        :if  (charn flag #\-) 
        :collect (list (intern (string-upcase key)) (thing (car (last rest))) flag)))

(defun cli(threes)
  (let ((args  #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*))
    (dolist (three threes threes)
      (aif (member (third three) args :test 'equal)
           (let ((b4  (second it)))
             (setf (second three) (cond ((eq b4 t) nil)
                                        ((eq b4 nil) t)
                                        (t (thing (second it))))))))))




(print 1)
(print *help*)
(print (words "asdas,,,asdasda,,,"))
(print (settings *help*))
(print 10)

(defmacro eg (what arg doc &rest src) 
  "define a example"
  `(push (list ',what ',doc (lambda ,arg ,@src)) *egs*))

(defun demo (what doc fun)
  (unless (eq t (funcall fun ))
    (incf fails)
    (format t "~&FAIL [~(~a~)] ~a ~%" what doc)
    1)
  0) 

(defun demos (settings all &optional one)
  "Run `one` (or `all`) the demos, resttomg glonal s seatch time"
  (let ((fails 0)
        (resets (copy-list settings)))
    (dolist (trio all) 
       (loop for (key . value) in resets do (setf (cdr (assoc key settings)) value))
       (setf *seed* (or (cdr (assoc 'seed settings)) 10019))
       (incf fails (demo (first trio) (second trio) (third trio))))
    #+clisp (ext:exit fails)
    #+sbcl  (sb-ext:exit :code fails)))

 


