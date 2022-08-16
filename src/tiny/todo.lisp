
Does a symbol name start with b4?

(defun b4-sym (b4 sym &aux (n (length b4)) (s (symbol-name sym)))
  (and (>= (length s) n) (equalp b4 (subseq s 0 n))))
Returns all functions in a package.

(defun funs (&optional (package :common-lisp-user) &aux out)
  (aif (find-package package)
    (do-all-symbols (s package)
      (if (and (fboundp s) (eql it (symbol-package s)))
        (push s out))))
  out)

(defun run (eg my)
  (setf my      (copy-tree my)
        *seed*  (! my all seed))
  (if (! my all meek)
    (funcall eg my)
    (multiple-value-bind (_ e)
      (ignore-errors (funcall eg my))
      (incf (! my all tries))
      (cond (e (incf (! my all fails))
               (format t "~&~a [~a] ~a~%" (red "✖") eg (yellow e)))
            (t (format t "~&~a [~a]~%" (green "✔") eg ))))))

; Update `my` from the command  line.
; Run the appropriate test functions  (or if `eg` is "ls"
; then just list everything).
; Return to the operating system  the number of failures.
(defun main(my &key (package :common-lisp-user) (b4 "EG."))
  (let* ((egs (loop for fun in (funs package)
                if (b4-sym b4 fun) collect fun))
         (my  (cli my))
         (eg  (intern (string-upcase (! my all eg)))))
    (case eg
      (all (loop for fun in egs do (run fun my)))
      (ls  (loop for fun in egs do
             (format t "  :eg ~15a : ~a~%"
                     fun (or (documentation fun 'function) ""))))
      (otherwise (when (member eg egs) (run eg my))))
    (halt (! my all fails))))
:w


;;;;;;;;;;;;;;;;;;;;;;;
(load "lib/strings")
(load "lib/macros")
(defun parseHelp(str)
  (labels ((want       (s)   (and (> (length s) 0) (eq #\- (char s 0))))
           (splits     (s)   (cells s :char #\Space))
           (oneTwoLast (lst) `(,(first lst) ,(read-from-string (second lst)) 
                                            ,(car (last lst)))))
    (mapcar #'oneTwoLast (mapcar #'splits (remove-if-not #'want (lines str))))))

(defun settings1(str)
  (let ((threes (parseHelp str)))
    (list (cons '_helpstr str)
          (cons '_threes threes)
          (mapcar (lambda (lst) (cons (second lst) (third lst))) threes))))

(defun setting (flag.key.default)
  (destructuring-bind (flag key default)flag.key.default
    (let* ((args #+clisp ext:*args* 
                 #+sbcl sb-ext:*posix-argv*)
           (it (member flag args :test 'equalp)))
    ;(print (list key flag default it))
      (cons key (cond ((not it)            default)
                      ((equal default t)   nil)
                      ((equal default nil) t)
                      (t                   (thing (second it))))))))

(defun cl1 (lst) 
  (let ((tmp (mapcar #'setting  (! lst _threes))))
    (if (! tmp help) (print (! lst _helpstr)))
    lst))

(cl1 (settings1 "
ad
as
das
a
a

-h help shoe help nil
-a adas da aas as = 23
-K k asdd as as = t
-s seed asd as a asd = nil
-a asd sad as d = asda
"))


(defun chunks (lines &optional one all (b4 #\;) new)
  (if (null lines)
    (reverse (push (reverse one) all))
    (let ((line (pop lines)))
      (if (zerop (length line))
        (return-from chunks (chunks lines one all b4)))
      (cond ((equal (elt line 0) #\() (setf new #\())
            ((equal (elt line 0) #\;) (setf new #\;)
                                      (setf line (subseq line 2)))
            (t (setf new b4)))
      (cond ((eql new b4) (push line one)
                          (chunks lines one all b4))
            (t            (if one (push (reverse one) all))
                          (chunks lines (list line) all new))))))

(defun doc (file &optional (str t))
  (dolist (chunk (chunks (lines "espy.lisp")))
    (if (eql #\( (elt (car chunk) 0))
      (format str  "~%```lisp~%~{~a~%~}```~%~%" chunk)
      (format str  "~{~a~%~}" chunk))))



Skip to content
Search or jump to…
Pull requests
Issues
Marketplace
Explore

@timm
timm
/
lisp
Public
Code
Issues
3
Pull requests
Actions
Projects
Wiki
Security
Insights
Settings
lisp/src/lib/readme.lisp
@ai4se
ai4se saving
Latest commit 5a7864b on Jun 27, 2019
 History
 1 contributor
51 lines (45 sloc)  1.7 KB

;; vim: ts=2 sw=2 sts=2 et:
;-------- -------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(got "sys.lisp" "lib/reads.lisp")

(fyi "### Usage
```bash
cd src/xx
sbcl --script ../lib/readme.lisp > README.md
git add README.md
``` ")

(defun readme(dir &optional (s t))
  "Generate README.md from all doco strings
  form all LISP code in a directory."
  (format t "~a~%# ~a~%~%~%"
          (para1 "../../README.md")
          (string-upcase dir))
  (dolist (f (sort (directory "*.lisp")
                   #'(lambda (x y) (string< (pathname-name x)
                                            (pathname-name y)))))
    (let ((name (pathname-name f)))
      (format t "~%~%## [~a.lisp](~a.lisp)~%~%" name name)
      (doread (x f)
        (labels
          ((defp   () (member (first x) '(deftest defun
                                           defmacro defmethod)))
           (fyip   () (eql    (first x)  'fyi))
           (secret () (char= #\_ (elt (symbol-name (second x)) 0)))
           (docp   () (and    (> (length x) 3)
                              (stringp (fourth x))
                              (not (equal "" (fourth x)))))
           (dump   (str  &optional (pad ""))
                   (format s "~a~a~%" pad str)))
          (when (fyip)
            (terpri s) (terpri s)
            (dump (second x))
            (terpri s) (terpri s)
            )
          (when (and (defp) (docp) (not (secret)))
            (format s "~%`~(~a~) ~(~a~)`~%~%<ul>"
                    (second x) (or (third x) ""))
            (dump (fourth x) "   ")
            (format s "</ul>~%")))))))

(let ((cli (args)))
  (if (and cli (equalp "--makedoc" (first cli)))
    (readme (second cli))))

'(home (©2019 ("Tim Menzies")))
'(:site (src (contrib (discuss)))
  :code (lib (oo (sample))))


Footer
© 2022 GitHub, Inc.
Footer navigation
Terms
Privacy
Security
Status
Docs
Contact GitHub
Pricing
API
Training
Blog
About

