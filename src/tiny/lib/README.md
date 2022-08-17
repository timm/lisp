

## [egs.lisp](egs.lisp)


`(eg (what arg doc &rest src))`

<ul>   define a example
</ul>


<details>
<summary>(code)</summary>

```lisp
(defmacro eg (what arg doc &rest src)
 `(push (list ',what ',doc (lambda ,arg ,@src)) *egs*))
```

</details>


`(demos (settings all &optional one))`

<ul>   
Run `one` (or `all`) the demos. Reset globals between each
  run.  Return to the operating systems the failure count (so
  fails=0 means `successs`).
</ul>


<details>
<summary>(code)</summary>

```lisp
(defun demos (settings all &optional one)
 (let ((fails 0) (resets (copy-list settings)))
  (dolist (trio all)
   (destructuring-bind (what doc fun) trio (setf what (format nil ~(~a~) what))
    (when (member what (list 'all one) test 'equalp)
     (loop for (key . value) in resets do
      (setf (cdr (assoc key settings)) value))
     (setf *seed* (or (cdr (assoc 'seed settings)) 10019))
     (unless (eq t (funcall fun)) (incf fails)
      (format t ~&fail [~a] ~a ~% what doc)))))
  (exit fails)))
```

</details>



## [lists.lisp](lists.lisp)



## [macros.lisp](macros.lisp)


`(! (l x))`

<ul>   Get into association lists.
</ul>


<details>
<summary>(code)</summary>

```lisp
(defmacro ! (l x) `(cdr (assoc ',x ,l)))
```

</details>


`(? (s x &rest xs))`

<ul>   (? obj x y z) == (slot-value (slot-value (slot-value obj 'x) 'y) 'z)
</ul>


<details>
<summary>(code)</summary>

```lisp
(defmacro ? (s x &rest xs)
 (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))
```

</details>


`(geta (x lst &optional (init 0)))`

<ul>   Endure lst has a slot for `x`. If missing, initialize it with `init`.
</ul>


<details>
<summary>(code)</summary>

```lisp
(defmacro geta (x lst &optional (init 0))
 `(cdr
   (or (assoc ,x ,lst test #'equal)
    (car (setf ,lst (cons (cons ,x ,init) ,lst))))))
```

</details>



## [maths.lisp](maths.lisp)


`(rnd (number &optional (digits 3)))`

<ul>   Round to `digits` decimal places.
</ul>


<details>
<summary>(code)</summary>

```lisp
(defun rnd (number &optional (digits 3))
 (let* ((div (expt 10 digits)) (tmp (/ (round (* number div)) div)))
  (if (zerop digits) (floor tmp) (float tmp))))
```

</details>


`(randf (&optional (n 1.0)))`

<ul>   Random float 0.. n
</ul>


<details>
<summary>(code)</summary>

```lisp
(defun randf (&optional (n 1.0))
 (setf *seed* (mod (* 16807.0d0 *seed*) 2.147483647d9))
 (* n (- 1.0d0 (/ *seed* 2.147483647d9))))
```

</details>


`(randi (&optional (n 1)))`

<ul>   Random int 0..n
</ul>


<details>
<summary>(code)</summary>

```lisp
(defun randi (&optional (n 1)) (floor (* n (/ (randf 1.0e9) 1000000000))))
```

</details>



## [readme.lisp](readme.lisp)


`(readme (&optional (s t)))`

<ul>   
Generate README.md from all doco strings
  form all LISP code in a directory.
</ul>


<details>
<summary>(code)</summary>

```lisp
(defun readme (&optional (s t))
 (dolist
  (f
   (sort (directory *.lisp)
    #'(lambda (x y) (string< (pathname-name x) (pathname-name y)))))
  (let ((name (pathname-name f)))
   (format t ~%~%## [~a.lisp](~a.lisp)~%~% name name)
   (reads f
    (lambda (x)
     (labels
      ((lisps (x) (format nil ~%~%```lisp~%~(~a~)~%```~%~% x))
       (details (x)
        (format nil ~%~%<details>~%<summary>(code)</summary>~a</details>~%~%
         x))
       (defp nil (member (first x) '(defun defmacro defmethod)))
       (secret nil (char= _ (elt (symbol-name (second x)) 0)))
       (docp nil
        (and (> (length x) 3) (stringp (fourth x)) (not (equal  (fourth x)))))
       (dump (str &optional (pad )) (format s ~a~a~% pad str)))
      (when (and (defp) (docp) (not (secret)))
       (format s ~%`(~(~a~) ~(~a~))`~%~%<ul> (second x) (or (third x) ))
       (dump (fourth x)    ) (format s </ul>~%)
       (format s ~a
        (details (lisps (append (subseq x 0 3) (cddddr x))))))))))))
```

</details>



## [settings.lisp](settings.lisp)


`(cli (key.flag.help.default))`

<ul>   If `flag` exists on command line, update `key`.
</ul>


<details>
<summary>(code)</summary>

```lisp
(defun cli (key.flag.help.default)
 (destructuring-bind (key flag help default) key.flag.help.default
  (declare (ignore help))
  (let* ((args *args*) (it (member flag args test 'equalp)))
   (cons key
    (cond ((not it) default) ((equal default t) nil) ((equal default nil) t)
     (t (thing (second it))))))))
```

</details>


`(settings (header options))`

<ul>   Update settings. If  `help` is set, print help.
</ul>


<details>
<summary>(code)</summary>

```lisp
(defun settings (header options)
 (let ((tmp (mapcar (lambda (x) (cli x)) options)))
  (when (! tmp help) (format t ~&~{~a~%~}~%options:~% (lines header))
   (dolist (one options)
    (destructuring-bind (flag help default) (cdr one)
     (format t   ~a   ~a = ~a~% flag help default))))
  tmp))
```

</details>



## [strings.lisp](strings.lisp)


`(charn (x))`

<ul>   Last thing from a string.
</ul>


<details>
<summary>(code)</summary>

```lisp
(defun charn (x) (and (stringp x) (> (length x) 0) (char x (1- (length x)))))
```

</details>


`(trim (x))`

<ul>   Kill leading tailing whitespace.
</ul>


<details>
<summary>(code)</summary>

```lisp
(defun trim (x)
 (string-trim
  '(  	
    )
  x))
```

</details>


`(thing (x &aux (y (trim x))))`

<ul>   Turn `x` into a number or string or `?`.
</ul>


<details>
<summary>(code)</summary>

```lisp
(defun thing (x &aux (y (trim x)))
 (cond ((string= y ?) ?) ((string= y t) t) ((string= y nil) nil)
  (t (let ((z (read-from-string y nil nil))) (if (numberp z) z y)))))
```

</details>


`(splits (str &key (char ,) (filter #'identity)))`

<ul>   Divide `str` on `char`, filtering all items through `filter`.
</ul>


<details>
<summary>(code)</summary>

```lisp
(defun splits (str &key (char ,) (filter #'identity))
 (loop for start = 0 then (1+ finish) for finish =
  (position char str start start) collecting
  (funcall filter (trim (subseq str start finish))) until (null finish)))
```

</details>


`(with-lines (file fun))`

<ul>   Call `fun` for each line in `file`.
</ul>


<details>
<summary>(code)</summary>

```lisp
(defun with-lines (file fun)
 (with-open-file (s file)
  (loop (funcall fun (or (read-line s nil) (return))))))
```

</details>



## [structs.lisp](structs.lisp)


`(defstruct+ (x doco &body body))`

<ul>   Creates %x for constructor, enables pretty print, hides slots with '_' prefix.
</ul>


<details>
<summary>(code)</summary>

```lisp
(defmacro defstruct+ (x doco &body body)
 (let*
  ((slots (mapcar (lambda (x) (if (consp x) (car x) x)) body))
   (show (remove-if (lambda (x) (eq _ (char (symbol-name x) 0))) slots)))
  `(progn
    (defstruct (,x (constructor ,(intern (format nil %make-~a x)))) ,@body)
    (defmethod print-object ((self ,x) str)
     (labels ((fun (y) (format nil :~(~a~) ~a y (slot-value self y))))
      (format str ~a (cons ',x (mapcar #'fun ',show))))))))
```

</details>



## [symbols.lisp](symbols.lisp)

