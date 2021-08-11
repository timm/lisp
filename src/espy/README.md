# Etc
## Misc utils
Macros
### aif
Anaphoric if (the result of the condition is cached in `it`).

<ul><details><summary>CODE</summary>

```lisp
(defmacro aif (test yes &optional no)
  `(let ((it ,test)) (if it ,yes ,no)))
```

</details></ul>

### whale
Anaphoric while (the current of the loop controller is cached in `a`).

<ul><details><summary>CODE</summary>

```lisp
(defmacro whale (expr &body body) 
  `(do ((a ,expr ,expr)) ((not a)) ,@body))
```

</details></ul>

### ?
Recursive  plist accessore.g. `(? p :outer :inner)`.

<ul><details><summary>CODE</summary>

```lisp
(defmacro ? (p x &rest xs)
  (if (null xs) `(getf ,p ,x) `(? (getf ,p ,x) ,@xs)))
```

</details></ul>

### o
Recurse struct accessore.g. `(o s address street number)`.

<ul><details><summary>CODE</summary>

```lisp
(defmacro o (s x &rest xs)
  (if (null xs) `(slot-value ,s ,x) `(o (slot-value ,s ,x) ,@xs)))
```

</details></ul>

### want
Simpler assert statement.

<ul><details><summary>CODE</summary>

```lisp
(defmacro want (x &rest y)
  `(assert ,x () ,@y))
```

</details></ul>

### rnd
Return  number with `places` number of decimals."

<ul><details><summary>CODE</summary>

```lisp
(defun rnd (number &optional (places 0))
  (let ((div (expt 10 places)))
    (float (/ (round (* number div)) div))))
```

</details></ul>

--------------------------------------------
## Random Numbers
I confess that I never found a way to do
platform independent random number generation with
CommonLisp. So I write my own."

<ul><details><summary>CODE</summary>

```lisp
(defvar *seed* 10013)
```

</details></ul>

### srand
Reset random number seed,

<ul><details><summary>CODE</summary>

```lisp
(defun srand (&optional (n 10013))  
  (setf *seed* n))
```

</details></ul>

### randi
Return a random integer 0.. n-1.

<ul><details><summary>CODE</summary>

```lisp
(defun randi (&optional (n 1)) 
  (floor (* n (/ (randf 1000.0) 1000))))
```

</details></ul>

### randf
Return a random flaot 0..n-1.

<ul><details><summary>CODE</summary>

```lisp
(defun randf (&optional (n 1.0)) 
  (_park-miller n))

(defun _park-miller (n)
  (let ((multiplier 16807.0d0)
        (modulus    2147483647.0d0))
    (setf *seed* (mod (* multiplier *seed*) modulus))
    (* n (- 1.0d0 (/ *seed* modulus)))))
```

</details></ul>

## System
Wrapper functions to SBCL system functions with strange names.
### halt
Exit

<ul><details><summary>CODE</summary>

```lisp
(defun halt (&optional (status 0)) (sb-ext:exit :code status))
```

</details></ul>

### argv
Arguments

<ul><details><summary>CODE</summary>

```lisp
(defun argv () sb-ext:*posix-argv*)
```

</details></ul>

### cli
Given a plist with keywords, if  the command line 
has any of the same keywords, then update the plist with the
command-line values.keywords that 

<ul><details><summary>CODE</summary>

```lisp
(defun cli (&key (plist  (deepcopy +config+)) 
                 (help   "")
                 (args   (cdr (deepcopy (argv))))
                 (now    (getf plist :all)))
   (whale (pop args)
     (print a)
     (setf a (read-from-string a))
     (cond ((equalp a :H)  (format t "~a~%" help))
           ((getf plist a) (setf now (getf plist a)))
           ((getf now a)   (setf (getf now a) 
                                 (read-from-string (car args))))
            ((keywordp a)   (format t (red "?? ignoring [~a]") a))))
   plist)
```

</details></ul>

## Types
### num?
Return a number (if we can). 

<ul><details><summary>CODE</summary>

```lisp
(defun num? (x &optional looping)
  (cond ((numberp x) x)
        ((stringp x) (let ((y (read-from-string x)))
                       (if (numberp y) y x)))
        (t x))) 
```

</details></ul>

## List stuff
### Deepcopy
Deep copy a list.

<ul><details><summary>CODE</summary>

```lisp
(defun deepcopy (x)
   (if (consp x) (mapcar #'deepcopy x) x))
```

</details></ul>

### Powerset
Return all subsets of a list.

<ul><details><summary>CODE</summary>

```lisp
(defun powerset (lst)
  (let ((out (list nil)))
    (dolist (x lst out)
      (dolist (tmp out)
        (push (cons x tmp) out)))))
```

</details></ul>

## Colors
### color
Return string `s`, surrounded by ANSI escape color sequences.

<ul><details><summary>CODE</summary>

```lisp
(defun color (s c &optional (str t))
  (let ((all '((black . 30) (red . 31) (green . 32)  (yellow . 33) 
               (blue . 34)  (magenta . 35) (cyan . 36) (white .37))))
    (format str "~c[~a1m~a~c[0m" #\ESC (cdr (assoc c all)) s #\ESC)))
```

</details></ul>

### red

<ul><details><summary>CODE</summary>

```lisp
(defun red (s) (color s 'red nil))
### green

(defun green (s) (color s 'green nil))
### yellow

(defun yellow (s) (color s 'yellow nil))
```

</details></ul>

## String stuff
### str->words
Kill white space, split string on `sep` (defaults to ',').

<ul><details><summary>CODE</summary>

```lisp
(defun str->words (s0 &optional (sep #\comma)) 
  (labels ((whitep (c) (member c '(#\space #\tab)))
           (worker (str &optional (lo 0))
                   (aif (position sep str :start lo)
                        (cons (subseq str lo  it) (worker str (1+ it)))
                        (list (subseq str lo)))))
    (let ((s1 (remove-if  #'whitep s0)))
      (unless (zerop (length s1)) (worker  s1)))))
```

</details></ul>

### file->words
For each line  in file `f`, call a function `fn` on a list of words in each line.

<ul><details><summary>CODE</summary>

```lisp
(defun file->words (f fn)
  (with-open-file (s f) 
    (whale (read-line s nil)
      (aif (str->words a) (funcall fn  it)))))
```