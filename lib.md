---
layout: default
title: lib.lisp
---

# lib.lisp

*Tim Menzies &middot; <timm@ieee.org> &middot; <http://timm.fyi> &middot; Sept'26*

A file of useful LISP scripting tips. The code it explains is
[`lib.lisp`](lib.lisp), and the two are kept in step by `make
weave`: every fenced block below is pulled from that file's
docstrings, so the documentation cannot drift from the code.

## Preliminaries

**Installs.** SBCL to execute, rlwrap to debug, nvim to edit,
pandoc to document, gawk for the little bits of glue.

**Width.** Code is 65 characters wide, max. That is what fits a
three-column listing, and it is why nothing here has long names.

**Hash-bang.** A script can know how to run itself. Mark it
executable with `chmod +x lib.lisp`, then name the interpreter on
line one:

```
#!/usr/bin/env sbcl --script
```

After that, `./lib.lisp` just works. One catch: `load` is not a
shell, so it does not know what `#!` means. Any file that loads a
hash-bang script has to teach its reader to skip the line first,
which is what `ezr.lisp` does before it pulls in `lib.lisp`:

```lisp
(set-dispatch-macro-character #\# #\!
  (lambda (s c n) (declare (ignore c n)) (read-line s) (values)))
```

**Editing.** `nvim --clean` is a sane IDE with bracket matching
and nothing else. Teach it the indentation rules on lines 1 and 2:

```
;;<!-- vim: set ft=lisp ts=2 et sw=2 : -->
;;<!-- vim: set lispwords+=loop,format,error,labels,aif : -->
```

**Two lisps.** SBCL is faster, but CLISP is what many sites
already have, so this code targets both. The `#+sbcl` and
`#+clisp` forms pick the right branch at read time.

**The command line.** SBCL's REPL cannot even up-arrow to the
last command. Run it as `rlwrap sbcl` and that stops being true.

## Script header

SBCL is very loud about crashes. Two lines calm it down, and they
have to come after the handler they install:

```lisp
#+sbcl (declaim (sb-ext:muffle-conditions warning style-warning))
#+sbcl (setf sb-ext:*invoke-debugger-hook* #'brief-error)
```

Avoid SBCL's long errr dump. Just show 1 line messages.
```lisp
(defun brief-error (c h)
  (declare (ignore h))
  (format *error-output* "~&!! ~a~%"
    (substitute #\Space #\Newline (princ-to-string c)))
  (halt 1))
```

Exit, reporting FAILS. Zero exits quietly, with status zero.
```lisp
(defun halt (&optional (fails 0))
  (when (> fails 0)
    (format t "~&ERROR: ~a failure~:p~%" fails)
    #+clisp (ext:exit fails) #+sbcl (sb-ext:exit :code fails)))
```

## Macros

Macros go near the top of the file, so that everything below can
use them. Use them sparingly, though: they make code harder to
read, and the ones worth the cost are few.

Anaphoric if: THEN and ELSE read TEST's value as `it`; e.g.
```lisp
(aif (parse thing) (print it))
```
```lisp
(defmacro aif (test then &optional else)
  `(let ((it ,test)) (if it ,then ,else)))
```

Dive through nested structs; e.g.
```lisp
(? x a b) ==> (slot-value (slot-value x 'a) 'b)
```
```lisp
(defmacro ? (x k &rest ks)
  (if ks `(? (slot-value ,x ',k) ,@ks)
         `(slot-value ,x ',k)))
```

Dollar prefix reads a slot of the current struct, which must
be named `i`; e.g. `$x` ==> `(slot-value i 'x)`. Reader
macros have nowhere to hang a docstring, so this one is
documented in a comment.
```lisp
(set-macro-character #\$
  (lambda (s c) (declare (ignore c))
    `(slot-value i ',(read s t nil t))))
```

Count X in alist LST, starting the count at zero if new; e.g.
```lisp
(let (seen)
  (mapc (lambda (x) (incf (has x seen))) '(a a b b b))
  seen) ==> ((b . 3) (a . 2))
```
```lisp
(defmacro has (x lst)
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x 0) ,lst))))))
```

The arrow macro is the one that earns its keep most often. With
it, that counting example above collapses to a single line.

A short lambda whose args arrive as %1 to %5; e.g.
```lisp
(let (seen)
  (->> (incf (has %1 seen)) '(a a b b b))
  seen) ==> ((b . 3) (a . 2))
```
That is the `has` example above, now a one-liner.
```lisp
(defmacro -> (&body b)
  `(lambda (%1 &optional %2 %3 %4 %5)
     (declare (ignorable %2 %3 %4 %5))
     ,@b))
```

Mapcar BODY, as a short lambda, over LISTS.
```lisp
(defmacro ->> (body &rest lists)
  `(mapcar (-> ,body) ,@lists))
```

## Settings and examples

Avoid magic numbers buried in the code. Anything that steers
behaviour belongs in `*settings*`, where the command line can
reach it. Each setting is four things:

    (name flag documentation default)

so a program's options are just a list:

```lisp
(defun defaults ()
  '((seed "-s" "random number seed"   1234567891)
    (p    "-p" "distance coeffecient" 2)))
```

Everything that tunes behaviour, as (name flag doc default).
```lisp
(defvar *settings* nil)
```

The current value of setting X, e.g. `(my seed)`.
```lisp
(defmacro my (x)
  `(fourth (assoc ',x *settings*)))
```

Code should ship with demos that show it off and shout when
something breaks. Here every demo is a function named `eg--`
something, and `cli` finds them by name, so `--rand` runs
`eg--rand`. Each runs on a fresh seed, and the process exit
status is the number that failed.

Update settings B4 from the command line, then halt.
`-s 42` sets one option and `--foo` runs `(eg--foo)`.
Exit status is the number of examples that failed.
```lisp
(defun cli (b4 &aux (av (cli-args)) (bad 0))
  (setf *settings* b4)
  (loop for flag = (pop av) while flag do
    (aif (find flag b4 :key #'second :test #'equal)
      (setf (fourth it) (thing (pop av)))
      (aif (cli-eg flag)
        (setf bad (cli-run it bad))
        (format t "?? ~a~%" flag))))
  (halt bad))
```

The strings after the program name on the command line.
```lisp
(defun cli-args ()
  #+sbcl  (cdr sb-ext:*posix-argv*)
  #+clisp ext:*args*)
```

The example function named by FLAG, if one is defined.
```lisp
(defun cli-eg (flag)
  (aif (intern (format nil "EG~:@(~a~)" flag))
    (and (fboundp it) it)))
```

Call F on a fresh seed, reporting any error it raises.
Returns FAILS, incremented if F blew up.
```lisp
(defun cli-run (f &optional (fails 0))
  (setf *seed* (my seed))
  (handler-case (progn (funcall f) fails)
    (error (e)
      (format t "~&!! ~(~a~): ~a~%" f
        (substitute #\Space #\Newline (princ-to-string e)))
      (1+ fails))))
```

Coerce STR to a number, a boolean, `?`, or leave it alone.
```lisp
(defun thing (str &aux (*read-eval* nil))
  (let ((v (handler-case (read-from-string str nil :none)
             (error () :none))))
    (if (or (numberp v) (member v '(t nil ?))) v str)))
```

## Random numbers

State of the random number generator.
```lisp
(defvar *seed* 1234567891)
```

A random float in [0,N), from our own generator.
Rolling our own keeps the stream identical across platforms.
```lisp
(defun rand (&optional (n 1))
  (setf *seed* (mod (* 16807 *seed*) 2147483647))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))
```

A random integer in [0,N).
```lisp
(defun rint (&optional (n 100))
  (floor (* n (rand))))
```

A new list holding LST's items in random order.
Copies to a vector first, for fast random access.
```lisp
(defun shuffle (lst &aux (v (coerce lst 'vector)))
  (loop for i from (1- (length v)) downto 1 do
    (rotatef (aref v i) (aref v (rint (1+ i)))))
  (coerce v 'list))
```

## Odds and ends

Print KVS as key/value pairs, one pair per line.
```lisp
(defun kv (&rest kvs)
  (loop for (k v) on kvs by #'cddr do
    (format t "~&~(~a~)~10t~s~%" k v)))
```

The rows of FILE, each one a list of coerced cells.
```lisp
(defun csv (file)
  (with-open-file (s file)
    (loop for line = (read-line s nil) while line
      collect (csv-cells (string-right-trim '(#\Return) line)))))
```

Split S on SEP, coercing each cell with `thing`.
```lisp
(defun csv-cells (s &optional (sep #\,) (lo 0)
                  (hi (position sep s :start lo)))
  (cons (thing (subseq s lo hi))
        (if hi (csv-cells s sep (1+ hi)))))
```

