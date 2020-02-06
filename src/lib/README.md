<a name=top></a>
'([home](https://github.com/timm/lisp/blob/master/README.md#top) 
([&copy;2019](https://github.com/timm/lisp/blob/master/LICENSE.md) 
(["Tim Menzies"](http://menzies.us))))
<img width=1 height=25 src="https://github.com/timm/lisp/blob/master/etc/img/FFFFFF.png">
<a href="https://github.com/timm/lisp/blob/master/README.md#top">
<img src="https://raw.githubusercontent.com/timm/lisp/master/etc/img/gotlisp.png" ></a><br>
'(:site ([src](http://github.com/timm/lisp) 
([contrib](https://github.com/timm/lisp/blob/master/CONTRIBUTING.md)
([discuss](https://github.com/timm/lisp/issues))))      
&nbsp;&nbsp;:code ([lib](https://github.com/timm/lisp/tree/master/src/lib/README.md#top)
([oo](https://github.com/timm/lisp/tree/master/src/oo/README.md#top)
([sample](https://github.com/timm/lisp/tree/master/src/sample/README.md#top)))))

# LIB




## [hash.lisp](hash.lisp)


`do-hash ((k v h &optional out) &body body)`

<ul>   Set key 'k' and value 'v' to items in hash
</ul>

`hash-keys (h &aux out)`

<ul>   return keys in hash
</ul>


## [list.lisp](list.lisp)


`head (lst)`

<ul>   Return first item
</ul>

`tail (lst)`

<ul>   Return last item
</ul>

`l->a (lst)`

<ul>   coerce list to array
</ul>

`a->l (a &key (lo 0) (hi (length a)))`

<ul>   coerce array to list
</ul>

`select (selector-fn facts)`

<ul>   return all list items satisying selector-fn
</ul>


## [macros.lisp](macros.lisp)


`af (test then &optional else)`

<ul>   Anaphoric 'if'
</ul>

`while (test &body body)`

<ul>   implements 'while' (which is not standard in LISP)
</ul>

`whale (test &body body)`

<ul>   implements 'while' (which is not standard in LISP)
</ul>

`until (test &body body)`

<ul>   implements 'until' (which is not standard in LISP)
</ul>

`doitems ((one n list &optional out) &body body)`

<ul>   Set 'one' and 'n' to each item in a list, and its position.
</ul>


## [maths.lisp](maths.lisp)



## [ok.lisp](ok.lisp)



## [rand.lisp](rand.lisp)



The LISP random number generator does not let me
easily set the same seeds on mutilple plaforms. Hemce,
these 12 lines of code.

`reset-seed (&optional n)`

<ul>Reset seed to `n` (default = `(my :rand :seed)`</ul>

`randf (&optional (n 100))

<ul>Generate a random float in the range 0.. n. </ul>

`randi (&optional (n 1))

<ul>Generate a random integer in the range 0.. n. </ul>




## [readme.lisp](readme.lisp)



### Usage

```bash
cd src/xx
sbcl --script ../lib/readme.lisp > README.md
git add README.md
``` 



`readme (dir &optional (s t))`

<ul>   
Generate README.md from all doco strings 
  form all LISP code in a directory.
</ul>


## [reads.lisp](reads.lisp)


`doread ((it f &optional out &key (take #'read)) &body body)`

<ul>   Iterator for running over files or strings.
</ul>

`para1 (f)`

<ul>   Read everything up to first blank line.
</ul>

`s->lines (x &optional (s (make-string-input-stream x)))`

<ul>   Convert a string to a list of lines
</ul>


## [sys.lisp](sys.lisp)


`klass-slots (it)`

<ul>   what are the slots of a class?
</ul>

`klass-slot-definition-name (x)`

<ul>   what is a slot's name?
</ul>

`args `

<ul>   what are the command line args?
</ul>

`stop `

<ul>   how to halt the program?
</ul>

`sh (cmd)`

<ul>   Run a shwll command
</ul>
