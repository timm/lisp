<a name=top></a>
<a href="https://github.com/timm/lisp/blob/master/README.md#top">
<img src="https://raw.githubusercontent.com/timm/lisp/master/etc/img/gotlisp.png" ></a><br>
[home](https://github.com/timm/lisp/blob/master/README.md#top) ::
[src](http://github.com/timm/lisp) ::
[contrib](https://github.com/timm/lisp/blob/master/CONTRIBUTING.md) ::
[discuss](https://github.com/timm/lisp/issues) ::
[lib](https://github.com/timm/lisp/tree/master/src/lib/README.md#top) :: 
[oo](https://github.com/timm/lisp/tree/master/src/oo/README.md#top)  :: 
[rows](https://github.com/timm/lisp/tree/master/src/rows/README.md#top) ::
[@copy; 2019](https://github.com/timm/lisp/blob/master/LICENSE.md) [Tim Menzies](http://menzies.us) 

# LIB




## [hash.lisp](hash.lisp)


`do-hash ((k v h &optional out) &body body)`

-   Set key 'k' and value 'v' to items in hash

`hash-keys (h &aux out)`

-   return keys in hash


## [macros.lisp](macros.lisp)


`af (test then &optional else)`

-   Anaphoric 'if'

`while (test &body body)`

-   implements 'while' (which is not standard in LISP)

`whale (test &body body)`

-   implements 'while' (which is not standard in LISP)

`until (test &body body)`

-   implements 'until' (which is not standard in LISP)


## [ok.lisp](ok.lisp)



## [rand.lisp](rand.lisp)



## [readme.lisp](readme.lisp)




### Usage

```bash
cd src/xx
sbcl --script ../lib/readme.lisp > README.md
git add README.md
```




`readme (&optional (s t))`

-   
Generate README.md from all doco strings 
  form all LISP code in a directory.


## [reads.lisp](reads.lisp)


`doread ((it f &optional out &key (take #'read)) &body body)`

-   Iterator for running over files or strings.

`para1 (f)`

-   Read everything up to first blank line.

`s->lines (x &optional (s (make-string-input-stream x)))`

-   Convert a string to a list of lines

`s->words 
(s &optional
 (sep
  '(,   	
    )))`

-   Convert a string to a list of words


## [sys.lisp](sys.lisp)


`klass-slots (it)`

-   what are the slots of a class?

`klass-slot-definition-name (x)`

-   what is a slot's name?

`args `

-   what are the command line args?

`stop `

-   how to halt the program?

`sh (cmd)`

-   Run a shwll command
