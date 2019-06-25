<a name=top></a>
<a href="https://github.com/timm/lisp/blob/master/README.md#top">
<img src="https://raw.githubusercontent.com/timm/lisp/master/etc/img/gotlisp.png" ></a><br>
[home](https://github.com/timm/lisp/blob/master/README.md#top) ::
[src](http://github.com/timm/lisp) ::
[contrib](https://github.com/timm/lisp/blob/master/CONTRIBUTING.md) ::
[discuss](https://github.com/timm/lisp/issues) ::
[license](https://github.com/timm/lisp/blob/master/LICENSE.md)<br>
[lib](https://github.com/timm/lisp/tree/master/src/lib/README.md#top) :: 
[oo](https://github.com/timm/lisp/tree/master/src/oo/README.md#top)  :: 
[rows](https://github.com/timm/lisp/tree/master/src/rows/README.md#top)  

# LIB




## [macros.lisp](macros.lisp)


`aif (test then &optional else)`

-   Anaphoric 'if'

`while (test &body body)`

-   implements 'while' (which is not standard in LISP)

`until (test &body body)`

-   implements 'until' (which is not standard in LISP)


## [ok.lisp](ok.lisp)



## [oo.lisp](oo.lisp)


`defslot (slot x form)`

-   helper function for defthing

`defthing (x parent &rest slots)`

-   simpler creator for class

`public-slot-names ((it thing))`

-   return all thing slots that don't start with '_'

`print-object ((it thing) out)`

-   print string for all public slot names


## [rand.lisp](rand.lisp)



## [readme.lisp](readme.lisp)




### Usage

```bash
cd src/xx
sbcl --script ../lib/readme.lisp > README.md
git add README.md
```




`readme `

-   
Generate README.md from all doco strings 
   form all LISP code in a directory.


## [reads.lisp](reads.lisp)


`reads (f &key (act #'print) (get #'read) (str t))`

-   Read  a file, calling 'fn' on each s-expression. 

`para1 (f)`

-   Read everything up to first blank line.

`lines (x &optional (s (make-string-input-stream x)))`

-   Convert a string to a list of lines


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

-   A multi-implementation function equivalent for the C function system
