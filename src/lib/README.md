

[home](http://git.io/gotlisp) | [src](http://github.com/timm/lisp) [contrib](https://github.com/timm/lisp/blob/master/CONTRIBUTING.md) | [discuss](https://github.com/timm/lisp/issues) | [license](https://github.com/timm/lisp/blob/master/LICENSE)<br>
<a href="https://git.io/gotlisp"><img src="https://raw.githubusercontent.com/timm/lisp/master/etc/img/gotlisp.png"></a><br>
Areas: [lib](https://github.com/timm/lisp/tree/master/src/lib) | 
[oo](https://github.com/timm/lisp/tree/master/src/oo)  | 
[rows](https://github.com/timm/lisp/tree/master/src/rows)   

# LIB




## ok.lisp



## oo.lisp


`defslot (slot x form)`

-   helper function for defthing

`defthing (x parent &rest slots)`

-   simpler creator for class

`public-slot-names ((it thing))`

-   return all thing slots that don't start with '_'

`print-object ((it thing) out)`

-   print string for all public slot names


## rand.lisp



## readme.lisp




### Usage

```bash
cd src/xx
sbcl --script ../lib/readme.lisp > README.md
git add README.md
```




`readme `

-   Generate README.md from all doco strings
   form all LISP code in a directory.


## reads.lisp


`string-lines (str)`

-   Convert a string to a list of lines.

`reads (f &optional (fn #'print) (str t))`

-   Read  a file, calling 'fn' on each s-expression.


## sys.lisp


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
