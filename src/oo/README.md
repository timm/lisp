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
[&copy;2019](https://github.com/timm/lisp/blob/master/LICENSE.md), [Tim Menzies](http://menzies.us) 

# OO




## [keeper.lisp](keeper.lisp)


`keep (it &body body)`

-   with hash table _cache, compute once, then keep


## [macros.lisp](macros.lisp)


`? (obj first-slot &rest more-slots)`

-   From https://goo.gl/dqnmvH:


## [p.lisp](p.lisp)



## [q.lisp](q.lisp)


`? (obj first-slot &rest more-slots)`

-   From https://goo.gl/dqnmvH:


## [thing.lisp](thing.lisp)


`defslot (slot x form)`

-   helper function for defthing

`defthing (x parent &rest slots)`

-   simpler creator for class

`public-slot-names ((it thing))`

-   return all thing slots that don't start with '_'

`print-object ((it thing) out)`

-   for things, print all public slots
