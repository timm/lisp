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

<ul>   with hash table _cache, compute once, then keep
</ul>


## [macros.lisp](macros.lisp)


`? (obj first-slot &rest more-slots)`

<ul>   From https://goo.gl/dqnmvH:
</ul>


## [p.lisp](p.lisp)



## [q.lisp](q.lisp)


`? (obj first-slot &rest more-slots)`

<ul>   From https://goo.gl/dqnmvH:
</ul>


## [thing.lisp](thing.lisp)


The standard LISP object syntax is very verbose.
My `defthing` macro is a simpler way to specify an object.

E.g. here is a subclass of `thing` that has two slots
which initialize to a `gensym` and `nil`, respectively.

```lisp
(defthing keeper thing (id (gensym "kept")) (_cache))
```

Also, all my `thing`s know how to print their public slots
(which is any slot whose name does not start with `_`)


`defthing (x parent &rest slots)`

<ul>   Succinct class creation
</ul>

`print-object ((it thing) out)`

<ul>   for things, print all public slots
</ul>
