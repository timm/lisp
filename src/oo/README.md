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

# OO




## [keeper.lisp](keeper.lisp)



`Keeper`s are subclasses of `thing`s that,
optionally, now how to cache the results of a method
call (so if that method is called N times, we only
compute it once). 

The cache is maintain within the `_cache` variable.



`defkept (m a &body b)`

<ul>   
Define a method that will cache its result,
  returning the same result if called multiple times
</ul>


## [macros.lisp](macros.lisp)


`? (obj first-slot &rest more-slots)`

<ul>   
Easy reference to LISP slots; e.g. (? obj 'a 'b c) expands
  to `(slot-value slot-value (slot-value obj 'a) 'b) c)`.
  Built using advice from https://goo.gl/dqnmvH.
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
