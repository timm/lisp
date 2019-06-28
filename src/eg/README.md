<a name=top></a>
[home](https://github.com/timm/lisp/blob/master/README.md#top) ::
[&copy;2019](https://github.com/timm/lisp/blob/master/LICENSE.md) ::
[Tim Menzies](http://menzies.us) 
<img width=1 height=30 src="https://github.com/timm/lisp/blob/master/etc/img/FFFFFF.png">
<a href="https://github.com/timm/lisp/blob/master/README.md#top">
<img src="https://raw.githubusercontent.com/timm/lisp/master/etc/img/gotlisp.png" ></a><br>
([src](http://github.com/timm/lisp) ::
[contrib](https://github.com/timm/lisp/blob/master/CONTRIBUTING.md) ::
[discuss](https://github.com/timm/lisp/issues))  ::
([lib](https://github.com/timm/lisp/tree/master/src/lib/README.md#top) ::
[oo](https://github.com/timm/lisp/tree/master/src/oo/README.md#top)  :: 
[rows](https://github.com/timm/lisp/tree/master/src/rows/README.md#top) )

# EG




## [col.lisp](col.lisp)



A `col`umn is either a `num`ber or a `sym`bol.
`Col`s are places to store summaries about columns
of data in a `table`



`add ((c col) x &key (filter #'identity))`

<ul>   Add numbers to column.
</ul>

`dist ((c col) x y)`

<ul>   Return a number 0 .. 1
</ul>

`add1 ((nu num) x)`

<ul>   New numbers update `min` and `max`.
</ul>

`norm ((n num) x)`

<ul>   Convert x to the range 0..1.
</ul>

`add1 ((s sym) x)`

<ul>   Increment the symbols counts
</ul>

`norm ((s sym) x)`

<ul>   Normalize symbols does nothing.
</ul>


## [data.lisp](data.lisp)



## [xy.lisp](xy.lisp)


`add ((c col) x &key (filter #'identity))`

<ul>   Add numbers to column.
</ul>

`add1 ((nu num) x)`

<ul>   New numbers update `min` and `max`.
</ul>

`norm ((n num) x)`

<ul>   Convert x to the range 0..1.
</ul>

`add1 ((s sym) x)`

<ul>   Increment the symbols counts
</ul>

`norm ((s sym) x)`

<ul>   Normalize symbols does nothing.
</ul>
