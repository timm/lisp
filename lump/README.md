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



# LUMP




-------

## [col.lisp](col.lisp)





<ul>

 Cols can be eother `num`s or `sym`s for numeric
or symbolic content (respectively).

</ul>





-------

## [got.lisp](got.lisp)



-------

## [is.lisp](is.lisp)



-------

## [k.lisp](k.lisp)



-------

## [macros.lisp](macros.lisp)


### `while (test &body body)`



<ul>

Adding a `while` loop to LISP.

</ul>



### `getr (how obj f &rest fs)`



<ul>

Recursive access to contents.

</ul>



### `? (x &rest fs)`



<ul>

Recursive access to slot instances

</ul>



### `dohash ((k v h &optional out) &body body)`



<ul>

Set key `k` and value `v` to items in hash. Returns `out`.

</ul>



### `doitems ((one pos lst &optional out) &body body)`



<ul>

Item `one` is found at `pos` in `lst`. Returns `out`.

</ul>




-------

## [my.lisp](my.lisp)


### `my (&rest fs)`



<ul>

getter for globals

</ul>




-------

## [okcol.lisp](okcol.lisp)



-------

## [oo.lisp](oo.lisp)


### `defthing (x parent &rest slots)`



<ul>

Succinct class creation

</ul>



### `print-object ((it thing) out)`



<ul>

for things, print all public slots

</ul>




-------

## [os.lisp](os.lisp)


### `klass-slots (it)`



<ul>

what are the slots of a class?

</ul>



### `klass-slot-definition-name (x)`



<ul>

what is a slot's name?

</ul>



### `args `



<ul>

what are the command line args?

</ul>



### `stop `



<ul>

how to halt the program?

</ul>



### `sh (cmd)`



<ul>

Run a shwll command

</ul>




-------

## [readmes.lisp](readmes.lisp)


### `doread ((it f &optional out &key (take #'read)) &body body)`



<ul>

Iterator for running over files or strings.

</ul>



### `readme (dir &optional (s t))`



<ul>

Generate README.md from doco strings from LISP code in a directory.

</ul>




-------

## [rows.lisp](rows.lisp)



-------

## [strings.lisp](strings.lisp)



-------

## [yes.lisp](yes.lisp)



-------

## [yes_is.lisp](yes_is.lisp)



-------

## [yes_macros.lisp](yes_macros.lisp)



-------

## [yes_my.lisp](yes_my.lisp)



-------

## [yes_rows.lisp](yes_rows.lisp)



-------

## [yes_test.lisp](yes_test.lisp)


## License

 Copyright (C) 2020 Tim Menzies <timm@ieee.org>

 Everyone is permitted to copy and distribute verbatim or modified 
 copies of this license document, and changing it is allowed as long 
 as the name is changed. 

This program is free software. It comes without any warranty, to
the extent permitted by applicable law. You can redistribute it
and/or modify it under the terms of the Do What The Fuck You Want
To Public License, Version 2, as published by Sam Hocevar. See
http://www.wtfpl.net/ for more details. 

<p align=center>
            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE 
   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION 
</p>

1. You just DO WHAT THE FUCK YOU WANT TO.


### FAQ on the License

Can’t you change the wording? It’s inappropriate / childish / not corporate-compliant?

- The WTFPL lets you relicense the work under any other license.

Can I make money with my software using the WTFPL?

- Yes.

By the way, with the WTFPL, can I also…

- Oh but yes, of course you can.

But can I…

- Yes you can.

Can…

- Yes!

