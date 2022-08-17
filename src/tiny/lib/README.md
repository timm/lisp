<img align=left 
     width=300
     src="http://bilfp.wdfiles.com/local--files/common-lisp/common-lisp-logo.png">
'(home (©2019 ("Tim Menzies")) <br>
&nbsp;&nbsp; '(([tiny](/src/tiny/tiny.lisp)
               ([eg](/src/tiny/eg.lisp))) <br>
&nbsp;&nbsp; <b>:col</b> ([cols](/src/tiny/col/etc.lisp)
                         ([num](/src/tiny/col/etc.lisp)
                         ([sample](/src/tiny/col/etc.lisp)
                         ([sym](/src/tiny/col/etc.lisp)
                         ))))<br>
&nbsp;&nbsp; <b>:row</b> ([row](/src/tiny/row/row.lisp) 
                         ([data](/src/tiny/row/data.lisp) 
                         ))<br>
&nbsp;&nbsp; <b>:lib</b> ([egs](/src/tiny/lib/etc.lisp) 
                         ([list](/src/tiny/lib/list.lisp)
                         ([macros](/src/tiny/lib/macros.lisp)
                         ([maths](/src/tiny/lib/maths.lisp)
                         ([readme](/src/tiny/lib/readme.lisp)
                         ([settings](/src/tiny/lib/settings.lisp)
                         ([strings](/src/tiny/lib/strings.lisp)
                         ([structs](/src/tiny/lib/structs.lisp)
                         ))))))))))%%
<br clear=all>




## [egs.lisp](egs.lisp)

|Name |Args | Doc|
|--:|--|:---|
|`eg` | `(what arg doc &rest src)` |define a example |
|`demos` | `(settings all &optional one)` |Run `one` (or `all`) the demos. Reset globals between each   run.  Return to the operating systems the failure count (so   fails=0 means `successs`). |


## [lists.lisp](lists.lisp)

|Name |Args | Doc|
|--:|--|:---|


## [macros.lisp](macros.lisp)

|Name |Args | Doc|
|--:|--|:---|
|`!` | `(l x)` |Get into association lists. |
|`?` | `(s x &rest xs)` |(? obj x y z) == (slot-value (slot-value (slot-value obj 'x) 'y) 'z) |
|`geta` | `(x lst &optional (init 0))` |Endure lst has a slot for `x`. If missing, initialize it with `init`. |


## [maths.lisp](maths.lisp)

|Name |Args | Doc|
|--:|--|:---|
|`rnd` | `(number &optional (digits 3))` |Round to `digits` decimal places. |
|`randf` | `(&optional (n 1.0))` |Random float 0.. n |
|`randi` | `(&optional (n 1))` |Random int 0..n |


## [readme.lisp](readme.lisp)

|Name |Args | Doc|
|--:|--|:---|
|`reads` | `(file fun)` |For every s-expression in `file`, call `fun`. |
|`read-lines` | `(file fun)` |For every line in `file`, call `fun`. |
|`defp` | `(x)` |is this  a thing we wwant? |
|`secret` | `(x)` |is this a thing to hide? |
|`docp` | `(x)` |got doc? |
|`readme` | `(&optional (s t))` |Generate README.md from all doco strings   form all LISP code in a directory. |


## [settings.lisp](settings.lisp)

|Name |Args | Doc|
|--:|--|:---|
|`cli` | `(key.flag.help.default)` |If `flag` exists on command line, update `key`. |
|`settings` | `(header options)` |Update settings. If  `help` is set, print help. |


## [strings.lisp](strings.lisp)

|Name |Args | Doc|
|--:|--|:---|
|`charn` | `(x)` |Last thing from a string. |
|`trim` | `(x)` |Kill leading tailing whitespace. |
|`thing` | `(x &aux (y (trim x)))` |Turn `x` into a number or string or `?`. |
|`splits` | `(str &key (char ,) (filter #'identity))` |Divide `str` on `char`, filtering all items through `filter`. |
|`with-lines` | `(file fun)` |Call `fun` for each line in `file`. |


## [structs.lisp](structs.lisp)

|Name |Args | Doc|
|--:|--|:---|
|`defstruct+` | `(x doco &body body)` |Creates %x for constructor, enables pretty print, hides slots with '_' prefix. |


## [symbols.lisp](symbols.lisp)

|Name |Args | Doc|
|--:|--|:---|
