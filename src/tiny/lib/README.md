<img align=left 
     width=450
     src="https://imgs.xkcd.com/comics/lisp_cycles.png">
'(home (©2019 ("Tim Menzies"))) <br>
'(:col (src (contrib (discuss)))<br>
&nbsp;&nbsp; :row (lib (oo (sample)))<br>
&nbsp;&nbsp;  :lib (egs (list (macros (maths (readme <br>
&nbsp;&nbsp;&nbsp;&nbsp;  ([settings](/lib/settings) (strings (structs))))))))




## [egs.lisp](egs.lisp)

|Name |Args | Doc|
|--:|--|---|
|`eg` | `(what arg doc &rest src)` |define a example |
|`demos` | `(settings all &optional one)` |Run `one` (or `all`) the demos. Reset globals between each   run.  Return to the operating systems the failure count (so   fails=0 means `successs`). |


## [lists.lisp](lists.lisp)

|Name |Args | Doc|
|--:|--|---|


## [macros.lisp](macros.lisp)

|Name |Args | Doc|
|--:|--|---|
|`!` | `(l x)` |Get into association lists. |
|`?` | `(s x &rest xs)` |(? obj x y z) == (slot-value (slot-value (slot-value obj 'x) 'y) 'z) |
|`geta` | `(x lst &optional (init 0))` |Endure lst has a slot for `x`. If missing, initialize it with `init`. |


## [maths.lisp](maths.lisp)

|Name |Args | Doc|
|--:|--|---|
|`rnd` | `(number &optional (digits 3))` |Round to `digits` decimal places. |
|`randf` | `(&optional (n 1.0))` |Random float 0.. n |
|`randi` | `(&optional (n 1))` |Random int 0..n |


## [readme.lisp](readme.lisp)

|Name |Args | Doc|
|--:|--|---|
|`reads` | `(file fun)` |For every s-expression in `file`, call `fun`. |
|`read-lines` | `(file fun)` |For every line in `file`, call `fun`. |
|`defp` | `(x)` |is this  a thing we wwant? |
|`secret` | `(x)` |is this a thing to hide? |
|`docp` | `(x)` |got doc? |
|`readme` | `(&optional (s t))` |Generate README.md from all doco strings   form all LISP code in a directory. |


## [settings.lisp](settings.lisp)

|Name |Args | Doc|
|--:|--|---|
|`cli` | `(key.flag.help.default)` |If `flag` exists on command line, update `key`. |
|`settings` | `(header options)` |Update settings. If  `help` is set, print help. |


## [strings.lisp](strings.lisp)

|Name |Args | Doc|
|--:|--|---|
|`charn` | `(x)` |Last thing from a string. |
|`trim` | `(x)` |Kill leading tailing whitespace. |
|`thing` | `(x &aux (y (trim x)))` |Turn `x` into a number or string or `?`. |
|`splits` | `(str &key (char ,) (filter #'identity))` |Divide `str` on `char`, filtering all items through `filter`. |
|`with-lines` | `(file fun)` |Call `fun` for each line in `file`. |


## [structs.lisp](structs.lisp)

|Name |Args | Doc|
|--:|--|---|
|`defstruct+` | `(x doco &body body)` |Creates %x for constructor, enables pretty print, hides slots with '_' prefix. |


## [symbols.lisp](symbols.lisp)

|Name |Args | Doc|
|--:|--|---|
