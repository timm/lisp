

## [egs.lisp](egs.lisp)

|Name |Args | Doc|
|--:|--|---|
|`eg` | `(what arg doc &rest src)` |define a example <details><summary>xx</summary>adsaas
asdads
                    asd
                    asda</details> |
|`demos` | `(settings all &optional one)` |Run `one` (or `all`) the demos. Reset globals between each   run.  Return to the operating systems the failure count (so   fails=0 means `successs`). <details><summary>xx</summary>adsaas
asdads
                    asd
                    asda</details> |


## [lists.lisp](lists.lisp)

|Name |Args | Doc|
|--:|--|---|


## [macros.lisp](macros.lisp)

|Name |Args | Doc|
|--:|--|---|
|`!` | `(l x)` |Get into association lists. <details><summary>xx</summary>adsaas
asdads
                    asd
                    asda</details> |
|`?` | `(s x &rest xs)` |(? obj x y z) == (slot-value (slot-value (slot-value obj 'x) 'y) 'z) <details><summary>xx</summary>adsaas
asdads
                    asd
                    asda</details> |
|`geta` | `(x lst &optional (init 0))` |Endure lst has a slot for `x`. If missing, initialize it with `init`. <details><summary>xx</summary>adsaas
asdads
                    asd
                    asda</details> |


## [maths.lisp](maths.lisp)

|Name |Args | Doc|
|--:|--|---|
|`rnd` | `(number &optional (digits 3))` |Round to `digits` decimal places. <details><summary>xx</summary>adsaas
asdads
                    asd
                    asda</details> |
|`randf` | `(&optional (n 1.0))` |Random float 0.. n <details><summary>xx</summary>adsaas
asdads
                    asd
                    asda</details> |
|`randi` | `(&optional (n 1))` |Random int 0..n <details><summary>xx</summary>adsaas
asdads
                    asd
                    asda</details> |


## [readme.lisp](readme.lisp)

|Name |Args | Doc|
|--:|--|---|
|`reads` | `(file fun)` |For every s-expression in `file`, call `fun`. <details><summary>xx</summary>adsaas
asdads
                    asd
                    asda</details> |
|`defp` | `(x)` |is this  a thing we wwant? <details><summary>xx</summary>adsaas
asdads
                    asd
                    asda</details> |
|`secret` | `(x)` |is this a thing to hide? <details><summary>xx</summary>adsaas
asdads
                    asd
                    asda</details> |
|`docp` | `(x)` |got doc? <details><summary>xx</summary>adsaas
asdads
                    asd
                    asda</details> |
|`readme` | `(&optional (s t))` |Generate README.md from all doco strings   form all LISP code in a directory. <details><summary>xx</summary>adsaas
asdads
                    asd
                    asda</details> |


## [settings.lisp](settings.lisp)

|Name |Args | Doc|
|--:|--|---|
|`cli` | `(key.flag.help.default)` |If `flag` exists on command line, update `key`. <details><summary>xx</summary>adsaas
asdads
                    asd
                    asda</details> |
|`settings` | `(header options)` |Update settings. If  `help` is set, print help. <details><summary>xx</summary>adsaas
asdads
                    asd
                    asda</details> |


## [strings.lisp](strings.lisp)

|Name |Args | Doc|
|--:|--|---|
|`charn` | `(x)` |Last thing from a string. <details><summary>xx</summary>adsaas
asdads
                    asd
                    asda</details> |
|`trim` | `(x)` |Kill leading tailing whitespace. <details><summary>xx</summary>adsaas
asdads
                    asd
                    asda</details> |
|`thing` | `(x &aux (y (trim x)))` |Turn `x` into a number or string or `?`. <details><summary>xx</summary>adsaas
asdads
                    asd
                    asda</details> |
|`splits` | `(str &key (char ,) (filter #'identity))` |Divide `str` on `char`, filtering all items through `filter`. <details><summary>xx</summary>adsaas
asdads
                    asd
                    asda</details> |
|`with-lines` | `(file fun)` |Call `fun` for each line in `file`. <details><summary>xx</summary>adsaas
asdads
                    asd
                    asda</details> |


## [structs.lisp](structs.lisp)

|Name |Args | Doc|
|--:|--|---|
|`defstruct+` | `(x doco &body body)` |Creates %x for constructor, enables pretty print, hides slots with '_' prefix. <details><summary>xx</summary>adsaas
asdads
                    asd
                    asda</details> |


## [symbols.lisp](symbols.lisp)

|Name |Args | Doc|
|--:|--|---|
