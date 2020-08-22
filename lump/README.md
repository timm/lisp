

# LUMP




-------

## [col.lisp](col.lisp)



-------

## [got.lisp](got.lisp)



-------

## [is.lisp](is.lisp)



-------

## [k.lisp](k.lisp)



-------

## [macros.lisp](macros.lisp)


### `while (test &body body)`

Adding a `while` loop to LISP.


### `getr (how obj f &rest fs)`

Recursive access to contents.


### `? (x &rest fs)`

Recursive access to slot instances


### `dohash ((k v h &optional out) &body body)`

Set key `k` and value `v` to items in hash. Returns `out`.


### `doitems ((one pos lst &optional out) &body body)`

Item `one` is found at `pos` in `lst`. Returns `out`.



-------

## [my.lisp](my.lisp)


### `my (&rest fs)`

getter for globals



-------

## [okcol.lisp](okcol.lisp)



-------

## [oo.lisp](oo.lisp)


### `defthing (x parent &rest slots)`

Succinct class creation


### `print-object ((it thing) out)`

for things, print all public slots



-------

## [os.lisp](os.lisp)


### `klass-slots (it)`

what are the slots of a class?


### `klass-slot-definition-name (x)`

what is a slot's name?


### `args `

what are the command line args?


### `stop `

how to halt the program?


### `sh (cmd)`

Run a shwll command



-------

## [readmes.lisp](readmes.lisp)


### `doread ((it f &optional out &key (take #'read)) &body body)`

Iterator for running over files or strings.


### `readme (dir &optional (s t))`

Generate README.md from doco strings from LISP code in a directory.



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

