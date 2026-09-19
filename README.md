# lisp

`lib.lisp`: cool Common Lisp tricks in one file.
Reader macros (`$slot`, `@option`), anaphora (`if+`), short lambdas
(`fn`, `->`), `glu` (defstruct + methods), `for+` comprehensions,
`let+` bindings, seeded random, CSV reading, and a tiny CLI
that dispatches `--foo` to `eg--foo`.

```lisp
(load "lib")
(defun eg--hello (&optional _) (print (for+ (* x x) for x in '(1 2 3))))
(cli *the*)          ; sbcl --script app.lisp -s 7 --hello 0
```

`make` lists targets; `make sh`, `make pull`, `make push MSG=...`.

MIT license. (c) 2026 Tim Menzies, timm@ieee.org
