
# stuff

![](docs/lisp.png)

Minimal, extensible Common Lisp script for explainable multi-objective
reasoning. Supports incremental data analysis using symbolic and numeric
columns, with structured row-wise updates and explainable decision logic.

## Features

- Defines data structures (`data`, `cols`, `num`, `sym`)
  for symbolic/numeric analytics.
- Handles column initialization based on header conventions
  (`!`, `-`, `+`, etc.).
- Supports adding and removing rows with weight adjustments.
- Provides extensible methods (`add`, `more`) for custom learning workflows.

## Usage

Run with SBCL or CLISP:

```bash
sbcl --script ezr.lisp
```
Help:

```text
ezr.lisp: multi-objective explanation
(c) 2025, Tim Menzies <timm@ieee.org>, MIT license

Options:
   -k  k=2                 kth value
   -g  goal=one            start-up action
   -s  seed=1234567891     random number
   -f  file=../../moot/optimize/misc/auto93.csv data file
```

<!-- end_slide -->

# asdas

```clojure
(defmacro ? (x &rest at)
  "Nested slot access: (? x a b)
   = (slot-value (slot-value x 'a) 'b)."
  (if at `(? (slot-value ,x ',(car at)) ,@(cdr at))
         x))
```

<!-- end_slide -->

# asdasd asdjasdjasdas

asdas
asdas

