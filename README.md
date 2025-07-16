<img src="docs/lisp.png" align=left width=200><a 
   href="https://en.wikipedia.org/wiki/Explainable_artificial_intelligence"><img
   src="https://img.shields.io/badge/for-AI,XAI-blue?xstyle=flat&"></a>
<a href="https://gigamonkeys.com/book/introduction-why-lisp"><img
   src="https://img.shields.io/badge/uses-Lisp-purple.svg?xstyle=for-the-badge"
   alt="Language"><br></a>
<a href="https://github.com/timm/slip"><img
   src="https://img.shields.io/badge/src-code-orange.svg?xstyle=for-the-badge"
   alt="Source Code"></a>
<a href="https://github.com/timm/slip/blob/main/LICENSE.md"><img
   src="https://img.shields.io/badge/&copy;2025-MIT-brightgreen.svg?xstyle=for-the-badge"
   alt="License"></a><br clear=all>

Minimal, extensible Common Lisp script for explainable multi-objective reasoning. 
It supports incremental data analysis using symbolic and numeric columns, 
and allows structured row-wise updates with explainable decision logic.

## Features

- Defines data structures (`data`, `cols`, `num`, `sym`) for symbolic/numeric analytics.
- Handles column initialization based on header conventions (`!`, `-`, `+`, etc.).
- Supports adding and removing rows with weight adjustments.
- Provides extensible methods (`add`, `more`) for custom learning workflows.

## Usage

Run with SBCL or CLISP:

```bash
sbcl --script ezr.lisp
```
Help:

```
ezr.lisp: multi-objective explanation
(c) 2025, Tim Menzies <timm@ieee.org>, MIT license

Options:
   -k  k=2                 kth value
   -g  goal=one            start-up action
   -s  seed=1234567891     random number
   -f  file=../../moot/optimize/misc/auto93.csv data file
```
