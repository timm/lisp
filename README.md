<img src="docs/lisp.png" width="200">

<a href="https://timm.github.io/lisp/ezr.html">
  <img src="https://img.shields.io/badge/docs-view%20online-9cf.svg?logo=read-the-docs&logoColor=white" alt="Docs">
</a>
<a href="https://en.wikipedia.org/wiki/Explainable_artificial_intelligence">
  <img src="https://img.shields.io/badge/for-AI%2C%20XAI-007acc.svg?logo=openai&logoColor=white" alt="For AI, XAI">
</a>
<a href="https://gigamonkeys.com/book/introduction-why-lisp">
  <img src="https://img.shields.io/badge/uses-Lisp-6e4c13.svg?logo=common-lisp&logoColor=white" alt="Language">
</a>
<a href="https://github.com/timm/slip">
  <img src="https://img.shields.io/badge/src-code-fd6e00.svg?logo=github&logoColor=white" alt="Source Code">
</a>
<a href="https://github.com/timm/lisp/blob/main/LICENSE.md">
  <img src="https://img.shields.io/badge/%C2%A92025-MIT-28a745.svg?logo=opensourceinitiative&logoColor=white" alt="License">
</a>


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
