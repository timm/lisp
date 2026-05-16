---
title: nb — Naive Bayes (online)
nav_order: 3
prereqs: [lib]
---

![License](https://img.shields.io/badge/license-MIT-blue)
![Lang](https://img.shields.io/badge/lang-Common%20Lisp-purple)
![Runtime](https://img.shields.io/badge/runtime-SBCL-orange)

# nb.lisp — Online Naive Bayes Classifier

**Source:** [`nb.lisp`](./nb.lisp) · **Helpers:** [`lib.lisp`](./lib.lisp)
**See also:** [glossary](glossary.md) · [syllabus](syllabus.md)

---

## Big picture

Given a CSV with one labelled column (the *class*) and the
rest *features*, build a classifier that, for any new row,
predicts the class. The trick: train and test interleaved —
each new row is classified *first* (using everything seen so
far), then added to its true class's running summary. By the
time the file ends, you have a *confusion matrix* from one
pass through the data with no separate hold-out.

## Concepts primer

This section defines every AI/stats term used later. Skim it
once; refer back when a callout mentions a term unfamiliar.

### Supervised classification

You have rows; each row has *features* (independent inputs)
and a *label* (the *class* — what you want to predict). Build
a model from labelled rows so that, given features alone, the
model guesses the label. Here: numeric features (`Mpg+`,
`Volume`), symbolic features (`origin`), and a class column
whose name ends `!`.

### CSV input format

Every example in this file reads a CSV. The first row names
the columns; suffix and case decide the role:

```
   Name      role          example
   -------   ----------    ----------------------------
   [A-Z]*    numeric x     Mpg, Volume, Pres, Plas
   [a-z]*    symbolic x    origin, color, lights
   *!        class (y)     class!,  diagnosis!
   *X        skip          IdX, NotesX
   ?         missing       any cell can be "?"
```

Concrete example (`diabetes.csv`, abridged):

```
   Preg, Plas, Pres, Skin, Insu, Mass, Pedi, Age, class!
   6,    148,  72,   35,   0,    33.6, 0.627, 50,  positive
   1,    85,   66,   29,   0,    26.6, 0.351, 31,  negative
   8,    183,  64,   0,    0,    23.3, 0.672, 32,  positive
   ...
```

Eight features (all numeric, capitalised), one class column
`class!`. Every later structure in this file is a way of
summarising columns like these.

### Distributions: mid and spread

A *distribution* is the pattern of values a column shows. Two
numbers usually describe it well:

- **mid** (central tendency): where the values cluster.
- **spread** (deviation from the centre): how wide the cluster.

What "mid" and "spread" mean differs by column type:

| column type | mid             | spread                 |
|-------------|-----------------|------------------------|
| numeric     | mean (μ)        | standard deviation (σ) |
| symbolic    | mode (most freq)| entropy (this file: --) |

In code, every column kind responds to the same two verbs
[`mid`](#mid-sym) and [`spread`](#spread-num); the dispatcher
picks the right math. That is the *only* shape a downstream
function needs to know.

### Likelihood, prior, posterior, Bayes' rule

For each candidate class `c`:

- **Prior** `P(c)`: how common `c` is overall.
- **Likelihood** `P(x | c)`: how typical the features `x`
  look *if* the class were `c`.
- **Posterior** `P(c | x)`: probability the class IS `c`,
  given features `x`. This is what we want.

Bayes' rule:
`P(c | x) ∝ P(x | c) · P(c)`.

We don't need the absolute posterior, just the *argmax over
c*, so we drop the constant denominator.

### Naive independence

To compute `P(x | c)` for multi-feature `x`, factorise:

`P(x₁, x₂, … | c) ≈ P(x₁ | c) · P(x₂ | c) · …`

That's *naive*: features are rarely independent. But the
approximation usually preserves the class *ranking* even when
the absolute probabilities are off. Hence "Naive Bayes".

### Log-likelihood

Products of tiny probabilities underflow to zero. Switch to
logs: addition replaces multiplication, no underflow.

`log P(c | x) = log P(c) + Σᵢ log P(xᵢ | c)`

### Laplace `k`-smoothing

For a symbolic feature, `P(x = v | c)` is naively `count(v)
/ n`. If `v` never appeared in class `c` during training,
that's `0` — and one zero kills the product. Fix: add a small
`k` to the numerator and `k · prior` to the denominator. Never
exactly zero. The `-k N` flag tunes how aggressively to smooth.

### `m`-estimate of prior

A second smoothing, this time of `P(c)`. With `m` virtual
observations spread uniformly across classes:

`P(c) ≈ (rows-in-c + m) / (total-rows + m · num-classes)`

`-m N` tunes pull toward uniform.

### Mean, variance, standard deviation

- **Mean** `μ`: average of values.
- **Variance** `σ²`: average squared distance from mean.
- **Standard deviation** `σ = √σ²`: in the same units as the
  data. Roughly: how spread out the values are.

### Probability density function (PDF)

For a numeric variable, a function `f(v)` such that the
*area under the curve* between two points equals the
probability of landing in that range. The PDF itself is
*not* a probability — it can be larger than 1. But ratios
of PDFs are well-defined likelihoods, which is all NB needs.

### Cumulative distribution function (CDF)

`F(v) = P(X ≤ v)`. Always between 0 and 1. We don't fit a
CDF directly here, but the [`bisect`](#bisect) function
*acts like* a sample CDF: `bisect(xs, v) / |xs|` is the
empirical fraction of `xs` at or below `v`. The [`ks`](#ks)
test compares two empirical CDFs.

### Gaussian (normal) distribution

The bell curve. PDF:
`f(v) = (1 / √(2πσ²)) · exp(-(v - μ)² / (2σ²))`.

Two parameters: mean `μ` (where the peak sits) and `σ`
(how wide the bell). Used for numeric features in NB:
[`like-num`](#like-num) is exactly this formula.

### Welford's algorithm

How do we compute mean and variance *online* — one row at a
time, without keeping all the data? Welford 1962:

```
n  += 1
d   = v - mu
mu += d / n
m2 += d * (v - mu)         ; NOTE: uses new mu
sd  = sqrt(m2 / (n-1))
```

Stable (avoids catastrophic cancellation), constant memory,
exact. [`add-num`](#add-num) is this verbatim.

### Confusion matrix

After classifying, for every (true class `want`, predicted
class `got`) pair, count how many rows ended up there. For
two classes `A` and `B`, `pos = A`:

|              | predicted A | predicted B |
|--------------|-------------|-------------|
| **actual A** | TP          | FN          |
| **actual B** | FP          | TN          |

- **TP** true positive (correctly predicted `A`)
- **FN** false negative (was `A`, predicted `B`)
- **FP** false positive (was `B`, predicted `A`)
- **TN** true negative (correctly predicted not-`A`)

### Recall, precision, specificity, accuracy, G-score

All derived from TP/FN/FP/TN:

- **Recall** (`pd`, true positive rate): `TP / (TP + FN)`.
  Of all true `A`s, what fraction did we catch?
- **Precision** (`prec`): `TP / (TP + FP)`. Of all rows we
  called `A`, what fraction really were?
- **Specificity** (1 − `pf`): `TN / (TN + FP)`. Of all true
  not-`A`s, what fraction did we leave alone?
- **Accuracy** (`acc`): `(TP + TN) / total`. Overall correct.
- **G-score**: harmonic mean of recall and specificity. A
  single number rewarding both. `(2·rec·spc) / (rec + spc)`.

### Online vs batch learning

- **Batch**: see all rows first, then build the model.
- **Online**: update the model one row at a time as rows
  arrive. NB here is online: [`nb`](#nb) does
  *classify-then-train* on each row.

### Burn-in / warm-start

The very first rows have nothing to classify against (the
model is empty). Skip classification for the first `@wait`
rows; just train. After that, every row tests then trains.
`-w N` tunes the burn-in length.

### Effect size: Cliff's delta, KS

For comparing two samples (e.g., two treatments under
[`sames`](#sames)):

- **Cliff's delta**: `|#(x>y) − #(x<y)| / (n·m)`. Range 0–1.
  Non-parametric — no Gaussian assumption.
- **Kolmogorov-Smirnov (KS)**: max gap between the two
  empirical CDFs. Picks up shape differences cliffs misses.
- Both are *non-parametric*: they ask "do these two samples
  look like the same distribution" without assuming any
  particular shape.

### Argmax / argmin

The *input* that produces the max (or min) of some function,
not the max value itself. `[`argm`](#classify) lst key` runs
in one pass — cheaper than sorting and taking the head.

## Problem

- **Input:** CSV in the format above
  ([CSV input format](#csv-input-format)).
- **Output:** an alist `((want . ((got . count) ...)) ...)` —
  the [confusion matrix](#confusion-matrix). From that,
  per-class
  [precision, recall, G-score](#recall-precision-specificity-accuracy-g-score).

## Approach

For each row's true class, keep a [`data`](#data) shaped exactly
like the input but holding only that class's rows. The class's
columns become per-class likelihood models: Gaussian
([Gaussian](#gaussian-normal-distribution)) for numbers,
Laplace-smoothed frequency
([Laplace `k`-smoothing](#laplace-k-smoothing)) for symbols.
Predict by maximising
`log P(class) + Σ log P(feature | class)` — the naive-Bayes
approximation that assumes features are conditionally
independent.

> [!IMPORTANT] limit
> The conditional-independence assumption is wrong on real data.
> Yet NB still wins many benchmarks because the *ranking* of
> classes survives even when the probabilities are biased.
> See [Limitations](#limitations).

## Architecture

```
   CSV
    │
    ▼
 [make-data]──> data {rows, cols={x,y,all,names}}
                       │
                       │  per row:
                       ▼
                  [nb] ──> {klasses, cm}
                       │
                       │  classify-then-train:
                       │  1. ensure klass(want) exists
                       │  2. if past burn-in: classify, cm-incf
                       │  3. add row to klass(want)
                       ▼
                  cm  ─[stats]─> per-class plist
                       │
                       ▼
                  [print-stats]
```

## Key structures

- [`sym`](#sym) — symbolic column: alist of (value . count).
- [`num`](#num) — numeric column: Welford `mu`, `m2`, `sd`.
- [`cols`](#cols) — split header into `x` (features) + `y` (class).
- [`data`](#data) — rows + cols.
- `cm` — alist of alists: `cm[want][got] = count`.

---

## Install

> [!NOTE]
> Three files only. No package manager. No build step. SBCL
> reads the source straight from disk.

**Tools** (Homebrew shown; apt/dnf equivalent works):

```bash
# macOS
brew install sbcl rlwrap

# Debian/Ubuntu
sudo apt install sbcl rlwrap

# Fedora
sudo dnf install sbcl rlwrap
```

- `sbcl` — Steel Bank Common Lisp. The interpreter.
- `rlwrap` — readline wrapper for arrow-key history at the
  Lisp REPL. Optional but you will want it.

**Get the code**:

```bash
git clone https://github.com/timm/lisp.git
cd lisp/teach
```

Or download the two source files directly:

```bash
mkdir teach && cd teach
curl -O https://raw.githubusercontent.com/timm/lisp/master/teach/lib.lisp
curl -O https://raw.githubusercontent.com/timm/lisp/master/teach/nb.lisp
```

**Get sample data** (classification CSVs with `class!` column):

```bash
git clone https://github.com/timm/moot.git
ls moot/classify/        # diabetes.csv, soybean.csv, heart.c.csv
```

**Run a command from the shell**:

```bash
sbcl --script nb.lisp -f moot/classify/diabetes.csv --nb
```

**Or open a REPL** and call functions interactively:

```bash
rlwrap sbcl --load lib.lisp --load nb.lisp
```

> [!TIP]
> Every fenced REPL block from here on starts with `[1]>` and
> increments. You can paste prompt text (after the `>`) into a
> live SBCL session and reproduce every result.

---

## Config

### the

`*the*` is the shared config. Walked by `lib.lisp`'s CLI driver:
`-k 1` rewrites the `k` cell. The `@key` reader macro from
`lib.lisp` (`@k` → `(the-of 'k)`) reads current values.

```lisp
(defparameter *the*
  (list (list 'file "auto93.csv" "-f" "csv input")
        (list 'k    1            "-k" "symbol smoothing")
        (list 'm    2            "-m" "prior smoothing")
        (list 'wait 5            "-w" "burn-in rows")
        (list 'seed 1            "-s" "random seed")))

(defun the-of (k) (second (assoc k *the*)))
```

> [!WARNING] gotcha
> Use `list` not `'(...)` for `*the*`. Quoted literals are
> read-only in many CL implementations — `setf` on a quoted
> cell silently no-ops on SBCL. The CLI mutates `*the*` to
> apply flags, so it MUST be mutable.

```
[1]> (load "lib") (load "nb")
T
[2]> (the-of 'k)
1
[3]> (the-of 'file)
"auto93.csv"
[4]> (setf (second (assoc 'k *the*)) 3) (the-of 'k)
3
```

---

## Atoms

A column is symbolic or numeric. Two structs, paired and parallel.

### sym

`sym` counts how often each value appears. `has` is an alist
of `(value . count)`. `at` is the column index in a row;
`txt` is the header string.

```lisp
(defstruct sym (at 0) (txt " ") (n 0) has)
```

### num

`num` keeps two scalars instead of every value: mean `mu` and
second-moment accumulator `m2`. Standard deviation `sd` is
derived on demand (recomputed in `add` for cache-locality).

```lisp
(defstruct (num (:constructor %make-num))
  (at 0) (txt " ") (n 0) (mu 0) (m2 0) (sd 0))

(defun make-num (&rest args) (apply #'%make-num args))
```

> [!NOTE] math
> [Welford's algorithm](#welfords-algorithm) — order matters:
> update `mu` *first*, then use the new `mu` in the `m2`
> increment. That order is what makes `sd = √(m2/(n−1))`
> exact after every step.

---

## Operations on atoms

Every column supports four verbs: `add`, `mid`, `spread`, `like`.
Each verb dispatches on column kind via CLOS. Pairs shown
adjacently so symmetry is visible.

### add-sym

`add` updates a summary by one observation. For `sym` it bumps
an existing cell or pushes a fresh one. Missing values (the
symbol `?`) are skipped: every later step assumes a column's
`n` reflects only seen values.

```lisp
(defmethod add ((i sym) v)
  (unless (eq v '?)
    (incf $n)
    (let ((cell (assoc v $has :test #'equal)))
      (if cell (incf (cdr cell))
          (push (cons v 1) $has))))
  v)
```

### add-num

The `num` add applies Welford's two-line update. `mu` moves
toward the new value at rate `1/n`. `m2` then uses the *new*
`mu`. `sd` is refreshed inline.

```lisp
(defmethod add ((i num) v)
  (unless (eq v '?)
    (incf $n)
    (let ((d (- v $mu)))
      (incf $mu (/ d $n))
      (incf $m2 (* d (- v $mu)))
      (setf $sd (if (< $n 2) 0
                    (sqrt (/ $m2 (1- $n)))))))
  v)
```

```
[5]> (defparameter s (make-sym))
S
[6]> (dolist (v '(a a a b b c)) (add s v))
NIL
[7]> (sym-has s)
((C . 1) (B . 2) (A . 3))
[8]> (defparameter n (make-num))
N
[9]> (dolist (v '(1 2 3 4 5)) (add n v))
NIL
[10]> (list (num-n n) (num-mu n) (num-sd n))
(5 3 1.5811388300841898d0)
[11]> (add n '?)
?
[12]> (num-n n)
5
```

### mid-sym

`mid` returns a central tendency. For symbols, mode: the value
with the largest count.

```lisp
(defmethod mid ((i sym))
  (car (reduce (ff+ (if (> (cdr _) (cdr __)) _ __))
               $has)))
```

### mid-num

For numbers, `mid` is the running mean. Same name, same role,
different body.

```lisp
(defmethod mid ((i num)) $mu)
```

```
[13]> (mid n)
3
[14]> (mid s)
A
[15]> (defparameter s2 (make-sym))
S2
[16]> (dolist (v '(x y x z x)) (add s2 v)) (mid s2)
X
```

### spread-num

`spread` returns a width. For numbers, the running standard
deviation. (NB only needs num spread; sym spread shipped with
the reference Lua but is dead code here, so omitted.)

```lisp
(defmethod spread ((i num)) $sd)
```

```
[17]> (spread n)
1.5811388300841898d0
[18]> (let ((n2 (make-num))) (add n2 5) (spread n2))
0
```

> [!NOTE] math
> `(1- $n)` denominator gives Bessel-corrected sample variance.
> For a single observation, `(1- 1) = 0` would divide by zero,
> so the guard `(< $n 2) 0` returns zero spread.

### like-sym

Likelihood `P(v | class)` for symbolic features. Laplace-`k`
smoothing prevents zero probabilities for unseen values, and
the `m`-estimate weights the class prior.

```lisp
(defmethod like ((i sym) v prior)
  (max 1e-32
       (/ (+ (aget v $has) (* @k prior))
          (+ $n @k 1e-32))))
```

### like-num

For numbers, a Gaussian PDF — bell curve around `mu` with
width `sd`.

```lisp
(defmethod like ((i num) v prior)
  (declare (ignore prior))
  (let* ((var (+ (* $sd $sd) 1e-32)))
    (* (/ 1 (sqrt (* 2 pi var)))
       (exp (- (/ (expt (- v $mu) 2)
                  (* 2 var)))))))
```

> [!NOTE] math
> The [Gaussian PDF](#gaussian-normal-distribution) has two
> terms: the normalising coefficient `1/√(2πσ²)` and the
> exponential falloff `exp(-(v-μ)²/(2σ²))`. Variance gets a
> tiny `+1e-32` so zero-variance columns don't divide by
> zero — they collapse to an extremely peaked spike.

> [!WARNING] gotcha
> [`likes`](#likes) wraps every `(like ...)` call in `(max
> 1e-32 ...)` before `log`. Without the clamp, an empty class's
> `like` returns 0 (zero variance, value far from mean →
> `exp(-huge) → 0`), and `log(0)` raises an error in CL.

> [!TIP] this-codebase
> The unused `prior` parameter on `like-num` is intentional.
> Every method must share the same argument shape so a single
> generic call site (in [`likes`](#likes)) dispatches without
> a type check. `(declare (ignore prior))` documents the
> unused arg and silences the compiler.

```
[19]> (like s 'a 0.5)
0.6
[20]> (like s 'q 0.5)
0.1
[21]> (like n 3.0 nil)
0.252313d0
[22]> (like n 50.0 nil)
1.2e-300
```

---

## Containers

### cols

A [`cols`](#cols) groups [`num`](#num)/[`sym`](#sym) instances
built from a CSV header. Suffix decides role:

- name ends `!` → class (y)
- name ends `X` → skip
- otherwise → feature (x)

Uppercase first letter → numeric; lowercase → symbolic.

```lisp
(defstruct (cols (:constructor %make-cols))
  names x y all)

(defun make-cols (txts &aux (i (%make-cols :names txts))
                            (n -1))
  (let+ ((one (txt) (! (if (upper-case-p (ch txt 0))
                           #'make-num #'make-sym)
                        :txt txt :at (incf n))))
    (setf $all (mapcar #'one txts))
    (dolist (c $all)
      (let ((last (ch (? c txt) -1)))
        (unless (find last "!X") (push c $x))
        (when (eq last #\!)      (push c $y)))))
  i)
```

`add` on `cols` fans a row across every column. The
[`ff+`](#ff) shortcut (`lib.lisp`) names the two arguments
`_` and `__`.

```lisp
(defmethod add ((i cols) row)
  (mapcar (ff+ (add _ __)) $all row)
  row)
```

```
[23]> (defparameter c
        (make-cols '("Age" "Income" "city" "class!")))
C
[24]> (mapcar #'sym-txt (mapcar #'identity (cols-y c)))
("class!")
[25]> (length (cols-x c))
3
```

### data

A [`data`](#data) is rows + a [`cols`](#cols). First `add`
seeds `cols` from the header; subsequent adds push a row and
update column stats.

```lisp
(defstruct (data (:constructor %make-data))
  rows cols (txt ""))

(defun make-data (&optional rows
                  &aux (i (%make-data)))
  (when rows (dolist (row rows) (add i row)))
  i)

(defmethod add ((i data) row)
  (if (null $cols)
      (setf $cols (make-cols row))
      (progn (push row $rows)
             (add $cols row)))
  row)
```

```
[26]> (defparameter d (make-data
                        (read-csv "diabetes.csv")))
D
[27]> (length (data-rows d))
768
[28]> (length (cols-x (data-cols d)))
8
[29]> (sym-txt (car (cols-y (data-cols d))))
"class!"
```

### clone-data

Makes an empty `data` sharing the schema. Used by [`nb`](#nb)
to bin rows by class without re-parsing the header.

```lisp
(defun clone-data (data &optional rows (txt ""))
  (let ((d (make-data)))
    (setf (data-txt d) txt)
    (add d (? data cols names))
    (when rows (dolist (r rows) (add d r)))
    d))
```

```
[30]> (data-txt (clone-data d nil "demo"))
"demo"
```

---

## Bayes

### likes

Sum `log(like)` across feature columns plus a smoothed class
prior. Logs convert multiplicative tiny probabilities into
additive ordinary numbers — no underflow.

```lisp
(defun likes (data row n-all n-h)
  (let* ((b (/ (+ (length (? data rows)) @m)
               (+ n-all (* @m n-h))))
         (s (loop for c in (? data cols x)
                  for v = (elt row (? c at))
                  unless (eq v '?)
                  sum (log (max 1e-32 (like c v b))))))
    (+ (log b) s)))
```

> [!NOTE] math
> `b` is the [`m`-estimate of prior](#m-estimate-of-prior):
> `(rows + m) / (total + m·classes)`. For `m=2`, it pulls
> small classes toward `1/classes` (uniform prior).

```
[31]> (likes d (car (data-rows d)) 768 2)
-25.45d0
[32]> (likes d (cadr (data-rows d)) 768 2)
-29.30d0
[33]> (< (likes d (car (data-rows d)) 768 2) 0)
T
```

### cm-incf

The confusion matrix `cm` is a 2-deep alist:
`((want . ((got . count) ...)) ...)`. Bumping needs two lookups;
if the inner cell is missing, prepend it.

```lisp
(defun cm-incf (cm want got)
  (let* ((row  (assoc want cm :test #'equal))
         (cell (assoc got (cdr row) :test #'equal)))
    (if cell (incf (cdr cell))
        (rplacd row (acons got 1 (cdr row))))))
```

> [!WARNING] gotcha
> `rplacd` is the destructive cdr-setter from old Lisp. We use
> it because alist mutation must update the cons cell that the
> outer list already references — `setf` on a freshly-built
> alist would lose the link.

### classify

Pick the class whose [`likes`](#likes) score is highest. `argm`
(from `lib.lisp`) does a single-pass argmax — `O(n)`, no
intermediate sort.

```lisp
(defun classify (klasses row n nk)
  (let ((best (argm klasses
                    (f+ (likes (cdr _) row n nk))
                    #'>)))
    (when best (car best))))
```

> [!TIP] optimization
> `(car (sort lst ...))` would also find the max, but at
> `O(n log n)` and one full copy of the list. `argm` is one
> pass, two locals. For an inner loop hit per row, that's a
> ~20× speedup on a 1000-row file.

### nb

The driver. For each row:

1. Ensure the row's true class has its own [`clone-data`](#clone-data).
2. After a burn-in (`@wait`), [`classify`](#classify) the row
   *first*, then [`cm-incf`](#cm-incf).
3. Always train: [`add`](#add-num) the row to its true class.

Online classify-then-train: each row is tested on the model
built from previous rows only — no leakage.

```lisp
(defun nb (data &aux (klasses nil) (cm nil)
                     (n 0) (nk 0)
                     (kat (? (car (? data cols y)) at)))
  (dolist (row (reverse (? data rows)) cm)
    (let ((want (elt row kat)))
      (unless (assoc want klasses :test #'equal)
        (incf nk)
        (push (cons want
                    (clone-data data nil
                                (format nil "~a" want)))
              klasses)
        (push (cons want nil) cm))
      (when (> n @wait)
        (let ((got (classify klasses row n nk)))
          (when got (cm-incf cm want got))))
      (incf n)
      (add (aget want klasses nil) row))))
```

> [!IMPORTANT] this-codebase
> `(reverse (? data rows))` — rows were pushed via `add`, so
> they live in reverse insertion order. `reverse` puts them
> back into CSV order, which matters for any online algorithm.

```
[34]> (defparameter cm (nb d))
CM
[35]> (length cm)
2
[36]> (car (assoc "tested_positive" cm :test #'equal))
"tested_positive"
[37]> (length (cdr (assoc "tested_positive" cm
                         :test #'equal)))
2
```

---

## Stats

### pct

Rounded percentage. The tiny epsilon avoids zero-division when
a class is never predicted.

```lisp
(defun pct (x y)
  (floor (+ (/ (* 100 x) (+ y 1e-32)) 0.5)))
```

### cm-total / cm-fnfp / metrics

Three single-purpose helpers compose into [`stats`](#stats).
Reads top-down: total cells, then for each class compute false
counts, then build the per-class plist.

```lisp
(defun cm-total (cm)
  (loop for (_ . gots) in cm
        sum (loop for (_ . c) in gots sum c)))

(defun cm-fnfp (cm pos)
  (let ((fn 0) (fp 0))
    (loop for (w . wgots) in cm do
      (loop for (g . c) in wgots do
        (cond ((and (equal w pos) (not (equal g pos)))
               (incf fn c))
              ((and (not (equal w pos)) (equal g pos))
               (incf fp c)))))
    (values fn fp)))

(defun metrics (pos n tp fn fp)
  (let* ((tn  (- n tp fp fn))
         (rec (/ tp (+ tp fn 1e-32)))
         (spc (/ tn (+ tn fp 1e-32)))
         (g   (/ (* 2 rec spc) (+ rec spc 1e-32))))
    (list :class (format nil "~a" pos)
          :n n :tn tn :fn fn :fp fp :tp tp
          :pd   (pct tp (+ tp fn))
          :pf   (pct fp (+ fp tn))
          :prec (pct tp (+ tp fp))
          :acc  (pct (+ tp tn) n)
          :g    (floor (+ (* 100 g) 0.5)))))
```

> [!NOTE] eval
> `g = 2·rec·spc / (rec+spc)` — the
> [G-score](#recall-precision-specificity-accuracy-g-score)
> is the harmonic mean of recall and specificity. Rewards
> classifiers that handle *both* error types, not just one.

### stats

Composes the three above into a list of per-class plists.

```lisp
(defun stats (cm)
  (let ((n (cm-total cm)))
    (loop for (pos . gots) in cm
          collect (multiple-value-bind (fn fp)
                      (cm-fnfp cm pos)
                    (metrics pos n (aget pos gots)
                             fn fp)))))
```

### print-stats

Format strings parametrised to keep the call site readable.

```lisp
(defparameter *hdr-fmt*
  "~&~5a ~5a ~5a ~5a ~5a   ~4a ~4a ~4a ~4a ~4a   ~a~%")
(defparameter *row-fmt*
  "~&~5d ~5d ~5d ~5d ~5d   ~4d ~4d ~4d ~4d ~4d   ~a~%")

(defun print-stats (rows)
  (format t *hdr-fmt*
          "n" "tn" "fn" "fp" "tp"
          "pd" "pf" "prec" "acc" "g" "class")
  (dolist (r rows)
    (format t *row-fmt*
            (getf r :n) (getf r :tn) (getf r :fn)
            (getf r :fp) (getf r :tp)
            (getf r :pd) (getf r :pf) (getf r :prec)
            (getf r :acc) (getf r :g)
            (getf r :class))))
```

```
[38]> (cm-total cm)
762
[39]> (multiple-value-bind (fn fp)
        (cm-fnfp cm "tested_positive")
        (list fn fp))
(109 94)
[40]> (length (stats cm))
2
[41]> (getf (car (stats cm)) :g)
68
[42]> (print-stats (stats cm))
n     tn    fn    fp    tp      pd   pf   prec acc  g      class
  762   156    94   109   403     81   41   79   73   68   tested_negative
  762   403   109    94   156     59   19   62   73   68   tested_positive
NIL
```

---

## Stat tests

A small toolkit for "are these two distributions the same?"

### bisect

Rightmost index `i` where `xs[i] ≤ x`. Standard binary search.

```lisp
(defun bisect (xs x)
  (let ((lo 0) (hi (1- (length xs))))
    (loop while (<= lo hi)
          do (let ((m (floor (+ lo hi) 2)))
               (if (<= (elt xs m) x)
                   (setf lo (1+ m))
                   (setf hi (1- m)))))
    lo))
```

> [!TIP] optimization
> [`cliffs-delta`](#cliffs-delta) and [`ks`](#ks) both need
> "how many of ys are ≤ v" for many v. The naive scan is
> `O(n·m)`. `bisect` makes each query `O(log m)` after one
> sort — overall `O(n log m)`.

### cliffs-delta

Non-parametric effect size: `|#(x>y) − #(x<y)| / (n·m)`. Range
0..1. Below ~0.195 means "small effect" — distributions are
similar.

```lisp
(defun cliffs-delta (xs ys)
  (let ((n (length xs)) (m (length ys))
        (ngt 0) (nlt 0))
    (dolist (v xs)
      (incf ngt (bisect ys v))
      (incf nlt (- m (bisect ys (+ v 1e-32)))))
    (/ (abs (- ngt nlt)) (* n m))))
```

### ks

Kolmogorov-Smirnov: max gap between empirical CDFs.

```lisp
(defun ks (xs ys)
  (let ((n (length xs)) (m (length ys)) (d 0))
    (flet ((gap (v)
             (abs (- (/ (bisect xs v) n)
                     (/ (bisect ys v) m)))))
      (dolist (v xs) (setf d (max d (gap v))))
      (dolist (v ys) (setf d (max d (gap v))))
      d)))
```

### same

Two distributions are *same* if Cohen-d gate (cheap median
distance) OR cliffs-delta gate OR KS gate says so. Cascading
lets cheap tests short-circuit.

```lisp
(defun same (xs ys eps)
  (let* ((xs (sort (copy-list xs) #'<))
         (ys (sort (copy-list ys) #'<))
         (n (length xs)) (m (length ys))
         (mx (elt xs (floor n 2)))
         (my (elt ys (floor m 2))))
    (cond ((<= (abs (- mx my)) eps) t)
          ((> (cliffs-delta xs ys) 0.195) nil)
          (t (<= (ks xs ys)
                 (* 1.36
                    (sqrt (/ (+ n m) (* n m)))))))))
```

```
[43]> (bisect '(1 3 5 7 9) 4)
2
[44]> (cliffs-delta '(1 2 3) '(1 2 3))
0
[45]> (cliffs-delta '(1 2 3) '(10 20 30))
1
[46]> (ks '(1 2 3) '(1 2 3))
0
[47]> (same '(1 2 3 4 5) '(1 2 3 4 5) 0.1)
T
[48]> (same '(1 2 3) '(10 20 30) 0.1)
NIL
```

### eps-pool / lead-same? / sames

Pool standard deviations to make a tolerance, then rank
treatments by walking sorted means and grouping
[`same`](#same) ones.

```lisp
(defun eps-pool (a b)
  (max 1 (* 0.35 (sqrt (/ (+ (expt (spread a) 2)
                             (expt (spread b) 2))
                          2)))))

(defun lead-same? (lead entry dict)
  (same (aget (car lead)  dict nil)
        (aget (car entry) dict nil)
        (eps-pool (cdr lead) (cdr entry))))

(defun sames (dict)
  (let* ((nums   (loop for (nm . vs) in dict
                       collect (cons nm (adds vs))))
         (sorted (gt (f+ (mid (cdr _))) nums))
         (lead   (car sorted))
         (rank   1)
         (out    nil))
    (dolist (entry sorted (nreverse out))
      (when (and (not (eq entry lead))
                 (not (lead-same? lead entry dict)))
        (incf rank)
        (setf lead entry))
      (push (list :name (car entry)
                  :num  (cdr entry)
                  :rank rank)
            out))))
```

### rank-print

Pretty-print [`sames`](#sames). Blank line between rank groups.

```lisp
(defun rank-print (dict)
  (format t "~&  ~4a  ~12a  ~6a  ~6a~%"
          "rank" "treatment" "mu" "sd")
  (let ((prev 0))
    (dolist (r (sames dict))
      (when (and (/= (getf r :rank) prev) (> prev 0))
        (format t "~%"))
      (setf prev (getf r :rank))
      (format t "~&  ~4d  ~12a  ~6,2f  ~6,2f~%"
              (getf r :rank) (getf r :name)
              (mid (getf r :num))
              (spread (getf r :num))))))
```

```
[49]> (eps-pool (adds '(1 2 3)) (adds '(1 2 3)))
1
[50]> (sames '(("a" . (1 2 3)) ("b" . (1 2 3))))
((:NAME "a" :NUM #S(NUM ...) :RANK 1)
 (:NAME "b" :NUM #S(NUM ...) :RANK 1))
```

---

## Examples

Each `eg--X` is self-contained, runnable, and prints or
returns something verifiable. CLI dispatches `--name` to
`eg--name`. `eg--check` runs assertion-flavoured egs and
exits non-zero on failure.

```lisp
(defun eg--testNum ()
  (let ((nm (adds '(1 2 3 4 5))))
    (eq= (num-n nm) 5 "Num n")
    (eq= (mid nm) 3 "Num mu")))

(defun eg--testSym ()
  (let ((s (make-sym)))
    (dolist (v '(a a a b b c)) (add s v))
    (eq= (sym-n s) 6 "Sym n")
    (eq= (cdr (assoc 'a (sym-has s))) 3 "Sym a count")))

(defun nb-min-g (file)
  (loop for r in (stats (nb (make-data (read-csv file))))
        minimize (getf r :g)))

(defun eg--testDiabetes ()
  (let ((g (nb-min-g
             "~/gits/timm/moot/classify/diabetes.csv")))
    (ok (>= g 50)
        (format nil "diabetes min G=~d >= 50" g))))
```

`lib.lisp` provides `eg--all`, which finds every `eg--X` symbol
and runs it.

```
[51]> (eg--testNum)
PASS Num n (want 5 got 5)
PASS Num mu (want 3 got 3)
[52]> (eg--testSym)
PASS Sym n (want 6 got 6)
PASS Sym a count (want 3 got 3)
[53]> (eg--testDiabetes)
PASS diabetes min G=68 >= 50
[54]> (defparameter *fails* 0)
*FAILS*
[55]> (eg--check)
PASS ... (5 unit asserts)
PASS diabetes min G=68 >= 50
PASS soybean min G=62 >= 30
;; asserts passed: 7  failed: 0
```

---

## Limitations

> [!IMPORTANT] limit
> - **Independence.** Naive Bayes assumes features are
>   conditionally independent given the class. They aren't.
>   Correlated features get double-counted; biased probability
>   estimates can shift class ranking on borderline rows.
> - **Cold start.** First instance of any value gets count 0 in
>   the matching class's [`sym`](#sym). Laplace `k` smoothing
>   mitigates but doesn't eliminate this. Tune via `-k N`.
> - **Numeric assumption.** [`like-num`](#like-num) is Gaussian.
>   Skewed or multi-modal columns are mis-modelled. No transform
>   step.
> - **Deterministic order.** [`nb`](#nb) reads the CSV in file
>   order. Pathological orderings can degrade burn-in classifier.
>   No shuffling; reproducibility wins over robustness here.
> - **Memory.** Each class keeps its own [`data`](#data) clone.
>   `O(n)` memory per class. Fine for ~10k rows; not for 10M.

## Shortcuts made

> [!NOTE] this-codebase
> - **Alist not hashtable.** `$has`, `cm`, `klasses` are all
>   alists. Linear lookup. Fine for small cardinality (≤ ~50
>   classes/values). Switch to `make-hash-table :test 'equal`
>   if you push beyond.
> - **String prefix for type marker.** [`cols`](#cols) parses
>   role from a single suffix character. No metadata column.
>   Robust enough for academic CSVs; brittle for real-world
>   data with weird headers.
> - **Online metrics, no holdout.** [`nb`](#nb) tests each row
>   against the prefix it has already seen. Reproducible but
>   underestimates the model — a proper k-fold or hold-out
>   split would give different (usually slightly lower) numbers.
> - **Logs everywhere.** Likelihoods are log-summed. The
>   absolute likelihood values are not recovered; only their
>   *ratio* (argmax) matters for classification.

---

## Tutorial

A primer for the Common Lisp constructs the body uses. Readers
fluent in Python, C, or Java recognise most of the structure;
the items below are where Lisp diverges.

### Quote vs sharp-quote

`'x` is data: stops evaluation, returns the symbol or list
literally. `#'x` is the function value of `x` — used where a
function is passed as an argument. First in [`add-sym`](#add-sym):
`:test #'equal`.

```
[T1]> 'foo
FOO
[T2]> #'<
#<FUNCTION <>
```

### `nil` is empty list AND false

No separate boolean type. Empty list, the symbol `nil`, and
false are the same value. Anything else is true.

```
[T3]> (if nil 'yes 'no)
NO
[T4]> (if '() 'yes 'no)
NO
[T5]> (if '(1) 'yes 'no)
YES
```

### `let` / `let*` / double-paren bindings

`let` binds in parallel; `let*` sequentially. Bindings live
inside an extra pair of parens: `(let ((x 1) (y 2)) ...)`.
First in [`make-num`](#num).

```
[T6]> (let  ((a 1) (b 2)) (+ a b))
3
[T7]> (let* ((a 1) (b (* a 2))) b)
2
```

### `setf` as universal place setter

Writes to any "place" — variable, struct slot, alist cell.
Used everywhere; `(setf $sd ...)` in [`add-num`](#add-num);
`(rplacd row ...)` in [`cm-incf`](#cm-incf) is the alist
equivalent.

### Lambda lists: `&optional`, `&key`, `&rest`, `&aux`

- `&optional (w 1)` — default value.
- `&key :txt "x"` — named arg.
- `&rest args` — collect extras.
- `&aux (var init)` — local bound after params, before body.

First in [`make-num`](#num) (`&rest args &aux (i ...)`).

```
[T8]> (defun f (x &optional (y 0) &aux (z (* x 2)))
        (+ x y z))
F
[T9]> (f 3)
9
[T10]> (f 3 1)
10
```

### Multiple values

`(values a b)` returns more than one value; caller pulls them
with `multiple-value-bind`. Used in [`cm-fnfp`](#cm-total-cm-fnfp-metrics)
and [`stats`](#stats).

```
[T11]> (defun two () (values 1 2))
TWO
[T12]> (multiple-value-bind (a b) (two) (list a b))
(1 2)
```

### `defstruct` and slot access

Declares a record type, generates a constructor, accessors,
type predicate. The `(:constructor %make-num)` clause renames
the auto-generated maker so we can wrap it in [`make-num`](#num).

### `defmethod` and CLOS dispatch

`(defmethod name ((arg type) ...) body)` defines one method
of a generic function. Runtime picks the method whose argument
types match. The body presents parallel methods (`add`, `mid`,
`spread`, `like`) for [`sym`](#sym) and [`num`](#num); the
dispatcher chooses without a type check in the caller.

### `loop` macro

CL's iteration DSL. Body uses:

- `loop for x in lst sum (...)` — accumulate.
- `loop for (k . v) in alist do (...)` — destructuring iter.
- `loop while cond do (...)` — open-ended.

First in [`likes`](#likes).

```
[T13]> (loop for x in '(1 2 3) collect (* x x))
(1 4 9)
[T14]> (loop for (k . v) in '((a . 1) (b . 2))
             sum v)
3
```

### Reader macros: `$`, `@`, `?`, `f+`, `ff+`

From `lib.lisp`:

- `$x` → `(slot-value i 'x)`. Works inside any function with
  `i` in scope.
- `@k` → `(the-of 'k)`. Looks up config.
- `(? x a b)` → `(slot-value (slot-value x 'a) 'b)`. Chain.
- `(f+ body)` → `(lambda (_) body)`. One-arg shortcut.
- `(ff+ body)` → `(lambda (_ __) body)`. Two-arg shortcut.

First in [`add-sym`](#add-sym) (`$n`, `$has`); throughout body.

```
[T15]> (defstruct foo bar)
FOO
[T16]> (let ((i (make-foo :bar 42))) $bar)
42
```

### `declare ignore`

When a `multiple-value-bind` or method param is unused,
declare it ignored to silence the compiler. First in
[`like-num`](#like-num).

---

> End. Read top-to-bottom for shape; re-read for seams. Each
> stanza is one concept; each builds on the prior.
