# hair (lisp): experiment notes

## improtant prompt

often overwhelmed by the itneraction. code accumualtes fillsls (relics from bygne experinetns that yu did not thnk to remove which confuse ther esults)

good tacit tow rite down what you think ash been elarned so far, start a new code base and implemtn from scratch jsut the things you think matter the most

## Notes
Newest first. One entry per question asked. Each says what was run, on
what, and what it showed, so a later session can pick up from here.

## Dead ends (read this first)

Paths already tried across lisp/hair, ~/gh/hair, ~/gh/tab-dev (lua).
Do not re-run without a reason something changed. Source in brackets:
[L] lisp/hair, [P] ~/gh/hair, [T] ~/gh/tab-dev/CLAUDE.md (details there).

**Live path (2026-11-22):** bagged forest of deep trees (leaf=2,
maxd 30), 10 trees on random halves, sqrt(#x) cols per node, rows
binned once per fold into 8 equal-frequency bins, cut by histogram
count-and-sweep, <=200 rows/class, vote = mean leaf class share.
39 sets: forest 87.1 vs LightGBM 89.1 auc, same or faster. Level on
small sets; loses 6-12 on big mixed (electricity, churn,
bank-marketing, adult) [L][P]. tab-dev: 30 deep trees reach LightGBM
parity on acc/auc, still lose on d2h; TabICL far ahead [T].

Discretization
- MDL (Fayyad-Irani) / recursive bins before trees: -1.5 auc vs q8 [L];
  -4.1 for naive Bayes [T]. Fine for *showing* bins (2-6), not for
  learning.
- Equal width bins: as bad as q4, fails on skewed cols (hypothyroid
  -17.5) [L].
- More bins: q16 +0.1, q64 +0.3, q256 ~= no binning. Not worth it [L].
- Re-binning per node (`cutTill`): slower and worse [T].
- Fixed sd/2 bin width: a bug, not a finding; it produced false
  "binning hurts" results [T].
- Uncapped sym values as bins: ID-like cols (~7000 values) blew up
  time; cap sym to q-1 values [L].

Ensembles
- Laplace-smoothed leaf votes: no gain [L].
- FFT / best-of-3^d collapsed subtrees from one tree: worse than one
  tree (78.9 vs 79.5) [L]; many-klass collapses (k+1 regions) [T].
- Boosting (SAMME over deep trees): no gain over plain deep forest;
  maybe needs a learning rate -- the one open boosting question [T].
- More trees: past 30, +0.1 per doubling [T]. More rows per class
  (cap 512/1024): +1.6 auc for 2x time; big-set gap stays [L].
- Shallow trees (depth 4, leaf 8): starved, -13 on many-klass sets.
  Always deep, small leaves [T].
- Column subsample cols=3: -5; sqrt(#x) ties all cols and is faster
  [T][L].
- Bagged / random-subspace naive Bayes: nothing (NB is stable) [T].
- Big-data tuning (cap1k + q32 + all trees): closes some gap, slower
  than LightGBM; reverted [L].

Naive Bayes [T]
- Info-gain column weighting -4 to -6 (UNVERIFIED rig; replication
  queued in tab-dev/QUEUE.md), AODE (contradictory records),
  per-cluster NB (unverified), bottom-up entropy merge (tie), kNN
  (worse, 12x slower), tuning m/bins (seed noise).

Speed
- Hash tables for small counts in SBCL: slow; use alists [L].
- Sorting per node per col: replace by bin ids cached once per fold +
  counts into a bins x classes array (lisp 12.9 -> 3.5 s; lua 6.7x)
  [L][T].
- Speedups measured under parallel load lie (GC/scheduler contention,
  Apple E-cores). Time serially, take the min of reps [T].
- LightGBM max_bin 255/32/8: barely changes its auc [L].
- PyPy: 1.5-2x on the python forest; no lightgbm under pypy [P].

Data / rig traps
- COMPAS53 leaked its class (post-outcome cols): fixed in moot [L].
- Binning test rows with stats from all of train while training on a
  sample: leak; fixed [T].
- Lisp csv: strip \r, skip blank lines (blood-transfusion, german) [L].
- Python `bin` min() over (cost, at, v) crashed on int/str ties (car);
  fixed [P]. `!` col must also be in tbl.y [P].
- report rows must follow the header's order, not each dict's [P].
- Seed set once per run: adding a treatment shifts every later draw;
  +/-1 auc noise per set [L].

Shared setup, unless an entry says otherwise:

- data: the 39 `*ens*` sets in `~/gits/moot/classify` (see `eg.lisp`)
- 5x2 cross-val (`xval` default), seed `(? *k* seed)` set once per run
- forest = 10 trees, each on a random half of <= `cap` rows per class,
  bins built once (`binned`), `:rf t` = sqrt(#x) random cols per node
- score = `aucs` (class-weighted one-vs-rest AUC, 0..100)
- LightGBM 4.7.0 defaults, `n_jobs=1`, same folds (test row #s from
  `eg-ens`), scored with the same `aucs` formula
- secs = train + predict, summed over the 10 folds; 2-core cloud box
- caveat: the seed is set once per run, so adding a treatment or
  changing one data set shifts the random draws for everything after it;
  expect +/- 1 AUC run-to-run noise on any one set

## 2026-11-23: idea (NOT RUN): is tree learning = GAC on pair-packets?

Question: before tree learning, bin each col into tiny lo..hi ranges,
make 2-D "packets" (cells over a pair of cols). Is tree learning then
just greedy agglomerative clustering (GAC) of those packets?

Answer: partly. It is the bottom-up mirror of a tree, not the same.
- Same: a tree is divisive clustering of x, scored by y; merging
  tiny bins is GAC, scored by y (ChiMerge vs Fayyad-Irani in 1-D).
  Same score + adjacent-only merges -> similar, not identical, cuts.
- Differ:
  - leaves: tree = axis-aligned boxes; GAC = any union of cells
    (can be non-convex)
  - depth: tree = path length; pairs = 2 cols only
  - context: tree re-splits col b differently under each branch of a;
    a pair (a,b) is fixed globally
  - GAC gives a dendrogram, not a decision rule
- So GAC on pairs ~ a set of depth-2 trees with freer leaf shapes.
- Cost: d^2 * b^2 grid, but data clumps in few cells (bingo), so only
  occupied packets need merging. Cheap.

Hybrid to try: GAC inside each pair, tree chooses among pairs, recurse.

    def tree(rows, pairs):       # split on best merged pair, recurse
      i, j, regions = min(((i, j, gac(packets(rows, i, j, B), y_var, adj))
                           for i, j in pairs), key=lambda t: wvar(t[2]))
      return {reg: tree(reg.rows, pairs) for reg in regions}

Risk: leaves are unions of cells, not boxes -> rules harder to read.
Relevant dead ends above: re-binning per node was slower and worse;
here bins are built once, only merges are per node.

## 2026-11-22: rows per class (cap 200, 512, 1024)

7 sets: diabetes, vehicle, soybean, COMPAS53, churn, adult, electricity.

| cap | mean AUC | secs |
|-----|----------|------|
| 200 | 84.6 | 5.2 |
| 512 | 85.9 | 7.6 |
| 1024 | 86.4 | 13.5 |
| LightGBM | 89.4 | 12.8 |

- 200 -> 1024 buys only 1.6 AUC for 2x the time (and is then slower
  than LightGBM). Gains are all on big sets (churn +4, electricity +3).
- electricity stays ~9 AUC behind LightGBM at any cap. So the gap is
  not rows seen; more likely 10 bagged trees vs LightGBM's 100 boosted.
- Decision: keep `cap 200`. To close the gap, try more trees (`:n 30`,
  `:n 100`) or boosting next.

## 2026-11-22: q8 forest vs LightGBM

All 39 sets, after sym columns were capped at q-1 values.

| | mean AUC | secs (all 39) |
|-|----------|---------------|
| q8 forest, cap 200 | 86.6 | 19.4 |
| LightGBM | 88.9 | 36.5 |

- ~2x faster overall; faster on most sets.
- Level with LightGBM on small sets (wins diabetes, ecoli, heart.c,
  hepatitis); the gap is on big sets: electricity -12, churn -8,
  bank-marketing -7, adult -6.
- Before the sym cap, COMPAS53 alone took 51 s (ID-like sym columns with
  ~7000 values each became ~7000 bins); after, 0.6 s.

## 2026-11-22: COMPAS53 leaked its class

`two_year_recid!` was predictable from post-screening outcomes
(`is_recid`, `r_*`, `vr_*`, `event`, `end`), COMPAS's own scores
(`decile_score`, `score_text`, `v_*`) and row IDs. Both learners scored
~98 AUC. Marked those 29 columns X in moot (commit e40a48e, see moot's
CHANGELOG.md). Now: hair 69, LightGBM 74, in line with published COMPAS
results (~0.70).

## 2026-11-22: how many bins? (q)

All 39 sets. Forest arms `:rf t`; single trees use all cols. Compared to
q=256 (~ no binning) with `same` (Cohen + Cliff's + KS).

| arm | mean AUC | delta vs q256 | win/tie/lose |
|-----|----------|---------------|--------------|
| q4 | 86.1 | -0.72 | 5/16/18 |
| q8 | 86.5 | -0.31 | 3/19/17 |
| q16 | 86.6 | -0.19 | 5/22/12 |
| q64 | 86.8 | -0.01 | 11/19/9 |
| w8 (equal width) | 86.1 | -0.73 | 10/15/14 |
| 1 tree, q8 vs q256 | 78.9 | +0.06 | 10/19/10 |

- q8 equal-frequency bins cost the forest ~0.3 AUC; most per-set losses
  < 1.5 (worst cmc -2.3). A single tree does not care (+0.06).
- Equal width is worse: as bad as q4, and fails on skewed columns
  (hypothyroid -17.5, electricity -5.7). Earlier "8 equal-width bins is
  fine" memory may have come from data without skewed columns.
- Decision: q=8, equal frequency. Then made every tree binned (one
  learner; old exhaustive cutter is in `attic.lisp`).
- Wall clock: 7.6 min for 39 sets x 10 folds x 8 arms on 2 cores.
