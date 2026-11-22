# hair (lisp): experiment notes

Newest first. One entry per question asked. Each says what was run, on
what, and what it showed, so a later session can pick up from here.

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
