#!/usr/bin/env python3 -B
"""plot: per-set mean AUC (39 moot/classify sets, 5x2 folds), one line
per arm, sets grouped numeric/symbolic/mixed/large, sorted by lgbm.
Reads only runs/ (so it works on any machine):
  runs/lgbm.txt   lgbm.py output: %folds lines for lgbm, 1:q8, q8 (hair)
  runs/*.dat      eg-ens output, row #s stripped: data|aucs|secs
  runs/tabicl.csv TabICL per set, from tab-dev (80:20 splits, 34 sets)
Usage: python3 -B plot.py OUT.png [--tabicl] NAME=FILE:COL ...
  NAME=lgbm.txt:q8   an arm from runs/lgbm.txt, by treatment name
  NAME=grid-q16.dat:0 column 0 of an eg-ens file
  --tabicl            add TabICL (black); show only its 34 sets
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license"""
import sys, csv, numpy as np, matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt

R = "runs/"
F, secs = {}, {}
for l in open(R + "lgbm.txt").read().splitlines():
  if l.startswith("%folds"):
    _, f, t, *v = l.split(); F[f, t] = np.mean(list(map(float, v)))
  if l.startswith("#secs"):
    secs.update(zip(["1:q8", "q8", "lgbm"], map(float, l.split()[3:6])))
order = list(dict.fromkeys(f for f, _ in F))

def arm(spec):
  "per-set mean auc and total secs of one NAME=FILE:COL arm"
  file, col = spec.split(":")
  if file == "lgbm.txt": return {f: F[f, col] for f in order}, secs[col]
  d, t = {}, 0.0
  for l in open(R + file).read().splitlines()[1:]:
    f, a, s = l.split("|")[:3]
    d.setdefault(f, []).append(float(a.split()[int(col)]))
    t += float(s.split()[int(col)])
  return {f: np.mean(v) for f, v in d.items()}, t

out, args = sys.argv[1], sys.argv[2:]
icl = "--tabicl" in args
A = [(k, *arm(v)) for k, v in (a.split("=") for a in args if "=" in a)]
d = {f: {k: m[f] for k, m, _ in A} for f in order}
if icl:
  T = {r["set"]: float(r["auc"]) for r in csv.DictReader(
       l for l in open(R + "tabicl.csv") if l[0] != "#")}
  for f in order: d[f]["tabicl"] = T.get(f, np.nan)
keep = [f for f in order if not icl or f in T]
G = [(g, [f for f in fs if f in keep]) for g, fs in
     [("numeric", order[0:10]), ("symbolic", order[10:20]),
      ("mixed", order[20:30]), ("large, mixed", order[30:39])]]
C = [("#2a6fdb", "o"), ("#e8613c", "s"), ("#1aaa7a", "^"), ("#9b59b6", "D")]
L = [(k, f"{k}, {t:.0f}s" if not icl else k, c, m)
     for (k, _, t), (c, m) in zip(A, C)]
if icl: L += [("tabicl", "tabicl (tab-dev runs)", "black", "v")]
first = A[0][0]                                  # sort by the first arm
SHORT = {"blood-transfusion-service-center": "blood-transfusion",
         "Internet-Advertisements": "internet-ads"}
fig, ax = plt.subplots(figsize=(14, 5.2))
x0, ticks, labs = 0, [], []
for gi, (g, fs) in enumerate(G):
  fs = sorted(fs, key=lambda f: d[f][first]); xs = np.arange(x0, x0 + len(fs))
  for k, name, c, m in L:
    mu = np.mean([d[f][k] for f in keep])
    ax.plot(xs, [d[f][k] for f in fs], color=c, marker=m, lw=2, ms=6,
            label=f"{name}: mean {mu:.1f}" if gi == 0 else None)
  ax.text(xs.mean(), 101.5, g, ha="center", va="bottom", fontsize=13)
  ticks += list(xs); labs += [SHORT.get(f, f) for f in fs]
  x0 += len(fs) + 1
  if gi < len(G) - 1: ax.axvline(x0 - 1, color="#555", lw=1)
ax.set_xticks(ticks)
ax.set_xticklabels(labs, rotation=60, ha="right", rotation_mode="anchor",
                   fontsize=10)
ax.set_ylim(40, 100); ax.set_xlim(-.7, x0 - 1.3)
ax.set_yticks(range(40, 101, 10)); ax.grid(axis="y", color="#ddd")
ax.set_ylabel("AUC (0..100), mean of 5x2 folds", fontsize=12)
ax.legend(loc="lower right", fontsize=11, frameon=True)
for s in ["top", "right"]: ax.spines[s].set_visible(False)
fig.tight_layout(); fig.savefig(out, dpi=150); print(out)
