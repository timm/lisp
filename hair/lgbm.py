#!/usr/bin/env python3 -B
"""lgbm: add lightgbm to eg-ens's table, on the same 5x2 folds.
Reads ens.dat (from `sbcl --script hair.lisp ens`): a #names
line, then per fold: data | aucs | secs | test row #s. Scores and ranks with
~/gh/hair's aucs and rank (Cohen + Cliff's delta + KS)."""
import os, sys, time, warnings
sys.path.insert(0, os.path.expanduser("~/gh/hair"))
from hair.stats import aucs, rank
import numpy as np, lightgbm as lgb
warnings.filterwarnings("ignore")

def load(f):
  "x matrix (nums float, syms category codes), class list"
  rows = [l.strip().split(",") for l in open(
          os.path.expanduser("~/gits/moot/classify/%s.csv" % f))
          if l.strip()]                          # skip blank lines
  head, rows = rows[0], rows[1:]
  k  = next(i for i, h in enumerate(head) if h.endswith("!"))
  xs = [i for i, h in enumerate(head)
        if i != k and not h.rstrip("+-!").endswith("X")]
  def col(i):
    vs = [r[i] for r in rows]
    if head[i][0].isupper():
      return [np.nan if v == "?" else float(v) for v in vs]
    codes = {v: j for j, v in enumerate(sorted(set(vs) - {"?"}))}
    return [codes.get(v, np.nan) for v in vs]
  cats = [j for j, i in enumerate(xs) if not head[i][0].isupper()]
  return (np.array([col(i) for i in xs]).T, [r[k] for r in rows], cats)

def lgbm(X, y, cats, train, test):
  "(want, {class: prob}) per test row"
  m = lgb.LGBMClassifier(verbose=-1, n_jobs=1)
  m.fit(X[train], [y[i] for i in train], categorical_feature=cats)
  return [(y[i], dict(zip(m.classes_, p)))
          for i, p in zip(test, m.predict_proba(X[test]))]

def main():
  folds, lines = {}, open(sys.argv[1] if len(sys.argv) > 1
                          else "ens.dat").readlines()
  T = lines[0][1:].split() + ["lgbm"]            # "#names" header
  for line in lines[1:]:
    f, a, s, t = line.strip().split("|")
    folds.setdefault(f, []).append(([float(v) for v in a.split()],
                                    [float(v) for v in s.split()],
                                    [int(v) for v in t.split()]))
  W, wins, means, secs = 12, dict.fromkeys(T, 0), {t: [] for t in T}, \
                         dict.fromkeys(T, 0.0)
  print("%-34s" % "#data" + "".join("%-*s" % (W, t) for t in T))
  for f, fs in folds.items():
    X, y, cats = load(f)
    ok = [i for i in range(len(y)) if y[i] != "?"]
    d  = {t: [] for t in T}
    for a, s, test in fs:
      for t, v, sec in zip(T, a, s):
        d[t].append(v); secs[t] += sec
      tset  = set(test)
      train = [i for i in ok if i not in tset]
      t0 = time.time()
      d["lgbm"].append(aucs(lgbm(X, y, cats, train, test)))
      secs["lgbm"] += time.time() - t0
    rk = rank(d, reverse=True)
    cells = []
    for t in T:
      mu = sum(d[t]) / len(d[t]); means[t].append(mu)
      wins[t] += rk[t] == 1
      cells.append("%5.1f%s" % (mu, "!" if rk[t] == 1 else ""))
    print("%-34s" % f + "".join("%-*s" % (W, c) for c in cells), flush=True)
  print("\n%-34s" % "#wins" + "".join("%5d       " % wins[t] for t in T))
  print("%-34s" % "#mean auc" + "".join(
        "%5.1f       " % (sum(means[t]) / len(means[t])) for t in T))
  print("%-34s" % "#secs (all data)" + "".join("%5.1f       " % secs[t] for t in T))

if __name__ == "__main__": main()
