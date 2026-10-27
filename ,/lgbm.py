#!/usr/bin/env python3
"""lgbm.py: LightGBM, over min-eg.lua's splits, scored its way.

A split is a seed, not a file: min.py's `rint` and `shuffle` are
the ones in min.lua, so this sees the rows the lua learners saw.

usage: python3 ,/lgbm.py f.csv        (writes <cache>/<set>/lgbm.csv)
"""
import os, sys, time
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import min as M
from lightgbm import LGBMClassifier, LGBMRegressor

Cache = os.path.expanduser("~/tmp/cache")
Reps  = 20

def home(f):
  "Where this data set's scores live, mirroring moot's tree."
  m = f.split("moot/")[-1]
  return f"{Cache}/{m[:-4]}"

def pair(t, s):
  "Rep S's two halves of T, from the seed alone."
  M.the.Seed = 1234567891 + s
  rows = M.shuffle(t.rows[:])
  n    = int(.8 * len(rows))
  return M.clone(t, rows[:n]), M.clone(t, rows[n:])

def goal(t):
  "One num goal: it.  Else: ydist, the distance to heaven."
  c = t.cols.y[0] if len(t.cols.y) == 1 else None
  if c is None or type(c) is not M.NUM: return lambda r: M.ydist(t, r)
  return lambda r: c.mu if r[c.at] == "?" else float(r[c.at])

def xy(t, i, klass):
  "Numbers in, label out.  Syms map through I, shared by the splits."
  X, y = [], []
  for r in t.rows:
    X.append([float("nan")                       if r[c.at] == "?" else
              i.setdefault((c.at, r[c.at]), len(i)) if type(c) is M.SYM else
              float(r[c.at])                     for c in t.cols.x])
    y.append(str(r[klass.at]) if klass else goal(t)(r))
  return X, y

def macro(want, got):
  "Accuracy, then abcd.awk's macro pd, pf and d2h."
  n, ok, ws, gs, hs = len(want), 0, {}, {}, {}
  for a, b in zip(want, got):       # a klass may be int here, str
    a, b = str(a), str(b)           # there: sklearn coerces mixed
    ok += a == b
    ws[a] = ws.get(a, 0) + 1
    gs[b] = gs.get(b, 0) + 1
    if a == b: hs[a] = hs.get(a, 0) + 1
  pd = pf = d2h = 0
  for x, w in ws.items():
    tp = hs.get(x, 0); fp = gs.get(x, 0) - tp; fn = w - tp
    tn = n - fn - fp - tp
    r1 = tp / (tp + fn)
    f1 = fp / (fp + tn) if fp + tn else 0
    pd += r1; pf += f1; d2h += ((1-r1)**2 + f1**2) ** .5 / 2 ** .5
  k = len(ws)
  return f"{100*ok/n:.1f},{100*pd/k:.1f},{100*pf/k:.1f},{d2h/k:.3f}"

def rank(want, got, check=5):
  "Label its best CHECK guesses: where does the best of those sit?"
  order = sorted(range(len(got)), key=lambda j: got[j])
  best  = min(want[j] for j in order[:check])
  ys    = sorted(want)
  for n, v in enumerate(ys):
    if v == best: return 100 * n / max(1, len(ys) - 1)
  return 100

def klassOf(t, file):
  "Classify sets guess the last ! column, if it is discrete."
  if "/classify/" not in file: return None
  c = next((c for c in reversed(t.cols.y) if c.txt.endswith("!")), None)
  if c is None or type(c) is M.SYM: return c
  return c if len({r[c.at] for r in t.rows}) <= 30 else None

def main(file):
  "Score LightGBM on every rep, unless that was already done."
  out = f"{home(file)}/lgbm.csv"
  if os.path.exists(out): print(out); return
  os.makedirs(home(file), exist_ok=True)
  t     = M.Tbl(file)
  klass = klassOf(t, file)
  if "/classify/" in file and not klass:
    print(f"{file}: no discrete ! column, skipped", file=sys.stderr); return
  lines = []
  for s in range(1, Reps + 1):
    tr, te = pair(t, s)
    i      = {}
    X1, y1 = xy(tr, i, klass)
    X2, y2 = xy(te, i, klass)
    t0     = time.time()
    model  = (LGBMClassifier if klass else LGBMRegressor)(verbose=-1)
    model.fit(X1, y1)
    got    = list(model.predict(X2))
    secs   = time.time() - t0
    lines.append(f"{s},{secs:.4f}," + (macro(y2, got) if klass else
      f"{rank(y2, got):.3f},"
      f"{sum(abs(a-b) for a,b in zip(y2,got))/len(y2):.6g}"))
  with open(out, "w") as f:
    f.write("rep,secs," + ("acc,pd,pf,d2h" if klass else "rank,mae") + "\n")
    f.write("\n".join(lines) + "\n")
  print(out)

main(sys.argv[1])
