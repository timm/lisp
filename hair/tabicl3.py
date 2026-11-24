#!/usr/bin/env python3 -B
"""tabicl3: add lightgbm AND TabICL to eg-ens's table, on the same 5x2 folds.

Same input and output as lgbm.py -- reads an ens.dat (from `sbcl --script
hair.lisp ens`, or se.lisp with SETS=...), whose every line is
`data|aucs|secs|test row #s` for the lisp arms -- but scores two more arms
per fold, so a figure can put lisp(10), LightGBM and TabICL on one axis.

TabICL is in-context: `fit` only stores the training rows, and the work
happens in `predict_proba`.  It is given the SAME train/test split the lisp
arms saw, which is the point: the cached TabICL scores in tab-dev are 20
random 80/20 holdouts, so they cannot be laid beside a 5x2 number.

MEMORY.  One fold of `se/issues/7/hadoop` (12,191 rows) peaked at 9.4GB at
the stock `batch_size=8`, and six of those at once wedged a 16GB laptop.
So: `batch_size=1` (3.1GB on the same fold, same predictions to the last
digit, same wall clock -- the knob only says how many ensemble members are
in flight), and big sets run with fewer workers than small ones.  TabICL
also starts its own joblib pool inside each worker, so LOKY_MAX_CPU_COUNT
and friends are pinned to 1 below; without that the process count is not
WORKERS but some multiple of it.

CRASHES.  Every fold is appended to a jsonl as it finishes, so a kill
costs the folds in flight, not the run.  Re-running skips what the jsonl
already holds.  The table is printed from the jsonl, not from memory.

usage: tabicl3.py ens.dat scores.jsonl > ens3.txt
       tabicl3.py ens.dat scores.jsonl --report > ens3.txt   (no runs)
"""
import os, sys, json, time, warnings, multiprocessing as mp

for v in ("OMP_NUM_THREADS", "MKL_NUM_THREADS", "OPENBLAS_NUM_THREADS",
          "NUMEXPR_NUM_THREADS", "VECLIB_MAXIMUM_THREADS", "LOKY_MAX_CPU_COUNT"):
  os.environ.setdefault(v, "1")
os.environ.setdefault("JOBLIB_MULTIPROCESSING", "0")
os.environ.setdefault("TOKENIZERS_PARALLELISM", "false")

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.expanduser("~/gh/hair"))
warnings.filterwarnings("ignore")

from hair.stats import aucs, rank
from lgbm import load, lgbm

Small   = int(os.getenv("SMALL_WORKERS", "6"))   # sets under Big rows
Big     = int(os.getenv("BIG_WORKERS", "2"))     # sets at or over Big rows
BigRows = int(os.getenv("BIG_ROWS", "3000"))
Batch   = int(os.getenv("BATCH", "1"))
Device  = os.getenv("DEVICE", "cpu")
Floor   = float(os.getenv("MEM_FLOOR_GB", "3"))  # wait if RAM is tighter
Data    = {}


def data(f):
  "Load once per worker."
  if f not in Data: Data[f] = load(f)
  return Data[f]


def roomy():
  "Block while free memory is under the floor, so a burst cannot wedge the box."
  try:
    import psutil
  except ImportError:
    return
  for _ in range(600):
    if psutil.virtual_memory().available / 2**30 >= Floor: return
    time.sleep(1)


def tabicl(X, y, train, test):
  "(want, {class: prob}) per test row, from the in-context model."
  from tabicl import TabICLClassifier
  m = TabICLClassifier(device=Device, n_jobs=1, random_state=1,
                       batch_size=Batch)
  m.fit(X[train], [y[i] for i in train])
  return [(y[i], dict(zip(m.classes_, p)))
          for i, p in zip(test, m.predict_proba(X[test]))]


def job(args):
  "One fold of one data set: score lgbm and tabicl on the lisp arms' split."
  f, k, test = args
  X, y, cats = data(f)
  ok    = [i for i in range(len(y)) if y[i] != "?"]
  train = [i for i in ok if i not in set(test)]
  out   = {"set": f, "fold": k}
  for name, fn in (("lgbm",   lambda: lgbm(X, y, cats, train, test)),
                   ("tabicl", lambda: tabicl(X, y, train, test))):
    if name == "tabicl": roomy()
    t0 = time.time()
    try:
      out[name] = [aucs(fn()), time.time() - t0]
    except Exception as e:                   # one bad fold must not sink the run
      print(f"# {f} fold {k} {name}: {type(e).__name__}: {e}",
            file=sys.stderr, flush=True)
      out[name] = [None, time.time() - t0]
  return out


def run(folds, rows, jsonl):
  "Everything not already in the jsonl, small sets first, biggest last."
  done = set()
  if os.path.exists(jsonl):
    for line in open(jsonl):
      try:
        d = json.loads(line); done.add((d["set"], d["fold"]))
      except json.JSONDecodeError:           # a half-written line from a kill
        pass
    print(f"# resuming: {len(done)} folds already scored", file=sys.stderr)
  todo = [(f, k, t) for f in folds for k, (_, _, t) in enumerate(folds[f])
          if (f, k) not in done]
  for label, pick, workers in (("small", lambda f: rows[f] < BigRows, Small),
                               ("big",   lambda f: rows[f] >= BigRows, Big)):
    jobs = [j for j in todo if pick(j[0])]
    if not jobs: continue
    print(f"# {label}: {len(jobs)} folds, {workers} workers, batch={Batch}",
          file=sys.stderr, flush=True)
    t0, n = time.time(), 0
    with open(jsonl, "a") as out, mp.Pool(workers) as pool:
      for d in pool.imap_unordered(job, jobs, chunksize=1):
        out.write(json.dumps(d) + "\n"); out.flush()
        n += 1
        if n % 10 == 0 or n == len(jobs):
          el = time.time() - t0
          print(f"# {label} {n}/{len(jobs)}, {el/60:.1f} min, "
                f"eta {el/n*(len(jobs)-n)/60:.1f} min", file=sys.stderr, flush=True)


def report(order, folds, jsonl):
  "The lgbm.py table, rebuilt from the jsonl."
  got = {}
  for line in open(jsonl):
    try:
      d = json.loads(line); got[(d["set"], d["fold"])] = d
    except json.JSONDecodeError:
      pass
  T = ["1:q8", "q8", "lgbm", "tabicl"]
  W, wins = 12, dict.fromkeys(T, 0)
  means, secs, spread = {t: [] for t in T}, dict.fromkeys(T, 0.0), {}
  print("%-34s" % "#data" + "".join("%-*s" % (W, t) for t in T))
  for f in order:
    d = {t: [] for t in T}
    for k, (a, s, _) in enumerate(folds[f]):
      for t, v, sec in zip(T, a, s):         # the lisp arms, as recorded
        d[t].append(v); secs[t] += sec
      g = got.get((f, k))
      if not g: continue
      for t in ("lgbm", "tabicl"):
        v, sec = g[t]
        if v is not None: d[t].append(v)
        secs[t] += sec
    if any(not d[t] for t in T):
      print(f"# {f}: incomplete, skipped", file=sys.stderr); continue
    spread[f] = d
    rk, cells = rank(d, reverse=True), []
    for t in T:
      mu = sum(d[t]) / len(d[t]); means[t].append(mu)
      wins[t] += rk[t] == 1
      cells.append("%5.1f%s" % (mu, "!" if rk[t] == 1 else ""))
    print("%-34s" % f + "".join("%-*s" % (W, c) for c in cells), flush=True)
  print("\n%-34s" % "#wins" + "".join("%5d       " % wins[t] for t in T))
  print("%-34s" % "#mean auc" + "".join(
        "%5.1f       " % (sum(means[t]) / len(means[t])) for t in T))
  print("%-34s" % "#secs (all data)" + "".join("%5.1f       " % secs[t] for t in T))
  for f, d in spread.items():
    for t in T:
      print("%%folds %s %s %s" % (f, t, " ".join("%.1f" % v for v in d[t])))


def main():
  dat, jsonl = sys.argv[1], sys.argv[2]
  folds = {}
  for line in open(dat).readlines()[1:]:
    f, a, s, t = line.strip().split("|")
    folds.setdefault(f, []).append(([float(v) for v in a.split()],
                                    [float(v) for v in s.split()],
                                    [int(v) for v in t.split()]))
  order = list(folds)
  rows  = {f: len(load(f)[1]) for f in order}
  if "--report" not in sys.argv: run(folds, rows, jsonl)
  report(order, folds, jsonl)


if __name__ == "__main__":
  mp.set_start_method("spawn")
  main()
