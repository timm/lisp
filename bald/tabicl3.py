#!/usr/bin/env python3 -B
"""tabicl3 (bald): add lightgbm AND TabICL to eg-ens's table, on the same
5x2 folds.

Reads an ens.dat from bald's eg-ens (`sbcl --script ens30.lisp`), whose
every line is `data|aucs|secs|test row #s` for the lisp arm(s) -- the
#names header says how many -- and scores two more arms per fold, so a
figure can put bald, LightGBM and TabICL on one axis.

Differences from hair's tabicl3.py:
  - lisp arm names come from the dat's #names header, not a hard list;
  - each TabICL fold gets a TABICL_TIMEOUT alarm (default 900s);
  - sets run smallest first, one set at a time; a big set's first fold
    is a probe, and if TabICL times out there the rest of the set is
    dropped (the figure only shows sets where TabICL terminates).

MEMORY.  TabICL spins up its own joblib pool per worker, so the thread
env vars are pinned to 1 below; batch_size=1 keeps one ensemble member
in flight; big sets run with fewer workers; and roomy() blocks while
free RAM is under MEM_FLOOR_GB.

CRASHES.  Every fold is appended to a jsonl as it finishes, so a kill
costs the folds in flight, not the run.  Re-running skips what the
jsonl already holds.  The table is printed from the jsonl.

usage: tabicl3.py ens.dat scores.jsonl > ens3.txt
       tabicl3.py ens.dat scores.jsonl --report > ens3.txt   (no runs)
"""
import os, sys, json, time, signal, warnings, multiprocessing as mp

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

Small   = int(os.getenv("SMALL_WORKERS", "5"))   # sets under Big rows
Big     = int(os.getenv("BIG_WORKERS", "2"))     # sets at or over Big rows
BigRows = int(os.getenv("BIG_ROWS", "3000"))
Batch   = int(os.getenv("BATCH", "1"))
Device  = os.getenv("DEVICE", "cpu")
Floor   = float(os.getenv("MEM_FLOOR_GB", "4"))  # wait if RAM is tighter
Timeout = int(os.getenv("TABICL_TIMEOUT", "900"))  # secs per TabICL fold
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


def _alarm(sig, frame): raise TimeoutError(f"TabICL fold over {Timeout}s")


def job(args):
  "One fold of one data set: score lgbm and tabicl on the lisp arms' split."
  f, k, test = args
  X, y, cats = data(f)
  ok    = [i for i in range(len(y)) if y[i] != "?"]
  train = [i for i in ok if i not in set(test)]
  out   = {"set": f, "fold": k}
  for name, fn in (("lgbm",   lambda: lgbm(X, y, cats, train, test)),
                   ("tabicl", lambda: tabicl(X, y, train, test))):
    if name == "tabicl":
      roomy()
      signal.signal(signal.SIGALRM, _alarm); signal.alarm(Timeout)
    t0 = time.time()
    try:
      out[name] = [aucs(fn()), time.time() - t0]
    except TimeoutError as e:
      print(f"# {f} fold {k} {name}: {e}", file=sys.stderr, flush=True)
      out[name] = [None, time.time() - t0]; out["timeout"] = True
    except Exception as e:                   # one bad fold must not sink the run
      print(f"# {f} fold {k} {name}: {type(e).__name__}: {e}",
            file=sys.stderr, flush=True)
      out[name] = [None, time.time() - t0]
    finally:
      if name == "tabicl": signal.alarm(0)
  return out


def run(folds, rows, jsonl):
  "Everything not already in the jsonl: one set at a time, smallest first."
  done = set()
  if os.path.exists(jsonl):
    for line in open(jsonl):
      try:
        d = json.loads(line); done.add((d["set"], d["fold"]))
      except json.JSONDecodeError:           # a half-written line from a kill
        pass
    print(f"# resuming: {len(done)} folds already scored", file=sys.stderr)
  t0, ntot, ndone = time.time(), 0, 0
  order = sorted(folds, key=lambda f: rows[f])
  todo  = {f: [(f, k, t) for k, (_, _, t) in enumerate(folds[f])
               if (f, k) not in done] for f in order}
  ntot  = sum(map(len, todo.values()))
  for f in order:
    jobs = todo[f]
    if not jobs: continue
    big, workers = rows[f] >= BigRows, Small if rows[f] < BigRows else Big
    with open(jsonl, "a") as out:
      if big:                                # probe: is this set feasible?
        d = job(jobs[0]); out.write(json.dumps(d) + "\n"); out.flush()
        ndone += 1; jobs = jobs[1:]
        if d.get("timeout"):
          print(f"# {f}: TabICL timed out on the probe fold; "
                f"dropping its other {len(jobs)} folds", file=sys.stderr,
                flush=True)
          ntot -= len(jobs); continue
      with mp.Pool(workers) as pool:
        for d in pool.imap_unordered(job, jobs, chunksize=1):
          out.write(json.dumps(d) + "\n"); out.flush(); ndone += 1
    el = time.time() - t0
    print(f"# {f} ({rows[f]} rows) done; {ndone}/{ntot} folds, "
          f"{el/60:.1f} min, eta {el/max(1,ndone)*(ntot-ndone)/60:.1f} min",
          file=sys.stderr, flush=True)


def report(T0, order, folds, jsonl):
  "The lgbm.py table, rebuilt from the jsonl. T0: lisp arm names, in order."
  got = {}
  if os.path.exists(jsonl):
    for line in open(jsonl):
      try:
        d = json.loads(line); got[(d["set"], d["fold"])] = d
      except json.JSONDecodeError:
        pass
  T = T0 + ["lgbm", "tabicl"]
  W, wins = 12, dict.fromkeys(T, 0)
  means, secs, spread = {t: [] for t in T}, dict.fromkeys(T, 0.0), {}
  print("%-34s" % "#data" + "".join("%-*s" % (W, t) for t in T))
  for f in order:
    d = {t: [] for t in T}
    for k, (a, s, _) in enumerate(folds[f]):
      for t, v, sec in zip(T0, a, s):        # the lisp arms, as recorded
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
        "%5.1f       " % (sum(means[t]) / max(1, len(means[t]))) for t in T))
  print("%-34s" % "#secs (all data)" + "".join("%5.1f       " % secs[t] for t in T))
  for f, d in spread.items():
    for t in T:
      print("%%folds %s %s %s" % (f, t, " ".join("%.1f" % v for v in d[t])))


def main():
  dat, jsonl = sys.argv[1], sys.argv[2]
  lines = open(dat).readlines()
  T0 = lines[0][1:].split()                  # "#names" header: the lisp arms
  folds = {}
  for line in lines[1:]:
    f, a, s, t = line.strip().split("|")
    folds.setdefault(f, []).append(([float(v) for v in a.split()],
                                    [float(v) for v in s.split()],
                                    [int(v) for v in t.split()]))
  order = list(folds)
  rows  = {f: len(load(f)[1]) for f in order}
  if "--report" not in sys.argv: run(folds, rows, jsonl)
  report(T0, order, folds, jsonl)


if __name__ == "__main__":
  mp.set_start_method("spawn")
  main()
