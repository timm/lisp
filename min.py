#!/usr/bin/env python3 -B
"""
min.py: a good guess from a few dozen labels.
Range means, shrunk to the grand mean, weighed by variance.
(c) 2026 Tim Menzies <timm@ieee.org> MIT license.

Options:
      -Bins=5          numeric columns get this many bins
      -M=2             m-estimate: prior weight for a thin bin
      -Budget=50       labels, all in
      -Check=5         of those, saved for the test rows
      -Repeats=20      how many train/test splits
      -P=2             minkowski coefficient
      -Seed=1234567891 random number seed
      -File=~/gits/moot/optimize/misc/auto93.csv

One file, four layers.  Each calls the layers above it and
knows nothing of the ones below:

      core     columns, tables, bins.  Calls nothing.
      rmeans   one cut per column: its bins, and its weight.
      rig      split, train, guess, rank the unseen rows.
      stats    cohen, cliffs, ks.  A leaf: nothing calls it.

"""
# pylint: disable=bad-indentation,multiple-statements
# pylint: disable=invalid-name,ungrouped-imports
# pylint: disable=no-member,assigning-non-slot  # `type()` structs
from typing import Any
import os, re, sys, traceback          # pylint: disable=C0410
from math import exp, log2, sqrt
from collections.abc import Callable, Iterable, Iterator

#-- types --------------------------------------------------
QTY  = int | float                    # any number
ATOM = QTY | str                      # one cell; "?" = unknown
ROW  = list[ATOM]
ROWS = list[ROW]
NUMS = list[QTY]

#-- misc ---------------------------------------------------
def say(x: Any, p: int = 2) -> str:
  "Floats get P decimals; dicts list their keys."
  if isinstance(x,float): x= f"{x:.{p}f}".rstrip("0").rstrip(".")
  if isinstance(x, dict):
    x= "{"+", ".join(f"{k}: {say(v,p)}" for k,v in x.items())+"}"
  return str(x)

def struct(s: str, **d: Any) -> type:
  """A __slots__ class, in one line.  A default that can be called is
  called, so every object gets its own list/dict (not a shared one)."""
  def init(i, **kw):
    for k, v in d.items():
      setattr(i, k, kw[k] if k in kw else v() if callable(v) else v)
  return type(s, (),
              dict(__slots__ = tuple(d), __init__  = init,
                   __repr__  = lambda i:say({k:getattr(i,k) for k in d})))

def opts(doc: str) -> dict:
  "The -key=value pairs in DOC."
  return {k: atom(v) for k,v in re.findall(r"-(\w+)=(\S+)", doc)}

def atom(s: str) -> ATOM:
  "'22' -> 22. '3.1' -> 3.1. '?' -> '?'. 'x' -> 'x'."
  try: return int(s)
  except ValueError:
    try: return float(s)
    except ValueError: return s

def csv(file: str) -> Iterator[ROW]:
  "ROWS of FILE, each cell coerced."
  with open(os.path.expanduser(file), encoding="utf-8-sig") as f:
    for line in f:
      if line.strip(): yield [atom(s.strip()) for s in line.split(",")]

def mid(vs: NUMS) -> QTY:
  "The median: the upper middle of a sorted copy."
  return sorted(vs)[len(vs)//2]

#-- structs ------------------------------------------------
# UPPER is the struct.  A default that is a type gets called, so
# `cols=COLS` gives every TBL its own COLS.
NUM  = struct("NUM",  txt=" ", at=0, n=0, mu=0, m2=0, sd=0, goal=1)
SYM  = struct("SYM",  txt=" ", at=0, n=0, has=dict)
COLS = struct("COLS", names=list, all=dict, x=list, y=list)
TBL  = struct("TBL",  rows=list, cols=COLS)
CUT  = struct("CUT",  bins=dict, w=0)          # one column's bins
MODEL= struct("MODEL", all=NUM, cuts=dict, cols=list)

Num, Sym, Cut, Model = NUM, SYM, CUT, MODEL    # Tbl, Cols: below
COL = NUM | SYM

#-- columns ------------------------------------------------
def Col(txt: str = " ", at: int = 0) -> COL:
  "Factory that returns NUM or SYM.  A trailing `-` minimises."
  return (Num(at=at, txt=txt, goal=0 if txt[-1] == "-" else 1)
          if txt[0].isupper() else Sym(at=at, txt=txt))

def add(col: COL, v: ATOM) -> None:
  "Show V to COL.  `?` changes nothing."
  if v != "?":
    col.n += 1
    if type(col) is Sym: col.has[v] = col.has.get(v, 0) + 1
    else:
      d = v - col.mu                      # Welford, for `div`
      col.mu += d / col.n
      col.m2  = max(0, col.m2 + d * (v - col.mu))
      col.sd  = 0 if col.n < 2 else (col.m2/(col.n-1))**.5

def adds(vs: Iterable[ATOM], col: COL | None = None) -> COL:
  "Every item of VS into COL."
  col = Num() if col is None else col
  for v in vs: add(col, v)
  return col

def div(col: COL) -> float:
  "Spread: entropy, or standard deviation."
  return (-sum(p*log2(p) for v in col.has.values() if (p := v/col.n) > 0)
          if type(col) is Sym else col.sd)

def norm(col: COL, v: ATOM) -> ATOM:
  "To 0..1, by the logistic curve.  Symbols do not scale."
  if type(col) is Sym or v == "?": return v
  z = max(-3, min(3, (v - col.mu) / (1e-32 + div(col))))
  return 1 / (1 + exp(-1.7 * z))

def bin(col: COL, v: ATOM) -> ATOM:
  "Which of `the.Bins` ranges holds V?  A symbol is its own bin."
  if type(col) is Sym or v == "?": return v
  return min(the.Bins, 1 + int(the.Bins * norm(col, v)))

#-- tables -------------------------------------------------
def Tbl(src: Iterable[ROW] | str) -> TBL:
  "A file name, or any rows whose first names the columns."
  src = csv(src) if isinstance(src, str) else iter(src)
  tbl = TBL(cols=Cols(next(src)))
  for row in src: addRow(tbl, row)
  return tbl

def Cols(names: list[str]) -> COLS:
  "Column roles, from a header: X skip, +-! goal, else x."
  cols = COLS(names=names)
  for at, s in enumerate(names):
    if s[-1] == "X": continue            # skip me entirely
    cols.all[at] = col = Col(s, at)      # keyed by at: X leaves gaps
    (cols.y if s[-1] in "+-!" else cols.x).append(col)
  return cols

def addRow(tbl: TBL, row: ROW) -> ROW:
  "Keep ROW; show its cells to my columns."
  tbl.rows += [row]
  for at, col in tbl.cols.all.items(): add(col, row[at])
  return row

def clone(tbl: TBL, rows: ROWS = None) -> TBL:
  "An empty copy of TBL, plus ROWS."
  return Tbl([tbl.cols.names] + (rows or []))

def ydist(tbl: TBL, row: ROW) -> float:
  "How far ROW's goals are from the best they could be; 0 is best."
  ys = [abs(norm(c, row[c.at]) - c.goal) for c in tbl.cols.y]
  return (sum(y**the.P for y in ys) / len(ys)) ** (1/the.P)

#-- rmeans -------------------------------------------------
def cut(col: COL, rows: ROWS, y: Callable, all1: NUM) -> CUT:
  "COL's y per bin over ROWS, and the variance those bins cut."
  a = Cut()
  for r in rows:
    b = bin(col, r[col.at])
    add(a.bins.setdefault(b, Num()), y(r))
  a.w = vcut(a, all1)
  return a

def train(tbl: TBL, rows: ROWS, y: Callable = None) -> MODEL:
  "rmeans: one cut per x column, over a clone of ROWS."
  y   = y or (lambda r: ydist(tbl, r))
  i   = clone(tbl, rows)
  all1= adds(y(r) for r in i.rows)
  return Model(all=all1, cols=i.cols.x,
               cuts={c.at: cut(c, i.rows, y, all1) for c in i.cols.x})

def vcut(a: CUT, all1: NUM) -> float:
  "How much variance A's bins cut from ALL1."
  return max(0, div(all1)**2
             - sum(v.n/all1.n * div(v)**2 for v in a.bins.values()))

def hit(m: MODEL, col: COL, row: ROW) -> tuple:
  "The num ROW's COL cell falls in, and COL's cut -- or None."
  a = m.cuts.get(col.at)
  return (a.bins.get(bin(col, row[col.at])), a) if a else (None, None)

def guess(m: MODEL, row: ROW) -> float:
  "rmeans guess: ROW's bin means, m-estimated, weighed by variance."
  s = w = 0; mu = m.all.mu
  for col in m.cols:
    v, a = hit(m, col, row)
    if v and a.w > 0:
      s += a.w * (v.n*v.mu + the.M*mu) / (v.n + the.M); w += a.w
  return s/w if w > 0 else mu

#-- rig ----------------------------------------------------
def rint(n: int) -> int:
  "1..N, from the.Seed; the same stream in lua, lisp and here."
  the.Seed = (16807 * the.Seed) % 2147483647
  return 1 + int(n * the.Seed / 2147483647)

def shuffle(t: ROWS) -> ROWS:
  "T, in place, Fisher-Yates, so every port sees one order."
  for i in range(len(t), 1, -1):
    j = rint(i); t[i-1], t[j-1] = t[j-1], t[i-1]
  return t

def split(tbl: TBL, at: float = .8) -> tuple[ROWS, ROWS]:
  "TBL's rows, shuffled, cut AT."
  rows = shuffle(tbl.rows[:])
  n    = int(at * len(rows))
  return rows[:n], rows[n:]

def ranked(m: MODEL, rows: ROWS) -> ROWS:
  "ROWS, best guess first."
  return sorted(rows, key=lambda r: guess(m, r))

def top(tbl: TBL, m: MODEL, rows: ROWS, y: Callable) -> float:
  """Label the top `the.Check` of the ranking, keep the best of
  those, and say where it really ranks (0 = the best row of all)."""
  ys   = sorted(y(r) for r in rows)
  best = min(y(r) for r in ranked(m, rows)[:the.Check])
  return 100 * ys.index(best) / max(1, len(rows) - 1)

#-- stats --------------------------------------------------
def cohen(xs: NUMS, ys: NUMS) -> float:
  "Gap between the means, in pooled standard deviations."
  a, b = adds(xs), adds(ys)
  sd = sqrt(((a.n-1)*div(a)**2 + (b.n-1)*div(b)**2) / (a.n+b.n-2))
  return abs(a.mu - b.mu) / (sd + 1e-32)

def cliffs(xs: NUMS, ys: NUMS) -> float:
  "How often XS and YS out-rank each other.  Sorted lists in."
  gt = lt = j = k = 0
  for x in xs:
    while j < len(ys) and ys[j] <  x: j += 1; k = j
    while k < len(ys) and ys[k] == x: k += 1
    gt += j; lt += len(ys) - k
  return abs(gt - lt) / (len(xs) * len(ys))

def ks(xs: NUMS, ys: NUMS) -> float:
  "Biggest gap between two cdfs, in crits.  Sorted lists in."
  n, m, p, q, d = len(xs), len(ys), 0, 0, 0
  while p < n and q < m:
    v = min(xs[p], ys[q])
    while p < n and xs[p] == v: p += 1
    while q < m and ys[q] == v: q += 1
    d = max(d, abs(p/n - q/m))
  return d / sqrt((n + m) / (n * m))

def same(xs: NUMS, ys: NUMS) -> bool:
  "Do all three tests call XS and YS the same?  Any order in."
  a, b = sorted(xs), sorted(ys)
  return (cohen(a,b) <= .35 and cliffs(a,b) <= .195
          and ks(a,b) <= 1.36)

#-- demos --------------------------------------------------
def eg_h(): print(__doc__)

def eg_the(): print(the)

def eg_cols():
  "Every column of the default file, summarised."
  t = Tbl(the.File)
  print(f"rows={len(t.rows)} x={len(t.cols.x)} y={len(t.cols.y)}")
  for c in t.cols.all.values():
    print(f"  {c.txt:12} {'sym' if type(c) is Sym else 'num'}"
          f" n={c.n:5} mid={say(mid([0]) if 0 else c.mu)}"
          f" div={say(div(c))}")

def eg_train():
  "Mean y per bin, after `the.Budget - the.Check` labels."
  t = Tbl(the.File)
  tr, _ = split(t)
  m = train(t, tr[:the.Budget - the.Check])
  for c in m.cols:
    a = m.cuts[c.at]
    for b, v in sorted(a.bins.items(), key=lambda z: str(z[0])):
      e = (v.n*v.mu + the.M*m.all.mu) / (v.n + the.M)
      print(f"  {c.txt:12} bin {str(b):8} n={v.n:3}"
            f" y={say(v.mu,3)} est={say(e,3)} w={say(a.w,4)}")

def eg_secs():
  "Train on the budget, then rank every test row.  3 seeds."
  import time
  t = Tbl(the.File); tot = 0
  for s in range(3):
    the.Seed = 1234567891 + s
    tr, te = split(t)
    tr2 = clone(t, tr); y = lambda r: ydist(tr2, r)
    t0  = time.time()
    m   = train(t, tr[:the.Budget - the.Check], y)
    for r in te: guess(m, r)
    tot += time.time() - t0
  print(f"{os.path.basename(the.File):20} rows={len(t.rows):6}"
        f" secs={tot/3:.4f}")

def eg_rank():
  "Label the top five of the ranking: where does the best one sit?"
  t = Tbl(the.File)
  out = []
  for s in range(the.Repeats):
    the.Seed = 1234567891 + s
    tr, te = split(t)
    tr2= clone(t, tr)                          # train rows only
    y  = lambda r: ydist(tr2, r)
    out += [top(t, train(t, tr[:the.Budget - the.Check], y), te, y)]
  print(f"{os.path.basename(the.File):24} test={len(t.rows)//5:6}"
        f" top{the.Check}={say(mid(out))}%")

#-- start up -----------------------------------------------
Settings = struct("Settings", **opts(__doc__))
the      = Settings()

def run(f: Callable) -> int:
  "Run F. Demos may poke `the`, so always put it back."
  b4 = {k: getattr(the,k) for k in the.__slots__}
  try:                   f()
  except Exception as e: traceback.print_exception(e); return 1
  finally:               [setattr(the,k,v) for k,v in b4.items()]
  return 0

def main(args: list[str], funcs: dict) -> None:
  """Dispatch -x or --x to FUNCS' eg_x; -key val updates `the`.
  Dashes are stripped, so -h and --h are the same ask."""
  bad = 0
  while args:
    s = args.pop(0); k = s.lstrip("-")
    if   "eg_" + k in funcs: bad += run(funcs["eg_" + k])
    elif hasattr(the, k):    setattr(the, k, atom(args.pop(0)))
    else: print(f"unknown arg: {s}")
  sys.exit(bad)

if __name__ == "__main__": main(sys.argv[1:], globals())
