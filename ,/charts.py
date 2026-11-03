#!/usr/bin/env python3
"""charts.py: the figures in rq.md, drawn from ~/tmp/cache.

Every point is one data set's median over 20 seeds.  An arm is a
<who>.csv in that set's cache dir, so a new learner joins a chart
by being run, not by being coded here.

usage: python3 ,/charts.py [clas|labels|top5|secs ...]   (all, if none)
"""
import os, sys, glob, statistics as st
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import min as M
from math import log, exp
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt

Cache = os.path.expanduser(os.getenv("CACHE", "~/tmp/cache"))
Docs  = os.getenv("DOCS") or os.path.join(os.path.dirname(
          os.path.dirname(os.path.abspath(__file__))), "docs")
Moot  = os.path.expanduser("~/gits/moot")

# one look per arm: colour, line style, marker, legend name
Look = {
  "nb":        ("#a6cee3", "-",  "o", "naive Bayes (self-tuned)"),
  "rmeans":    ("#a6cee3", "-",  "o", "rmeans, 45 labels"),
  "rmeansall": ("#1f5f9e", "-",  "o", "rmeans, every row"),
  "nb50":      ("#1f5f9e", "--", "D", "naive Bayes, 50 labels"),
  "nb72":      ("#a6cee3", "-",  "o", "naive Bayes (bins=7, m=2)"),
  "lgbm":      ("#d9661f", "-",  "o", "LightGBM"),
  "tabicl":    ("#6a3d9a", "-",  "s", "TabICLv2"),
  "tabpfn":    ("#e7298a", "--", "^", "TabPFN-3")}

def cells(dir, who, col):
  "Column COL of DIR's WHO.csv, one number per rep."
  f = f"{dir}/{who}.csv"
  if not os.path.exists(f): return None
  rows = [l.split(",") for l in open(f).read().splitlines() if l.strip()]
  at   = rows[0].index(col) if col in rows[0] else None
  if at is None: return None
  try:    return [float(r[at]) for r in rows[1:]]
  except (ValueError, IndexError): return None

def sets(kind, need):
  "Every cached KIND set holding all of NEED, as (name, dir)."
  out = []
  for d in sorted(glob.glob(f"{Cache}/{kind}/*")):
    if all(os.path.exists(f"{d}/{w}.csv") for w in need):
      out.append((os.path.basename(d), d))
  return out

def rows(name, kind):
  "How many rows that set's csv holds."
  f = glob.glob(f"{Moot}/{kind}/{name}.csv") + \
      glob.glob(f"{Moot}/{kind}/*/{name}.csv")
  if not f: return 0
  with open(f[0], errors="ignore") as h:
    h.readline(); return sum(1 for _ in h)

def klasses(name):
  "How many klasses that classify set has."
  f = glob.glob(f"{Moot}/classify/{name}.csv")
  if not f: return 0
  t = M.Tbl(f[0])
  c = [c for c in t.cols.y if c.txt.endswith("!")][-1]
  return len({r[c.at] for r in t.rows})

def fig(w=13, h=6):
  "One wide figure, 2600px across."
  f, ax = plt.subplots(figsize=(w, h), dpi=200)
  ax.spines[["top", "right"]].set_visible(False)
  ax.grid(axis="y", color="#eeeeee", zorder=0)
  return f, ax

def save(f, ax, name, title, xlab, ylab, rot=90, size=7):
  ax.set_title(title, fontsize=13, pad=18)
  ax.set_xlabel(xlab, fontsize=11, labelpad=12)
  ax.set_ylabel(ylab, fontsize=11)
  ax.tick_params(axis="x", labelsize=size, rotation=rot)
  f.tight_layout()
  f.savefig(f"{Docs}/{name}.png")
  plt.close(f)
  print(f"{Docs}/{name}.png")

def arms(need, extra=("tabicl", "tabpfn")):
  "NEED, plus whichever foundation models have actually run."
  return list(need) + [w for w in extra
                       if glob.glob(f"{Cache}/*/*/{w}.csv")
                       or glob.glob(f"{Cache}/*/{w}.csv")]

# ---- the four figures ------------------------------------------------
def clas():
  "pd, pf and d2h on moot/classify: one vote per klass, no weights."
  who  = arms(["nb72", "lgbm"], ("tabicl", "tabpfn"))
  S    = sets("classify", ["nb", "lgbm"])
  cols = [("pd", "pd %  (higher better)"), ("pf", "pf %  (lower better)"),
          ("d2h", "d2h  (lower better)")]
  med  = {(w, c): {n: st.fmean(cells(d, w, c) or [float("nan")])
                   for n, d in S} for w in who for c, _ in cols}
  only = os.getenv("ONLY")          # just the sets that arm changed
  if only:
    S = [(n, d) for n, d in S
         if cells(d, only, "acc") != cells(d, "nb", "acc")]
  grp = {n: (0 if klasses(n) == 2 else 1 if klasses(n) <= 5 else 2)
         for n, _ in S}
  S.sort(key=lambda nd: (grp[nd[0]], med[("lgbm", "d2h")][nd[0]]))
  names = [n for n, _ in S]
  edge  = [j for j in range(1, len(names))          # where a group ends
           if grp[names[j]] != grp[names[j - 1]]]
  near  = {"pd": 5, "pf": 5, "d2h": .05}   # "the same", in each unit
  tie   = {c: {n: bool(cells(d, "nb72", c)) and bool(cells(d, "tabicl", c))
                  and abs(st.fmean(cells(d, "nb72", c))
                        - st.fmean(cells(d, "tabicl", c))) <= near[c]
               for n, d in S} for c, _ in cols}
  f, ax = plt.subplots(3, 1, figsize=(13, 10), dpi=200, sharex=True,
                       gridspec_kw=dict(hspace=.28))
  for k, (c, ylab) in enumerate(cols):
    a = ax[k]
    a.spines[["top", "right"]].set_visible(False)
    a.grid(axis="y", color="#eeeeee", zorder=0)
    for j, n in enumerate(names):      # nb said nothing lgbm did not
      if tie[c][n]: a.axvspan(j - .5, j + .5, color="#ececec",
                                 lw=0, zorder=0)
    for j in edge: a.axvline(j - .5, color="#999999", lw=1, zorder=2)
    for w in who:
      ys = [med[(w, c)][n] for n in names]
      ok = [y for y in ys if y == y]
      col, ls, mk, lab = Look[w]
      a.plot(range(len(names)), ys, ls, color=col, marker=mk, ms=3, lw=1.6,
             label=lab, zorder=3)
    a.set_ylabel(ylab, fontsize=10)
    a.text(0, 1.015, "mean over sets:   " + "      ".join(
             "{} {:.3g} \u00b1 {:.2g}".format(Look[w][3], st.fmean(v),
               st.pstdev(v)) for w, v in
             ((w, [y for y in [med[(w, c)][n] for n in names] if y == y])
              for w in who)),
           transform=a.transAxes, ha="left", va="bottom", fontsize=10.5,
           color="#333333")
    if k == 0:
      for lo, hi, lab in zip([0] + edge, edge + [len(names)],
                             ("2 klasses", "3-5 klasses", "6+ klasses")):
        a.text((lo + hi - 1) / 2, .97, f"{lab}  (n={hi - lo})",
               transform=a.get_xaxis_transform(), ha="center", va="top",
               fontsize=9, color="#555555")
      a.plot([], [], lw=8, color="#ececec",
             label="naive Bayes (defaults) within 5 of TabICLv2")
      a.legend(frameon=False, fontsize=10, ncol=len(who) + 1,
               loc="lower center", bbox_to_anchor=(.5, 1.10))
  ax[0].set_title("Simple classifiers against a gradient booster and a"
                  " foundation model\n"
                  "(each score is the unweighted mean over the klasses;\neach point is one set's mean over 20 seeds)",
                  fontsize=13, pad=68)
  ax[2].set_xticks(range(len(names)))
  ax[2].set_xticklabels(names)
  ax[2].tick_params(axis="x", labelsize=7, rotation=90)
  ax[2].set_xlabel(f"{len(names)} moot/classify sets"
                   + (only and " where clustering ran" or "")
                   + ", sorted by LightGBM's d2h inside each group",
                   fontsize=11, labelpad=12)
  f.savefig(f"{Docs}/clas{only and '-' + only or ''}.png",
            bbox_inches="tight"); plt.close(f)
  print(f"{Docs}/clas{only and '-' + only or ''}.png")

def gain():
  "What clustering changed, on the sets its gate let it run."
  S = [(n, d) for n, d in sets("classify", ["nb", "lgbm"])
       if cells(d, "nbc", "acc")
       and cells(d, "nbc", "acc") != cells(d, "nb", "acc")]
  cols = [("pd", "pd %", 1), ("pf", "pf %", -1), ("d2h", "d2h", -1)]
  mu   = lambda d, w, c: st.fmean(cells(d, w, c))
  S.sort(key=lambda nd: mu(nd[1], "nb", "d2h") - mu(nd[1], "nbc", "d2h"))
  names = [n for n, _ in S]
  f, ax = plt.subplots(1, 3, figsize=(13, 6.5), dpi=200, sharey=True)
  for k, (c, lab, good) in enumerate(cols):
    a  = ax[k]
    a.spines[["top", "right"]].set_visible(False)
    gs = [mu(d, "nbc", c) - mu(d, "nb", c) for _, d in S]
    a.barh(range(len(gs)), gs, color=["#33a02c" if g * good > 0
             else "#d9661f" for g in gs], zorder=3, height=.7)
    a.axvline(0, color="#333333", lw=1, zorder=4)
    a.set_title(f"{lab}   mean {st.fmean(gs):+.3g}", fontsize=11)
    a.grid(axis="x", color="#eeeeee", zorder=0)
    a.set_xlabel("per cluster  -  one model", fontsize=9)
  ax[0].set_yticks(range(len(names)))
  ax[0].set_yticklabels([f"{n}  (k={klasses(n)})" for n in names],
                        fontsize=8)
  f.suptitle("Naive Bayes, one model per cluster: what it changed\n"
             f"({len(names)} of 69 classify sets passed the gate;"
             " green is better)", fontsize=13)
  f.tight_layout()
  f.savefig(f"{Docs}/gain.png", bbox_inches="tight"); plt.close(f)
  print(f"{Docs}/gain.png")

def labels():
  "Raw MAE on moot/regression: 45 labels against everyone's all."
  who = arms(["rmeans", "rmeansall", "lgbm"])
  S   = sets("regression", ["rmeans", "rmeansall", "lgbm"])
  med = {w: {n: st.median(cells(d, w, "mae") or [float("nan")])
             for n, d in S} for w in who}
  tie = {n: M.same(cells(d, "rmeans", "mae"), cells(d, "lgbm", "mae"))
         for n, d in S}
  S.sort(key=lambda nd: med["rmeansall"][nd[0]] - med["lgbm"][nd[0]])
  names = [n for n, _ in S]
  f, ax = fig(13, 6)
  for j, n in enumerate(names):
    if tie[n]: ax.axvspan(j - .5, j + .5, color="#e8e8e8", zorder=0)
  for w in who:
    ys = [med[w][n] for n in names]
    if not any(y == y for y in ys): continue   # not run yet: no legend line
    c, ls, mk, lab = Look[w]
    ax.plot(range(len(names)), ys, ls, color=c, marker=mk, ms=2.5, lw=1.6,
            label=lab, zorder=3)
  ax.set_yscale("log")
  ax.set_xticks(range(len(names)))
  ax.set_xticklabels([f"{n} ({int(.8*rows(n,'regression'))})"
                      for n in names])
  for t, n in zip(ax.get_xticklabels(), names):
    if tie[n]: t.set_color("#999999")
  ax.plot([], [], lw=8, color="#e8e8e8",
          label="45 labels == LightGBM (cohen, cliffs, ks)")
  ax.legend(frameon=False, fontsize=10, loc="upper left")
  save(f, ax, "labels", "rmeans on 45 labels vs every row of the 80%"
       " split, against LightGBM",
       f"{len(names)} moot/regression sets (training rows available),"
       " sorted by rmeans(all) - lgbm",
       "raw MAE, median of 20 seeds  (log)")

def top5():
  "Label only each arm's top 5: how good is the best of those?"
  who = arms(["rmeans", "rmeansall", "lgbm"])
  S   = sets("regression", ["rmeans", "rmeansall"])
  med = {w: {n: (lambda v: st.median(v) if v else float("nan"))(
                 cells(d, w, "rank")) for n, d in S} for w in who}
  S.sort(key=lambda nd: med["rmeans"][nd[0]])
  names = [n for n, _ in S]
  f, ax = fig(13, 6)
  ax.axhspan(0, 5, color="#eef4ee", zorder=0)
  ax.axhline(5, color="#2d7a2d", ls="--", lw=1.2, zorder=2,
             label="the 5% line")
  for w in who:
    ys = [med[w][n] for n in names]
    ok = [y for y in ys if y == y]
    if not ok: continue
    c, ls, mk, lab = Look[w]
    ax.plot(range(len(names)), ys, ls, color=c, marker=mk, ms=2.5, lw=1.6,
            label=f"{lab}  ({sum(1 for y in ok if y<=5)}/{len(ok)}"
                  " inside 5%)", zorder=3)
  ax.set_xticks(range(len(names)))
  ax.set_xticklabels([f"{n} ({int(.2*rows(n,'regression'))})"
                      for n in names])
  ax.set_ylim(bottom=0)
  ax.legend(frameon=False, fontsize=10, loc="upper left")
  save(f, ax, "top5", "Label only the top 5 of each ranking:"
       " how good is the best one?",
       "data sets (test rows), sorted by the 45-label result",
       "best of our top 5, as a percentile of the test set\n"
       "(0 = we found the very best row)")

def rate(v):
  "A speed-up reads as 651x; a slow-down needs its decimals."
  return f"{v:,.0f}x" if v >= 10 else f"{v:.2g}x"

def secs():
  "How many of this arm's runs fit inside one LightGBM run."
  mine = {"classify": "nb", "regression": "rmeans"}
  who  = arms([], ("tabicl", "tabpfn")) + ["jiffy"]
  cur  = {w: [] for w in who}
  n    = 0
  for kind, ours in mine.items():
    for name, d in sets(kind, ["lgbm", ours]):
      base = st.median(cells(d, "lgbm", "secs"))
      n   += 1
      for w in who:
        v = cells(d, ours if w == "jiffy" else w, "secs")
        if v: cur[w].append(base / st.median(v))
  f, ax = fig(13, 5.8)
  Look["jiffy"] = ("#1f5f9e", "-", "", "jiffy (luajit)")
  for w in who:
    xs = sorted(cur[w])
    if not xs: continue
    c, ls, mk, lab = Look[w]
    ax.plot(range(1, len(xs) + 1), xs, ls, color=c, lw=2,
            label=f"{lab}  (median {rate(st.median(xs))})", zorder=3)
  ax.set_yscale("log")
  ax.axhline(1, color="#333333", lw=1)
  ax.legend(frameon=False, fontsize=10, loc="upper left")
  save(f, ax, "secs", "One estimator against LightGBM and a"
       " foundation model",
       f"{n} moot classify+regression sets, each curve sorted on its own",
       "LightGBM seconds / this arm's  (log)", rot=0, size=9)

def raw():
  "Absolute seconds per run, each task on its own panel."
  mine = (("classify", "nb72"), ("regression", "rmeans"))
  who  = arms(["lgbm"], ("tabicl", "tabpfn")) + ["jiffy"]
  Look["jiffy"] = ("#1f5f9e", "-", "o", "jiffy (luajit)")
  f, ax = plt.subplots(1, 2, figsize=(13, 5.8), dpi=200, sharey=True)
  for k, (kind, ours) in enumerate(mine):
    a, cur = ax[k], {w: [] for w in who}
    S = sets(kind, ["lgbm", ours])
    for name, d in S:
      for w in who:
        v = cells(d, ours if w == "jiffy" else w, "secs")
        if v: cur[w].append(st.fmean(v))
    a.spines[["top", "right"]].set_visible(False)
    a.grid(axis="y", color="#eeeeee", zorder=0)
    for w in who:
      xs = sorted(cur[w])
      if not xs: continue
      c, _, _, lab = Look[w]
      a.plot(range(1, len(xs) + 1), xs, "-", color=c, lw=2.2, zorder=3,
             label=f"{lab}   total {sum(xs):,.0f}s")
    a.set_yscale("log")
    for y, t in ((1, "1 second"), (60, "1 minute")):
      a.axhline(y, color="#cccccc", lw=1, zorder=1)
      if k == 0: a.text(1, y, " " + t, fontsize=8, color="#999999",
                        va="bottom")
    a.legend(frameon=False, fontsize=9, loc="upper left")
    a.set_title(f"moot/{kind}   ({len(S)} sets)", fontsize=11)
    a.set_xlabel("sets, each curve sorted on its own", fontsize=10)
  ax[0].set_ylabel("seconds for one fit and predict,\n"
                   "mean of 20 seeds  (log)", fontsize=11)
  f.suptitle("What one fit and predict costs", fontsize=13)
  f.tight_layout()
  f.savefig(f"{Docs}/raw.png", bbox_inches="tight"); plt.close(f)
  print(f"{Docs}/raw.png")

def cost():
  "Seconds against training rows: how each arm grows with the data."
  mine = {"classify": "nb", "regression": "rmeans"}
  who  = arms(["lgbm"], ("tabicl", "tabpfn")) + ["jiffy"]
  Look["jiffy"] = ("#1f5f9e", "-", "o", "jiffy (luajit)")
  pts  = {w: [] for w in who}
  for kind, ours in mine.items():
    for name, d in sets(kind, ["lgbm", ours]):
      n = .8 * rows(name, kind)
      if n < 1: continue
      for w in who:
        v = cells(d, ours if w == "jiffy" else w, "secs")
        if v: pts[w].append((n, st.fmean(v)))
  f, ax = fig(13, 6.5)
  for w in who:
    xy = sorted(pts[w])
    if len(xy) < 2: continue
    xs = [a for a, _ in xy]; ys = [b for _, b in xy]
    lx = [log(a) for a in xs]; ly = [log(b) for b in ys]
    mx, my = st.fmean(lx), st.fmean(ly)       # least squares in log-log:
    v = sum((a - mx) ** 2 for a in lx)        # the slope is the exponent
    k = sum((a - mx) * (b - my) for a, b in zip(lx, ly)) / v if v else 0
    c, _, mk, lab = Look[w]
    ax.plot(xs, ys, "", color=c, marker=mk, ms=3.5, lw=0, alpha=.55,
            zorder=3)
    ax.plot(xs, [exp(my + k * (a - mx)) for a in lx], "-", color=c, lw=2,
            label=f"{lab}   secs ~ rows^{k:.2f}", zorder=4)
  ax.set_xscale("log"); ax.set_yscale("log")
  ax.legend(frameon=False, fontsize=10, loc="upper left")
  save(f, ax, "cost", "What one fit and predict costs, as the data grows",
       "training rows (80% of the set)  (log)",
       "seconds, mean of 20 seeds  (log)", rot=0, size=9)

if __name__ == "__main__":
  jobs = sys.argv[1:] or ["clas", "labels", "top5", "secs", "cost", "raw"]
  for j in jobs: globals()[j]()
