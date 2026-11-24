#!/usr/bin/env python3 -B
"""loc: the lines-of-code figure (loc.png, loc.pdf), two squares whose AREAS
are in proportion to the line counts.

The Lisp count is taken from the files, not typed in, so the figure cannot
drift from the code the way the old hard-coded 346 did.  It counts bald.yue, the
file printed in the paper: learner and rig together, non-blank and
non-comment.

LightGBM's number is from its own repo at 7810ece: src/ plus
include/LightGBM, C++ only, no cuda.  There is no checkout here to recount,
so it stays a constant -- recount it if that ever changes.

usage: python3 loc.py            (writes loc.png and loc.pdf beside this file)
"""
import os, re
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt

Here = os.path.dirname(os.path.abspath(__file__))
Lib  = ("bald.yue",)
Lgbm = 36328


def loc(f):
  "Non-blank, non-comment lines (-- starts a Yue comment)."
  return sum(1 for l in open(os.path.join(Here, f))
             if not re.match(r"^\s*(--.*)?$", l))


counts = {f: loc(f) for f in Lib}
lisp   = sum(counts.values())
print(" ".join(f"{f}={n}" for f, n in counts.items()), f"total={lisp}",
      f"lgbm={Lgbm}", f"ratio={lisp/Lgbm:.3%}")

(ln, lv, lc), (gn, gv, gc) = ("this code", lisp, "#e8613c"), ("lgbm", Lgbm, "#2a6fdb")
r = (lv / gv) ** .5                                  # areas ~ LOC
f2, lx = plt.subplots(figsize=(1.2, 1.9))
lx.add_patch(plt.Rectangle((0, 0), 1, 1, color=gc))
lx.add_patch(plt.Rectangle((0, 0), r, r, color=lc, ec="white", lw=.6))
lx.text(.5, .62, f"{gn}\n{gv:,}\nLOC", ha="center", va="center",
        color="white", fontsize=8, weight="bold")
lx.annotate(f"{ln}\n{lv} LOC", (r, r), (.1, .22), color="white", fontsize=7,
            weight="bold", arrowprops=dict(arrowstyle="-", color="white", lw=.6))
lx.set_xlim(0, 1); lx.set_ylim(0, 1); lx.set_xticks([]); lx.set_yticks([])
for s in lx.spines.values(): s.set_visible(False)
f2.set_facecolor("white"); f2.tight_layout(pad=.1)
f2.savefig(os.path.join(Here, "loc.png"), dpi=300)
f2.savefig(os.path.join(Here, "loc.pdf"))
