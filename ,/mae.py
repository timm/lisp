import sys, math
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
rows = [l.split() for l in open(sys.argv[1])]
rows = [(n, float(a), float(b), v) for n, a, b, v in rows]
rows.sort(key=lambda r: math.log(r[1]) - math.log(r[2]))   # the gap you see
names = [r[0] for r in rows]; fm = [r[1] for r in rows]; rm = [r[2] for r in rows]
wins = sum(r[3] == "win" for r in rows); losses = sum(r[3] == "loss" for r in rows)
ties = len(rows) - wins - losses
INK, INK2, GRID, SURF = "#0b0b0b", "#52514e", "#e8e7e3", "#fcfcfb"
BLUE, ORANGE = "#2a78d6", "#eb6834"
fig, ax = plt.subplots(figsize=(13, 5.2), dpi=150)
fig.patch.set_facecolor(SURF); ax.set_facecolor(SURF)
x = range(len(rows))
for ys, c, lab in [(rm, ORANGE, "rmeans (rm7)"), (fm, BLUE, "fmpsc3")]:
    ax.plot(x, ys, color=c, lw=2, solid_joinstyle="round", solid_capstyle="round",
            marker="o", ms=6, mfc=c, mec=SURF, mew=1.5, label=lab, zorder=3)
ax.set_yscale("log")
ax.set_xticks(list(x)); ax.set_xticklabels(names, rotation=90, fontsize=7, color=INK2)
ax.set_xlim(-0.7, len(rows) - 0.3)
ax.tick_params(axis="y", labelsize=8, colors=INK2); ax.tick_params(axis="x", length=0)
ax.grid(axis="y", which="major", color=GRID, lw=1); ax.set_axisbelow(True)
for s in ["top", "right", "left"]: ax.spines[s].set_visible(False)
ax.spines["bottom"].set_color(GRID)
ax.set_ylabel("median MAE over 20 seeds (log scale)", color=INK2, fontsize=9)
ax.set_title(f"MAE per regression data set, sorted by the gap fmpsc3 − rmeans "
             f"(on the log scale)\nfmpsc3 better on the left; {wins} win, "
             f"{ties} tie, {losses} loss (Same test)", loc="left", fontsize=11,
             color=INK)
leg = ax.legend(loc="upper left", frameon=False, fontsize=9)
for t in leg.get_texts(): t.set_color(INK)
fig.tight_layout(); fig.savefig(sys.argv[2], facecolor=SURF)
