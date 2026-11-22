#!/usr/bin/env python3 -B
"""plot: ens.png from lgbm.py output: median AUC over folds per set (solid), p80-p20 over folds (dashed), LOC + secs bars.
LOC (2026-11-22): lisp = ai+lithp+hair.lisp (whole lib, not just forest);
lgbm = LightGBM 7810ece src/ + include/LightGBM, C++, no cuda.
Usage: python3 plot.py ens.txt ens.png"""
import sys, numpy as np, matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
L=open(sys.argv[1]).read().splitlines()
sec=[float(v) for v in [l for l in L if l.startswith("#secs")][0].split()[3:6]]
rows=[l.split() for l in L if l.strip() and l[0] not in "#%"]
F={}
for l in L:
  if l.startswith("%folds"): _,f,t,*v=l.split(); F[(f,t)]=[float(x) for x in v]
TK=["1:q8","q8","lgbm"]
d={r[0]:[float(np.median(F[(r[0],t)])) for t in TK] for r in rows}
sp={r[0]:[float(np.subtract(*np.percentile(F[(r[0],t)],[80,20]))) for t in TK] for r in rows}
order=list(d)
G=[("numeric only",order[0:10]),("symbolic only",order[10:20]),
   ("mixed",order[20:30]),("mixed, >3000 rows",order[30:39])]
T=[("lgbm(100)",2,"#2a6fdb","o"),("lisp(10)",1,"#e8613c","s"),("lisp(1)",0,"#1aaa7a","^")]
fig=plt.figure(figsize=(26,7.5)); gs=fig.add_gridspec(1,5,width_ratios=[2.6,.6,20,.6,2.6],wspace=0)
lx=fig.add_subplot(gs[0]); ax=fig.add_subplot(gs[2]); bx=fig.add_subplot(gs[4]); x0=0; ticks=[];labs=[]
for gi,(g,fs) in enumerate(G):
  fs=sorted(fs,key=lambda f:d[f][2]); xs=np.arange(x0,x0+len(fs))
  for name,j,c,m in T:
    ys=[d[f][j] for f in fs]
    lab=f"{name:<10}{np.mean([d[f][j] for f in order]):6.1f}{np.mean([sp[f][j] for f in order]):10.1f}"
    ax.plot(xs,ys,color=c,marker=m,lw=2.2,ms=6,label=lab if gi==0 else None)
    iq=[sp[f][j] for f in fs]
    ax.plot(xs,iq,color=c,ls="--",lw=1.6,marker=m,ms=3)
  mu=" ".join(f"{n} {np.mean([d[f][j] for f in fs]):.1f}" for n,j,_,_ in T)
  pr=" ".join(f"{n.split()[0]} {np.percentile([d[f][j] for f in fs],20):.0f}-{np.percentile([d[f][j] for f in fs],80):.0f}" for n,j,_,_ in T)
  ax.text(xs.mean(),45,f"{g} (n={len(fs)})\nmean 50th: {mu}",ha="center",va="center",fontsize=11)
  ticks+=list(xs); labs+=fs; x0+=len(fs)+1
  if gi<3: ax.axvline(x0-1,color="#555",lw=1)
ax.set_xticks(ticks); ax.set_xticklabels(labs,rotation=50,ha="right",fontsize=11)
ax.set_ylim(0,100); ax.set_xlim(-.7,x0-1.3); ax.set_yticks(range(0,101,10))
ax.grid(axis="y",color="#ddd"); ax.set_ylabel("AUC (0..100), 5x2 folds",color="#555")

for s in ("top","right"): ax.spines[s].set_visible(False)
lg=ax.legend(title=(f"{'':<15}{'50th':>6}{'80th-20th':>10}").replace(" ","\u00a0"),loc="lower center",bbox_to_anchor=(.635,.17),
   frameon=False,prop={"family":"monospace","size":11},title_fontproperties={"family":"monospace","size":11})
lg._legend_box.align="left"
ax.text(.635,.15,"per set, over 10 folds: 50th (solid), 80th-20th (dashed);\ntable = mean over all 39 sets",transform=ax.transAxes,ha="center",va="top",fontsize=9,color="#555")
ax.set_title("lgbm(100 boosted trees) vs lisp(10 bagged trees: q8 bins, sqrt(#x) cols, cap 200) vs lisp(1 tree); sorted by lgbm within each group",loc="left",fontsize=13)
bt=sorted([(sec[j],n,c) for n,j,c,_ in T])
bx.bar(range(3),[b[0] for b in bt],color=[b[2] for b in bt],width=.65)
for i,b in enumerate(bt): bx.text(i,b[0]+1,f"{b[0]:.0f}s",ha="center",fontsize=11)
bx.set_xticks(range(3)); bx.set_xticklabels([b[1] for b in bt],fontsize=11)
bx.set_title("total secs, all 39 sets\ntrain+predict, 10 folds\neach learner on 1 core of 2\nIntel Xeon @ 2.10GHz, 7.8 GB RAM\n(cloud VM, run serially)",fontsize=10)
bx.set_yticks([]); bx.set_facecolor("#fcfcfa")
for sp in ("top","right","left"): bx.spines[sp].set_visible(False)
LOC=[("lisp",346,"#e8613c"),("lgbm",36328,"#2a6fdb")]  # see docstring
(ln,lv,lc),(gn,gv,gc)=LOC; r=(lv/gv)**.5          # areas ~ LOC
lx.add_patch(plt.Rectangle((0,0),1,1,color=gc)); lx.add_patch(plt.Rectangle((0,0),r,r,color=lc,ec="white",lw=1))
lx.text(.5,.55,f"{gn}\n{gv:,}",ha="center",va="center",color="white",fontsize=14,weight="bold")
lx.annotate(f"{ln} {lv:,}",(r,r),(r+.08,r+.18),color="white",fontsize=12,weight="bold",
            arrowprops=dict(arrowstyle="-",color="white"))
lx.set_xlim(0,1); lx.set_ylim(0,1); lx.set_aspect("equal"); lx.set_xticks([]); lx.set_yticks([])
lx.set_title("lines of code\n(area; no blanks/comments)",fontsize=11); lx.set_facecolor("#fcfcfa")
for sp in lx.spines.values(): sp.set_visible(False)
fig.set_facecolor("#fcfcfa"); ax.set_facecolor("#fcfcfa"); plt.tight_layout(); plt.savefig(sys.argv[2],dpi=120)
