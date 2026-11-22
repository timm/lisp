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
G=[("numeric attributes",order[0:10]),("symbolic attributes",order[10:20]),
   ("mixed attributes",order[20:30]),("large (3,300 ... 49,000 rows)",order[30:39])]
T=[("lgbm(100)",2,"#2a6fdb","o"),("lisp(10)",1,"#e8613c","s"),("lisp(1)",0,"#1aaa7a","^")]
fig=plt.figure(figsize=(26,12)); gs=fig.add_gridspec(1,5,width_ratios=[3.4,1.1,20,.6,2.8],wspace=0)
lx=fig.add_subplot(gs[0]); ax=fig.add_subplot(gs[2]); bx=fig.add_subplot(gs[4]); x0=0; ticks=[];labs=[]; H=[]
for gi,(g,fs) in enumerate(G):
  fs=sorted(fs,key=lambda f:d[f][2]); xs=np.arange(x0,x0+len(fs))
  for name,j,c,m in T:
    ys=[d[f][j] for f in fs]
    lab=f"{name:<10}{np.mean([d[f][j] for f in order]):8.1f}{np.mean([sp[f][j] for f in order]):13.1f}"
    ax.plot(xs,ys,color=c,marker=m,lw=2.2,ms=7)
    if gi==0: H.append(((plt.Line2D([],[],color=c,lw=2.5,marker=m,ms=7),plt.Line2D([],[],color=c,lw=2,ls="--")),lab))
    iq=[sp[f][j] for f in fs]
    ax.plot(xs,iq,color=c,ls="--",lw=2,marker=m,ms=4)
  mu=" / ".join(f"{np.mean([d[f][j] for f in fs]):.1f}" for n,j,_,_ in T)
  pr=" ".join(f"{n.split()[0]} {np.percentile([d[f][j] for f in fs],20):.0f}-{np.percentile([d[f][j] for f in fs],80):.0f}" for n,j,_,_ in T)
  ax.text(xs.mean(),52,f"n={len(fs)}\n50th: {mu}",ha="center",va="center",fontsize=15)
  ax.text(xs.mean(),101.5,g,ha="center",va="bottom",fontsize=17)
  ticks+=list(xs); labs+=fs; x0+=len(fs)+1
  if gi<3: ax.axvline(x0-1,color="#555",lw=1)
ax.set_xticks(ticks); ax.set_xticklabels(labs,rotation=50,ha="right",fontsize=14)
ax.set_ylim(0,100); ax.set_xlim(-.7,x0-1.3); ax.set_yticks(range(0,101,10))
ax.set_yticks(range(5,100,10),minor=True); ax.tick_params(axis="y",which="minor",length=0)
ax.grid(axis="y",which="major",color="#ddd"); ax.grid(axis="y",which="minor",color="#b0b0b0",ls=(0,(4,4)),lw=1); ax.set_ylabel("AUC (0..100), 5x2 folds",color="#555",fontsize=14); ax.tick_params(axis="y",labelsize=13)

for s in ("top","right"): ax.spines[s].set_visible(False)
from matplotlib.legend_handler import HandlerTuple
# legend as a table in panel 3: name | solid 50th | dashed (80-20)th
X0=22; mono=dict(family="monospace",fontsize=14)
ax.add_patch(plt.Rectangle((X0-.5,22.6-4.2),9.8,25,fc="#fcfcfa",ec="none",zorder=1.5))
ax.text(X0+5.0,37,"50th\nmedian",ha="right",va="bottom",**mono)
ax.text(X0+9.0,37,"(80-20)th\n~1.7 sd",ha="right",va="bottom",**mono)
for k,(h,lab) in enumerate(H):
  y=33-4.2*k; c=h[0].get_color(); m=h[0].get_marker(); name,med,spr=lab.split()
  ax.text(X0-.3,y,name,ha="left",va="center",**mono)
  ax.plot([X0+2.3,X0+3.3],[y,y],color=c,lw=2.5); ax.plot(X0+2.8,y,m,color=c,ms=7)
  ax.text(X0+5.0,y,med,ha="right",va="center",**mono)
  ax.plot([X0+5.7,X0+6.9],[y,y],color=c,lw=2,ls="--")
  ax.text(X0+9.0,y,spr,ha="right",va="center",**mono)
bt=sorted([(sec[j],n,c) for n,j,c,_ in T])
bx.bar(range(3),[b[0] for b in bt],color=[b[2] for b in bt],width=.65)
for i,b in enumerate(bt): bx.text(i,b[0]+1,f"{b[0]:.0f}s",ha="center",fontsize=15)
bx.set_xticks(range(3)); bx.set_xticklabels([b[1] for b in bt],fontsize=13)
bx.set_title("total secs, 39 sets\ntrain+predict, 10 folds\nrun serially (1 core each)\nXeon 2.1GHz, 8GB RAM\ncloud VM",fontsize=13)
bx.set_yticks([]); bx.set_facecolor("#fcfcfa")
for sp in ("top","right","left"): bx.spines[sp].set_visible(False)
LOC=[("lisp",346,"#e8613c"),("lgbm",36328,"#2a6fdb")]  # see docstring
(ln,lv,lc),(gn,gv,gc)=LOC; r=(lv/gv)**.5          # areas ~ LOC
lx.add_patch(plt.Rectangle((0,0),1,1,color=gc)); lx.add_patch(plt.Rectangle((0,0),r,r,color=lc,ec="white",lw=1))
lx.text(.5,.55,f"{gn}\n({gv:,} LOC)",ha="center",va="center",color="white",fontsize=18,weight="bold")
lx.annotate(f"{ln}\n({lv:,} LOC)",(r,r),(r+.08,r+.18),color="white",fontsize=15,weight="bold",
            arrowprops=dict(arrowstyle="-",color="white"))
lx.set_xlim(0,1); lx.set_ylim(0,1); lx.set_xticks([]); lx.set_yticks([])
lx.set_title("lines of code\n(area ~ LOC; no blanks/comments)",fontsize=14); lx.set_facecolor("#fcfcfa")
for sp in lx.spines.values(): sp.set_visible(False)
fig.set_facecolor("#fcfcfa"); ax.set_facecolor("#fcfcfa"); plt.tight_layout(); plt.savefig(sys.argv[2],dpi=120)
