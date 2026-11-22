#!/usr/bin/env python3 -B
"""plot: ens.png from lgbm.py table: AUC per set (p20/p80 dashed per group) + total secs bars.
Usage: python3 plot.py ens.txt ens.png"""
import sys, numpy as np, matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
L=open(sys.argv[1]).read().splitlines()
sec=[float(v) for v in [l for l in L if l.startswith("#secs")][0].split()[3:6]]
rows=[l.split() for l in open(sys.argv[1]) if l.strip() and not l.startswith("#")]
d={r[0]:[float(v.rstrip("!")) for v in r[1:4]] for r in rows}
order=list(d)
G=[("numeric only",order[0:10]),("symbolic only",order[10:20]),
   ("mixed",order[20:30]),("mixed, >3000 rows",order[30:39])]
T=[("lgbm",2,"#2a6fdb","o"),("q8 (10 trees)",1,"#e8613c","s"),("1:q8 (1 tree)",0,"#1aaa7a","^")]
fig=plt.figure(figsize=(23,7.5)); gs=fig.add_gridspec(1,3,width_ratios=[20,.6,2.6],wspace=0)
ax=fig.add_subplot(gs[0]); bx=fig.add_subplot(gs[2]); x0=0; ticks=[];labs=[]
for gi,(g,fs) in enumerate(G):
  fs=sorted(fs,key=lambda f:d[f][2]); xs=np.arange(x0,x0+len(fs))
  for name,j,c,m in T:
    ys=[d[f][j] for f in fs]
    ax.plot(xs,ys,color=c,marker=m,lw=2.2,ms=6,label=name if gi==0 else None)
    lo,hi=np.percentile(ys,[20,80])
    for v in (lo,hi): ax.hlines(v,xs[0]-.3,xs[-1]+.3,colors=c,linestyles="--",lw=1.1,alpha=.8)
  mu=" ".join(f"{n.split()[0]} {np.mean([d[f][j] for f in fs]):.1f}" for n,j,_,_ in T)
  pr=" ".join(f"{n.split()[0]} {np.percentile([d[f][j] for f in fs],20):.0f}-{np.percentile([d[f][j] for f in fs],80):.0f}" for n,j,_,_ in T)
  ax.text(xs.mean(),14,f"{g} (n={len(fs)})\nmean: {mu}\np20-p80: {pr}",ha="center",va="center",fontsize=11)
  ticks+=list(xs); labs+=fs; x0+=len(fs)+1
  if gi<3: ax.axvline(x0-1,color="#555",lw=1)
ax.set_xticks(ticks); ax.set_xticklabels(labs,rotation=50,ha="right",fontsize=11)
ax.set_ylim(0,100); ax.set_xlim(-.7,x0-1.3); ax.set_yticks(range(0,101,10))
ax.grid(axis="y",color="#ddd"); ax.set_ylabel("AUC (0..100), mean of 5x2 folds",color="#555")
for s in ("top","right"): ax.spines[s].set_visible(False)
ax.legend(title="treatment (dashed: p20, p80 per group)",loc="center right",bbox_to_anchor=(1,.55),frameon=False,fontsize=11)
ax.set_title("LightGBM vs bagged forest (10 trees, q8 bins, sqrt(#x) cols, cap 200) vs 1 tree; sorted by LightGBM within each group",loc="left",fontsize=13)
bt=sorted([(sec[j],n,c) for n,j,c,_ in T])
bx.bar(range(3),[b[0] for b in bt],color=[b[2] for b in bt],width=.65)
for i,b in enumerate(bt): bx.text(i,b[0]+1,f"{b[0]:.0f}s",ha="center",fontsize=11)
bx.set_xticks(range(3)); bx.set_xticklabels([b[1].split()[0] for b in bt],fontsize=11)
bx.set_title("total secs, all 39 sets\n(train+predict, 10 folds, 1 core)",fontsize=11)
bx.set_yticks([]); bx.set_facecolor("#fcfcfa")
for sp in ("top","right","left"): bx.spines[sp].set_visible(False)
fig.set_facecolor("#fcfcfa"); ax.set_facecolor("#fcfcfa"); plt.tight_layout(); plt.savefig(sys.argv[2],dpi=120)
