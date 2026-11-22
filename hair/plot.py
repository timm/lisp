#!/usr/bin/env python3 -B
"""plot: ens-nose.png from lgbm.py output: mean AUC over folds per set (solid), (p90-p10)/2.56 ~ sd over folds (dashed), LOC + secs bars.
LOC (2026-11-22): lisp = ai+lithp+hair.lisp (whole lib, not just forest);
lgbm = LightGBM 7810ece src/ + include/LightGBM, C++, no cuda.
Usage: python3 plot.py ens.txt ens-nose.png   (also writes ens-nose.pdf)"""
import sys, numpy as np, matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
L=open(sys.argv[1]).read().splitlines()
sec=[float(v) for v in [l for l in L if l.startswith("#secs")][0].split()[3:6]]
rows=[l.split() for l in L if l.strip() and l[0] not in "#%"]
F={}
for l in L:
  if l.startswith("%folds"): _,f,t,*v=l.split(); F[(f,t)]=[float(x) for x in v]
TK=["1:q8","q8","lgbm"]
d={r[0]:[float(np.mean(F[(r[0],t)])) for t in TK] for r in rows}
sp={r[0]:[float(np.subtract(*np.percentile(F[(r[0],t)],[90,10]))/2.56) for t in TK] for r in rows}  # ~sd
order=list(d)
G=[("numeric\nattributes",order[0:10]),("symbolic\nattributes",order[10:20]),
   ("mixed\nattributes",order[20:30]),("large, mixed\n(3K ... 49K rows)",order[30:39])]
T=[("lgbm(100)",2,"#2a6fdb","o"),("lisp(10)",1,"#e8613c","s"),("lisp(1)",0,"#1aaa7a","^")]
# sized for an IEEE 2-col figure: drawn at 2x (14.3in wide), printed at 7.16in,
# so a FS of n pt prints at n/2 pt
FS=dict(tick=14,lab=14,head=17,cap=15,mono=14,bar=15,loc=15)
SHORT={"blood-transfusion-service-center":"blood-transfusion",
       "Internet-Advertisements":"internet-ads"}
fig=plt.figure(figsize=(14.3,7.8))
gs=fig.add_gridspec(1,5,width_ratios=[2.9,1.6,20,.45,3.4],wspace=0)
lx=fig.add_subplot(gs[0]); ax=fig.add_subplot(gs[2]); bx=fig.add_subplot(gs[4])
x0=0; ticks=[]; labs=[]; H=[]
for gi,(g,fs) in enumerate(G):
  fs=sorted(fs,key=lambda f:d[f][2]); xs=np.arange(x0,x0+len(fs))
  for name,j,c,m in T:
    ax.plot(xs,[d[f][j] for f in fs],color=c,marker=m,lw=2,ms=6)
    ax.plot(xs,[sp[f][j] for f in fs],color=c,ls="--",lw=1.8,marker=m,ms=3.5)
    if gi==0: H.append((name,c,m,np.mean([d[f][j] for f in order]),
                        np.mean([sp[f][j] for f in order])))
  mu=" / ".join(f"{np.mean([d[f][j] for f in fs]):.0f}" for n,j,_,_ in T)
  ax.text(xs.mean(),52,f"mean: {mu}",ha="center",va="center",fontsize=FS["cap"])
  ax.text(xs.mean(),101.5,g,ha="center",va="bottom",fontsize=FS["head"])
  ticks+=list(xs); labs+=[SHORT.get(f,f) for f in fs]; x0+=len(fs)+1
  if gi<3: ax.axvline(x0-1,color="#555",lw=1)
ax.set_xticks(ticks)
ax.set_xticklabels(labs,rotation=60,ha="right",rotation_mode="anchor",fontsize=FS["tick"])
ax.set_ylim(0,100); ax.set_xlim(-.7,x0-1.3); ax.set_yticks(range(0,101,10))
ax.set_yticks(range(5,100,10),minor=True); ax.tick_params(axis="y",which="minor",length=0)
ax.grid(axis="y",which="major",color="#ddd")
ax.grid(axis="y",which="minor",color="#b0b0b0",ls=(0,(4,4)),lw=1)
ax.set_ylabel("AUC (0..100), 5x2 folds",fontsize=FS["lab"]); ax.tick_params(axis="y",labelsize=FS["tick"])
for s_ in ("top","right"): ax.spines[s_].set_visible(False)

# legend table (inset over panels 3+4): name | solid 50th | dashed (80-20)th
tx=ax.inset_axes([.555,.115,.40,.27]); tx.set_xlim(0,30); tx.set_ylim(0,5)
tx.set_facecolor("#fcfcfa"); tx.set_xticks([]); tx.set_yticks([])
for s_ in tx.spines.values(): s_.set_visible(False)
mono=dict(family="monospace",fontsize=FS["mono"])
tx.text(17.5,4.5,"mean",ha="right",va="center",**mono)
tx.text(29.5,4.5,"sd",ha="right",va="center",**mono)
for k,(name,c,m,med,spr) in enumerate(H):
  y=2.9-1.1*k
  tx.text(0,y,name,ha="left",va="center",**mono)
  tx.plot([9.6,12.2],[y,y],color=c,lw=2.2); tx.plot(10.9,y,m,color=c,ms=6)
  tx.text(17.5,y,f"{med:.1f}",ha="right",va="center",**mono)
  tx.plot([20.3,23.3],[y,y],color=c,lw=2,ls="--")
  tx.text(29.5,y,f"{spr:.1f}",ha="right",va="center",**mono)

bt=sorted([(sec[j],n,c) for n,j,c,_ in T])
bx.bar(range(3),[b[0] for b in bt],color=[b[2] for b in bt],width=.84)
bx.set_ylim(0,max(b[0] for b in bt))
for i,b in enumerate(bt): bx.text(i,b[0]-1,f"{b[0]:.0f}s",ha="center",va="top",color="white",weight="bold",fontsize=FS["bar"])
bx.set_xticks(range(3)); bx.set_xticklabels([b[1].replace("(","\n(") for b in bt],fontsize=FS["bar"])
bx.set_title("total secs\n(serial, 1 core;\nXeon 2.1GHz, 8GB)",fontsize=FS["head"]-4)
bx.set_yticks([]); bx.set_facecolor("#fcfcfa"); bx.set_xlim(-.55,2.55)
for s_ in ("top","right","left"): bx.spines[s_].set_visible(False)

LOC=[("lisp",346,"#e8613c"),("lgbm",36328,"#2a6fdb")]  # see docstring
(ln,lv,lc),(gn,gv,gc)=LOC; r=(lv/gv)**.5          # areas ~ LOC
lx.add_patch(plt.Rectangle((0,0),1,1,color=gc)); lx.add_patch(plt.Rectangle((0,0),r,r,color=lc,ec="white",lw=1))
lx.text(.5,.55,f"{gn}\n{gv:,}\nLOC",ha="center",va="center",color="white",fontsize=FS["loc"]-1,weight="bold")
lx.annotate(f"{ln}\n{lv}\nLOC",(r,r),(.12,.2),color="white",fontsize=FS["loc"]-2,weight="bold",
            arrowprops=dict(arrowstyle="-",color="white"))
lx.set_xlim(0,1); lx.set_ylim(0,1); lx.set_xticks([]); lx.set_yticks([])
lx.set_title("lines of code\n(area ~ LOC)",fontsize=FS["head"]-2); lx.set_facecolor("#fcfcfa")
for s_ in lx.spines.values(): s_.set_visible(False)
fig.set_facecolor("#fcfcfa"); ax.set_facecolor("#fcfcfa"); plt.tight_layout()
plt.savefig(sys.argv[2],dpi=200)
plt.savefig(sys.argv[2].rsplit(".",1)[0]+".pdf")   # vector copy for the paper
