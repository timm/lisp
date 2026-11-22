#!/usr/bin/env python3 -B
"""plot: ens-nose.png from lgbm.py output: mean AUC over folds per set (solid), (p90-p10)/2.56 ~ sd over folds (dashed), secs bars; LOC as its own loc.png/.pdf.
LOC (2026-11-22): lisp = ai+lithp+hair.lisp (whole lib, not just forest);
lgbm = LightGBM 7810ece src/ + include/LightGBM, C++, no cuda.
Usage: python3 plot.py ens.txt ens-nose.png      (also writes ens-nose.pdf)
       python3 plot.py ens-se.txt ens-se.png    (moot/classify/se sets)"""
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
SE=all(f.startswith("se/") for f in order)
if SE:   # moot/classify/se: one panel per task
  SEG=[("se/issues/7/","issues\n(7 days)"),("se/smells/","smells"),
       ("se/defect/","defects"),("se/static/cooked/","static\nwarnings")]
  G=[(h,[f for f in order if f.startswith(p)]) for p,h in SEG]
  order=[f for _,fs in G for f in fs]
  CAPY,TAB=48,[.30,.14,.40,.40]
else:    # the 39 non-SE classify sets: by attribute type, then size
  G=[("numeric\nattributes",order[0:10]),("symbolic\nattributes",order[10:20]),
     ("mixed\nattributes",order[20:30]),("large, mixed\n(3K ... 49K rows)",order[30:39])]
  CAPY,TAB=52,[.555,.14,.40,.40]
T=[("lgbm(100)",2,"#2a6fdb","o"),("lisp(10)",1,"#e8613c","s"),("lisp(1)",0,"#1aaa7a","^")]
# sized for an IEEE 2-col figure: drawn at 2x (14.3in wide), printed at 7.16in,
# so a FS of n pt prints at n/2 pt
XT=14 if len(order)<50 else 10   # x tick font
FS=dict(tick=14,lab=14,head=17,cap=15,mono=14,bar=15,loc=15)
SHORT={"blood-transfusion-service-center":"blood-transfusion",
       "Internet-Advertisements":"internet-ads"}
fig=plt.figure(figsize=(14.3,5.2))
# runtime bars on the left; lines of code on the right (not for SE: same code)
gs=fig.add_gridspec(1,3,width_ratios=[3.4,1.6,20],wspace=0)   # runtime bars | gap | AUC
bx=fig.add_subplot(gs[0]); ax=fig.add_subplot(gs[2])
x0=0; ticks=[]; labs=[]; H=[]
for gi,(g,fs) in enumerate(G):
  fs=sorted(fs,key=lambda f:d[f][2]); xs=np.arange(x0,x0+len(fs))
  for name,j,c,m in T:
    ax.plot(xs,[d[f][j] for f in fs],color=c,marker=m,lw=2,ms=6)
    ax.plot(xs,[sp[f][j] for f in fs],color=c,ls="--",lw=1.8,marker=m,ms=3.5)
    if gi==0: H.append((name,c,m,np.mean([d[f][j] for f in order]),
                        np.mean([sp[f][j] for f in order])))
  mu=" / ".join(f"{np.mean([d[f][j] for f in fs]):.0f}" for n,j,_,_ in T)
  pass  # per-panel means dropped; legend has the overall means
  ax.text(xs.mean(),101.5,g,ha="center",va="bottom",fontsize=FS["head"])
  ticks+=list(xs); labs+=[SHORT.get(f,f).split("/")[-1] for f in fs]; x0+=len(fs)+1
  if gi<len(G)-1: ax.axvline(x0-1,color="#555",lw=1)
ax.set_xticks(ticks)
ax.set_xticklabels(labs,rotation=60,ha="right",rotation_mode="anchor",fontsize=XT)
ax.set_ylim(0,100); ax.set_xlim(-.7,x0-1.3); ax.set_yticks(range(0,101,10))
ax.set_yticks(range(5,100,10),minor=True); ax.tick_params(axis="y",which="minor",length=0)
ax.grid(axis="y",which="major",color="#ddd")
ax.grid(axis="y",which="minor",color="#b0b0b0",ls=(0,(4,4)),lw=1)
ax.set_ylabel("AUC (0..100), 5x2 folds",fontsize=FS["lab"]); ax.tick_params(axis="y",labelsize=FS["tick"])
for s_ in ("top","right"): ax.spines[s_].set_visible(False)

# legend table (inset over panels 3+4): name | solid 50th | dashed (80-20)th
tx=ax.inset_axes(TAB); tx.set_xlim(0,30); tx.set_ylim(0,5)
tx.patch.set_alpha(0); tx.set_xticks([]); tx.set_yticks([])
for s_ in tx.spines.values(): s_.set_visible(False)
mono=dict(family="monospace",fontsize=FS["mono"],bbox=dict(fc="#fcfcfa",ec="none",pad=1))
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

fig.set_facecolor("#fcfcfa"); ax.set_facecolor("#fcfcfa"); plt.tight_layout()
plt.savefig(sys.argv[2],dpi=200)
plt.savefig(sys.argv[2].rsplit(".",1)[0]+".pdf")   # vector copy for the paper

# lines of code: its own small figure (loc.png/.pdf, next to the output),
# sized to print as is (no scaling) in a wrapfigure about 1.2in wide
if not SE:
  LOC=[("lisp",346,"#e8613c"),("lgbm",36328,"#2a6fdb")]  # see docstring
  (ln,lv,lc),(gn,gv,gc)=LOC; r=(lv/gv)**.5          # areas ~ LOC
  f2,lx=plt.subplots(figsize=(1.2,1.9))
  lx.add_patch(plt.Rectangle((0,0),1,1,color=gc))
  lx.add_patch(plt.Rectangle((0,0),r,r,color=lc,ec="white",lw=.6))
  lx.text(.5,.62,f"{gn}\n{gv:,}\nLOC",ha="center",va="center",color="white",fontsize=8,weight="bold")
  lx.annotate(f"{ln}\n{lv} LOC",(r,r),(.1,.22),color="white",fontsize=7,weight="bold",
              arrowprops=dict(arrowstyle="-",color="white",lw=.6))
  lx.set_xlim(0,1); lx.set_ylim(0,1); lx.set_xticks([]); lx.set_yticks([])   # rectangles: same aspect, so areas ~ LOC
  for s_ in lx.spines.values(): s_.set_visible(False)
  f2.set_facecolor("white"); f2.tight_layout(pad=.1)
  d=sys.argv[2].rsplit("/",1)[0]+"/" if "/" in sys.argv[2] else ""
  f2.savefig(d+"loc.png",dpi=300); f2.savefig(d+"loc.pdf")

