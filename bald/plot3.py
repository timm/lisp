#!/usr/bin/env python3 -B
"""plot3 (bald): same figure as hair's plot3.py, but the lisp arm is
bald's n30 (30 trees, 16 bins, sqrt cols), from bald/tabicl3.py output.

Per set: mean AUC over 5x2 folds (solid), (p90-p10)/2.56 ~ sd (dashed);
total secs as log bars on the left.  Only sets where every arm has folds
appear (tabicl3.py's report already drops the rest).

Usage: python3 plot3.py ens3.txt OUT.png      (also writes OUT.pdf)"""
import os, sys, numpy as np, matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
L=open(sys.argv[1]).read().splitlines()
sec=[float(v) for v in [l for l in L if l.startswith("#secs")][0].split()[3:7]]
rows=[l.split() for l in L if l.strip() and l[0] not in "#%"]
F={}
for l in L:
  if l.startswith("%folds"): _,f,t,*v=l.split(); F[(f,t)]=[float(x) for x in v]
TK=["1:q16","n30","lgbm","tabicl"]
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
else:    # the 39 non-SE classify sets, grouped by NAME (some sets may
         # be missing: TabICL never terminated there)
  ENS=["balance.scale","blood-transfusion-service-center","breast.w",
       "diabetes","ecoli","glass","ionosphere","sonar","vehicle","wdbc",
       "vote","soybean","tic-tac-toe","german","car","solar.flare2",
       "primary.tumor","breast.cancer","spect.test","audiology",
       "credit.a","eucalyptus","ilpd","heart.c","hepatitis","colic",
       "autos","cylinder.bands","credit.g","cmc","hypothyroid","sick",
       "Internet-Advertisements","churn","COMPAS53","nomao",
       "bank-marketing","electricity","adult"]
  got=set(order)
  G=[(h,[f for f in ENS[a:b] if f in got]) for h,a,b in
     [("numeric\nattributes",0,10),("symbolic\nattributes",10,20),
      ("mixed\nattributes",20,30),("large, mixed",30,39)]]
  G=[(h,fs) for h,fs in G if fs]
  order=[f for _,fs in G for f in fs]
  CAPY,TAB=52,[.555,.14,.40,.40]
T=[(os.getenv("LAB1","lisp(1)"),0,"#2ca02c","^"),
   ("lgbm(100)",2,"#2a6fdb","o"),
   (os.getenv("LAB30","bald(30)"),1,"#e8613c","s"),
   ("TabICL",3,"#000000","D")]
# sized for an IEEE 2-col figure: drawn at 2x (14.3in wide), printed at 7.16in,
# so a FS of n pt prints at n/2 pt
XT=14 if len(order)<50 else 10   # x tick font
FS=dict(tick=14,lab=14,head=17,cap=15,mono=14,bar=15,loc=15)
SHORT={"blood-transfusion-service-center":"blood-transfusion",
       "Internet-Advertisements":"internet-ads"}
fig=plt.figure(figsize=(14.3,5.2))
gs=fig.add_gridspec(1,3,width_ratios=[3.4,1.6,20],wspace=0)   # runtime bars | gap | AUC
bx=fig.add_subplot(gs[0]); ax=fig.add_subplot(gs[2])
x0=0; ticks=[]; labs=[]; H=[]
for gi,(g,fs) in enumerate(G):
  fs=sorted(fs,key=lambda f:d[f][1]); xs=np.arange(x0,x0+len(fs))
  for name,j,c,m in T:
    ax.plot(xs,[d[f][j] for f in fs],color=c,marker=m,lw=2,ms=6)
    ax.plot(xs,[sp[f][j] for f in fs],color=c,ls="--",lw=1.8,marker=m,ms=3.5)
    if gi==0: H.append((name,c,m,np.mean([d[f][j] for f in order]),
                        np.mean([sp[f][j] for f in order])))
  ax.text(xs.mean(),101.5,g,ha="center",va="bottom",fontsize=FS["head"])
  ticks+=list(xs); labs+=[SHORT.get(f,f).split("/")[-1] for f in fs]; x0+=len(fs)+1
  if gi<len(G)-1: ax.axvline(x0-1,color="#555",lw=1)
ax.set_xticks(ticks)
ax.set_xticklabels(labs,rotation=60,ha="right",rotation_mode="anchor",fontsize=XT)
ax.set_ylim(0,100); ax.set_xlim(-.7,x0-1.3); ax.set_yticks(range(0,101,10))
ax.set_yticks(range(5,100,10),minor=True); ax.tick_params(axis="y",which="minor",length=0)
ax.grid(axis="y",which="major",color="#ddd")
ax.grid(axis="y",which="minor",color="#b0b0b0",ls=(0,(4,4)),lw=1)
# nudged right: the runtime panel's "28,066s" was touching this label
ax.set_ylabel("AUC (0..100), 5x2 folds",fontsize=FS["lab"],labelpad=-2); ax.tick_params(axis="y",labelsize=FS["tick"])
for s_ in ("top","right"): ax.spines[s_].set_visible(False)

# legend table (inset over panels 3+4): name | solid mean | dashed sd
tx=ax.inset_axes(TAB); tx.set_xlim(0,30); tx.set_ylim(0,5)
tx.patch.set_alpha(0); tx.set_xticks([]); tx.set_yticks([])
for s_ in tx.spines.values(): s_.set_visible(False)
mono=dict(family="monospace",fontsize=FS["mono"],bbox=dict(fc="#fcfcfa",ec="none",pad=1))
tx.text(17.5,4.5,"mean",ha="right",va="center",**mono)
tx.text(29.5,4.5,"sd",ha="right",va="center",**mono)
for k,(name,c,m,med,spr) in enumerate(H):
  y=3.1-1.0*k
  tx.text(0,y,name,ha="left",va="center",**mono)
  tx.plot([9.6,12.2],[y,y],color=c,lw=2.2); tx.plot(10.9,y,m,color=c,ms=6)
  tx.text(17.5,y,f"{med:.1f}",ha="right",va="center",**mono)
  tx.plot([20.3,23.3],[y,y],color=c,lw=2,ls="--")
  tx.text(29.5,y,f"{spr:.1f}",ha="right",va="center",**mono)

# TabICL is ~300x the others, so a linear bar hides bald and lgbm entirely
bt=sorted([(sec[j],n,c) for n,j,c,_ in T])
bx.bar(range(len(bt)),[b[0] for b in bt],color=[b[2] for b in bt],width=.84)
bx.set_yscale("log"); bx.set_ylim(min(b[0] for b in bt)/3,max(b[0] for b in bt)*6)
for i,b in enumerate(bt):
  bx.text(i,b[0]*1.25,f"{b[0]:,.0f}s" if b[0]>=10 else f"{b[0]:.1f}s",
          ha="center",va="bottom",weight="bold",fontsize=FS["bar"]-1)
bx.set_xticks(range(len(bt)))
# four arm names side by side in a narrow panel collide; stand them up
bx.set_xticklabels([b[1] for b in bt],rotation=90,ha="center",
                   va="top",fontsize=FS["bar"]-1)
bx.set_title("total secs, log scale\n(summed over folds,\n1 thread each)",fontsize=FS["head"]-5)
bx.set_yticks([]); bx.set_facecolor("#fcfcfa"); bx.set_xlim(-.55,len(bt)-.45)
for s_ in ("top","right","left"): bx.spines[s_].set_visible(False)

fig.set_facecolor("#fcfcfa"); ax.set_facecolor("#fcfcfa"); plt.tight_layout()
plt.savefig(sys.argv[2],dpi=200)
plt.savefig(sys.argv[2].rsplit(".",1)[0]+".pdf")   # vector copy for the paper
