#!/bin/bash
# Overnight: score the 61 SE sets with TabICL on the 5x2 folds, draw the
# figure, drop both figures into the paper, commit, push to Overleaf.
#
# Safe to re-run: tabicl3.py resumes from se61.jsonl, and every step below
# refuses to go on if the step before it did not produce what it promised.
# Memory is the thing that wedged this laptop once already, so: batch=1,
# small sets 5-wide, the eight big ones 2-wide, and a 4GB floor.
set -u
cd /Users/timm/gh/lisp/hair || exit 1

PY=~/.venvs/tabicl/bin/python
LOG=/Users/timm/gh/lisp/hair/finish.log
PAPER=/Users/timm/gh/jssprimer

say() { echo "[$(date '+%H:%M:%S')] $*" >> "$LOG"; }

say "start"

# 1. the run (resumes; nice, so the machine stays usable if anyone wakes up)
SMALL_WORKERS=5 BIG_WORKERS=2 BIG_ROWS=3000 BATCH=1 MEM_FLOOR_GB=4 \
  nice -n 10 $PY -B tabicl3.py se61.dat se61.jsonl > se61.txt 2>> "$LOG"
if ! grep -q "^#mean auc" se61.txt; then say "FAIL: se61.txt has no table"; exit 1; fi
say "run done: $(grep '^#mean auc' se61.txt)"

# 2. the figure
nice -n 10 $PY -B plot3.py se61.txt ens3-se.png >> "$LOG" 2>&1
[ -s ens3-se.pdf ] || { say "FAIL: no ens3-se.pdf"; exit 1; }
say "figure drawn"

# 3. into the paper (ens3-nose.pdf is already there)
cp ens3-se.pdf "$PAPER/" || { say "FAIL: copy"; exit 1; }

# 4. does it still build?  latexmk returns non-zero on a real error
cd "$PAPER" || exit 1
if latexmk -pdf -interaction=nonstopmode -halt-on-error main.tex >> "$LOG" 2>&1; then
  say "latex ok"
else
  say "FAIL: latex did not build; not pushing"; exit 1
fi
latexmk -c >> "$LOG" 2>&1

# 5. commit and push to Overleaf
git add -A ens3-nose.pdf ens3-se.pdf main.tex Makefile >> "$LOG" 2>&1
if git diff --cached --quiet; then
  say "nothing to commit"
else
  git commit -m "TabICL on our own 5x2 folds: two figures, 34 general + 61 SE sets

The TabICL numbers in Table 1 came from 20 random 80/20 holdouts, so
TabICL trained on 80% of each set while lisp(10) and LightGBM trained on
50%.  These two figures put all three on the same folds, same seeds, same
rows.  TabICL still wins -- 90.8 AUC against 87.5 and 85.7 over the 34
general sets -- and costs 277x LightGBM and 2176x lisp(10) to do it.

Omitted from the general figure: the four sets of 34K-49K rows, and
Internet-Advertisements at 1,559 columns.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>" >> "$LOG" 2>&1 \
    && say "committed"
  if git push >> "$LOG" 2>&1; then say "pushed to Overleaf"; else say "FAIL: push"; exit 1; fi
fi

say "ALL DONE"
