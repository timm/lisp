#!/bin/sh
# again.sh: edit a lua learner, see the figure.
#
# Only the lua arm is recomputed.  lgbm.csv and tabicl.csv cost hours
# and are never touched, so comparing against them is a lookup.  This
# bypasses make on purpose: make's cache rule would void them too.
#
# usage: ,/again.sh          # nb, redraw clas.png
#        ,/again.sh rmeans   # rmeans, redraw labels.png and top5.png
#        DOCS=/tmp/x ,/again.sh      # draw somewhere else first
cd "$(dirname "$0")/.." || exit 1
who=${1:-nb}
case $who in
  nb)     kind=classify;   png="clas" ;;
  rmeans) kind=regression; png="labels top5" ;;
  *)      echo "$0: nb or rmeans, not $who" >&2; exit 1 ;;
esac
rm -f "$HOME"/tmp/cache/$kind/*/$who.csv
ls "$HOME"/gits/moot/$kind/*.csv \
 | xargs -P "${P:-8}" -S 8192 -I{} luajit jiffy-eg.lua go=$who file={} \
 > /dev/null || exit 1
exec "$HOME"/tmp/nnenv/bin/python ,/charts.py $png
