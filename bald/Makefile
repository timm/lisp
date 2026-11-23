# bald (lisp): hair, less hair. "make" = help.
SHELL := bash
.SILENT:

help: ## show this help
	grep -E '^[a-z.%/]+:.*##' Makefile | sed 's/:.*## /\t/'

msg ?= saving

push: ## git add this dir, commit (msg="..."), push
	git add . && { git commit -m "$(msg)" || true; } && git push

run: ## run every eg (sbcl --script bald.lisp all)
	sbcl --script bald.lisp all

charts: ## redraw bald.png, bald30.png from runs/ (no data files needed)
	python3 -B plot.py bald.png "lightgbm(100 trees)=lgbm.txt:lgbm" \
	  "hair(10 trees, 8 bins)=lgbm.txt:q8" "bald(10 trees, 8 bins)=n10q8.dat:1"
	python3 -B plot.py bald30.png --tabicl "lightgbm(100 trees)=lgbm.txt:lgbm" \
	  "bald(10 trees, 8 bins)=n10q8.dat:1" "bald(30 trees, 16 bins)=grid-q16.dat:0"
