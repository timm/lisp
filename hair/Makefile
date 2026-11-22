# hair (lisp). "make" = help.
SHELL := bash
.SILENT:
cols  ?= 3
Lisp   = $(wildcard *.lisp)
D     ?= ../docs
Html   = $(Lisp:%.lisp=$(D)/%.lisp.html)

help: ## show this help
	grep -E '^[a-z.%/]+:.*##' Makefile | sed 's/:.*## /\t/'

msg ?= saving

push: ## git add this dir, commit (msg="..."), push
	git add . && { git commit -m "$(msg)" || true; } && git push

run: ## run every eg (sbcl --script hair.lisp all)
	sbcl --script hair.lisp all

html: $(D)/index.html ## render *.lisp + ../docs/index.html; open it
	open $(D)/index.html

# index: title, one-liner, a link per source with its lines of code
# (no blanks, no ;comments)
$(D)/index.html: $(Html) Makefile
	{ echo '<meta charset=utf-8><title>hair (lisp)</title>'; \
	  echo '<meta name=viewport content="width=device-width,initial-scale=1">'; \
	  echo '<body style="margin:0;padding:2em 0"><center>'; \
	  echo '<h1 style="font:normal 2em Times New Roman,serif">'; \
	  echo 'hair (lisp) : hands-on AI recipes</h1>'; \
	  echo '<div style="max-width:600px;padding:0 16px;text-align:left;font:11pt serif">'; \
	  echo '<p>A Common Lisp port of <a href="https://timm.github.io/hair/">'; \
	  echo 'hair</a>: data, distances, stats and trees in a few hundred'; \
	  echo 'lines. Run it with <code>sbcl --script hair.lisp all</code>.'; \
	  echo '<p><b>Sources</b><ul>'; \
	  for f in $(Lisp); do \
	    n=$$(grep -cvE '^[[:space:]]*(;.*)?$$' $$f); \
	    echo "<li><a href=\"$$f.html\">$$f</a> ($$n lines)</li>"; done; \
	  echo '</ul></div></center>'; } > $@

Chrome = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"

pdf: $(D)/ai.lisp.pdf ## ai.lisp as landscape pdf; open it
	open $<

$(D)/%.pdf: $(D)/%.html
	$(Chrome) --headless --disable-gpu --no-pdf-header-footer \
	  --print-to-pdf=$@ $< 2>/dev/null

$(D)/%.lisp.html: %.lisp $(D)/hi.py
	mkdir -p $(D)
	python3 $(D)/hi.py $(cols) $< > $@

ens: ## rerun forest + lightgbm on 39 sets (5x2), redraw ens.png
	sbcl --script hair.lisp ens > ens.dat
	python3 -B lgbm.py ens.dat > ens.txt
	python3 -B plot.py ens.txt ens.png
