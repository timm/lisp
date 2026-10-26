SHELL    := bash
GIT_ROOT := $(shell git rev-parse --show-toplevel 2>/dev/null)
ETC      := $(GIT_ROOT)/,
A2PS_DIR := $(shell a2ps --list=defaults 2>/dev/null \
              | awk '/library path/{getline; print $$1; exit}')

# a2ps splits option values on commas, and our etc dir IS
# a comma, so it can never be named in a path.  Install the
# sheet by name instead, then ask for that name (def, lua).
$(A2PS_DIR)/%.ssh: $(ETC)/%.ssh ## install an a2ps style
	@mkdir -p $(dir $@)
	@cp $< $@

Chars ?= 65
Font  ?= 4.5
A2PS   = a2ps -Br --quiet --landscape                 \
             --pro=color --chars-per-line=$(Chars)   \
             --font-size=$(Font)                     \
             --line-numbers=1 --borders=no           \
             --columns=3 -M letter -o -

~/tmp/%.pdf: %.lisp Makefile $(A2PS_DIR)/def.ssh ## .lisp ==> .pdf
	@mkdir -p ~/tmp
	@echo "pdf-ing $@ ... "
	@$(A2PS) --pretty-print=def $< | ps2pdf - $@
	@open $@

~/tmp/%.pdf: %.lua Makefile $(A2PS_DIR)/lua.ssh ## .lua ==> .pdf
	@mkdir -p ~/tmp
	@echo "pdf-ing $@ ... "
	@$(A2PS) --pretty-print=lua $< | ps2pdf - $@
	@open $@

.DEFAULT_GOAL := usage
MOOT ?= $(HOME)/gits/moot
Reps ?= 20

# ---- the cache -------------------------------------------------
# A split is a seed, not a file, so nothing here copies rows: the
# cache holds only scores.  The two freshness rules are split by
# what each tool is good at.  Timestamps are make's job, so "an
# input moved, the old scores are void" lives here, as a rule that
# DELETES them.  "This learner has no scores yet" is an existence
# test, so it lives in jiffy-eg.lua and in lgbm.py, each of which
# skips any file already written.  Make invalidates; lua rebuilds.
CACHE ?= $(HOME)/tmp/cache
P     ?= 8
# -S 8192: BSD xargs assembles at most 255 bytes per -I line, and
# silently drops the rest of the work when a path makes it longer.
CLAS   = $(wildcard $(MOOT)/classify/*.csv)
OPT    = $(wildcard $(MOOT)/optimize/*/*.csv)
REG    = $(wildcard $(MOOT)/regression/*.csv)
# One stamp per language, so editing lua never voids the hour of
# LightGBM runs, and editing python never voids the lua ones.
SETS   = $(CLAS) $(OPT) $(REG)
VOID   = $(patsubst $(MOOT)/%.csv,$(CACHE)/%/.lua,$(SETS)) \
         $(patsubst $(MOOT)/%.csv,$(CACHE)/%/.py,$(SETS))

$(CACHE)/%/.lua: $(MOOT)/%.csv jiffy.lua jiffy-eg.lua
	@mkdir -p $(dir $@)
	@rm -f $(dir $@)nb.csv $(dir $@)rmeans.csv $(dir $@)rmeansall.csv
	@touch $@

$(CACHE)/%/.py: $(MOOT)/%.csv min.py $(ETC)/lgbm.py
	@mkdir -p $(dir $@)
	@rm -f $(dir $@)lgbm.csv
	@touch $@

cache: $(VOID) ## drop any scores whose csv or code has moved

nb: cache ## naive Bayes, every classify set, 20 seeds
	@echo "$@ ..."
	@ls $(MOOT)/classify/*.csv \
	 | xargs -P $(P) -S 8192 -I{} luajit jiffy-eg.lua go=$@ file={} > /dev/null

rmeans: cache ## rmeans, every optimize and regression set
	@echo "$@ ..."
	@ls $(MOOT)/optimize/*/*.csv $(MOOT)/regression/*.csv \
	 | xargs -P $(P) -S 8192 -I{} luajit jiffy-eg.lua go=$@ file={} > /dev/null

lgbm: cache ## LightGBM, those same seeds, those same scores
	@echo "$@ ..."
	@ls $(MOOT)/classify/*.csv $(MOOT)/optimize/*/*.csv \
	    $(MOOT)/regression/*.csv \
	 | xargs -P $(P) -S 8192 -I{} python3 $(ETC)/lgbm.py {} > /dev/null

rmeansall: cache ## rmeans again, reading every training row
	@echo "$@ ..."
	@ls $(MOOT)/optimize/*/*.csv $(MOOT)/regression/*.csv \
	 | xargs -P $(P) -S 8192 -I{} luajit jiffy-eg.lua go=$@ file={} > /dev/null

results: nb rmeans rmeansall lgbm ## every learner, every set, once

fresh: ## forget every score
	@rm -rf $(CACHE)

# x.md is both input and output here, so mtime can never decide.
weave: $(ETC)/weave.awk ## weave x.lisp code into x.md, in place
	@for f in *.lisp; do m=$${f%.lisp}.md; touch $$m;      \
	   gawk -f $(ETC)/weave.awk $$f $$m > $$m.tmp \
	     && mv $$m.tmp $$m || { rm -f $$m.tmp; exit 1; };     \
	 done

todo: ## list forms in a .lisp that its .md never mentions
	@for f in *.lisp; do \
	   gawk -v strict=1 -f $(ETC)/weave.awk $$f $${f%.lisp}.md >/dev/null; \
	 done

ENGINE ?= tectonic
DOCDEPS = $(ETC)/lisp2md.awk $(ETC)/acm.tex $(ETC)/code.lua Makefile
PANDOC  = pandoc -f markdown --syntax-highlighting=idiomatic \
            --lua-filter=$(ETC)/code.lua                      \
            --shift-heading-level-by=-1                       \
            --template=$(ETC)/acm.tex                         \
            -V title="$(notdir $<)"

~/tmp/%.md: %.lisp $(ETC)/lisp2md.awk ## .lisp ==> .md
	@mkdir -p ~/tmp
	@awk -f $(ETC)/lisp2md.awk $< > $@

~/tmp/%.tex: %.lisp $(DOCDEPS) ## .lisp ==> .tex (to debug the latex)
	@mkdir -p ~/tmp
	@echo "tex-ing $@ ... "
	@awk -f $(ETC)/lisp2md.awk $< | $(PANDOC) -o $@

~/tmp/%.html: %.lisp $(DOCDEPS) $(ETC)/head.html ## .lisp ==> .html
	@mkdir -p ~/tmp
	@echo "html-ing $@ ... "
	@awk -f $(ETC)/lisp2md.awk $< \
	  | pandoc -s -f markdown                                 \
	           --lua-filter=$(ETC)/code.lua                   \
	           --shift-heading-level-by=-1                    \
	           -H $(ETC)/head.html                            \
	           -V title="$(notdir $<)" -o $@

~/tmp/%.doc.pdf: %.lisp $(DOCDEPS) ## .lisp ==> 2-column acm pdf
	@mkdir -p ~/tmp
	@echo "doc-ing $@ ... "
	@awk -f $(ETC)/lisp2md.awk $< | $(PANDOC) --pdf-engine=$(ENGINE) -o $@
	@open $@
