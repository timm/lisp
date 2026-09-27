SHELL    := bash
GIT_ROOT := $(shell git rev-parse --show-toplevel 2>/dev/null)
ETC      := $(GIT_ROOT)/,
A2PS_DIR := $(shell a2ps --list=defaults 2>/dev/null \
              | awk '/library path/{getline; print $$1; exit}')

# a2ps splits option values on commas, and our etc dir IS
# a comma, so it can never be named in a path.  Install the
# sheet by name instead, then ask for it as "def".
$(A2PS_DIR)/def.ssh: $(ETC)/def.ssh ## install a2ps style
	@mkdir -p $(dir $@)
	@cp $< $@

Chars ?= 65
Font  ?= 4.5
~/tmp/%.pdf: %.lisp Makefile $(A2PS_DIR)/def.ssh ## .lisp ==> .pdf
	@mkdir -p ~/tmp
	@echo "pdf-ing $@ ... "
	@a2ps --pretty-print=def -Br --quiet --landscape          \
	      --pro=color --chars-per-line=$(Chars)        \
	      --font-size=$(Font)                          \
	      --line-numbers=1 --borders=no --columns=3    \
	      -M letter -o - $< | ps2pdf - $@
	@open $@

.DEFAULT_GOAL := usage
MOOT ?= $(HOME)/gits/moot
Reps ?= 20

.PHONY: usage weave todo test

usage: ## show these targets
	@gawk 'BEGIN {FS=":.*## "; print "\ntargets:"} \
	       /^[a-zA-Z0-9_.%\/~-]+:.*## /  \
	         {printf "  \033[36m%-14s\033[0m %s\n", $$1, $$2}' \
	  $(MAKEFILE_LIST)

test: ## mean win of 20 holdouts, per moot data set
	@mkdir -p ~/tmp
	@ls $(MOOT)/optimize/*/*.csv \
	 | xargs -P 8 -I{} sbcl --script ezr.lisp -R $(Reps) -f {} --holdout \
	   2> ~/tmp/$@.err \
	 | sort -n | tee ~/tmp/$@.out | fmt -60
	@echo "--- crashes: $$(grep -c . ~/tmp/$@.err) (see ~/tmp/$@.err)"

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
