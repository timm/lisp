R=$(shell git rev-parse --show-toplevel)

all: tiny.lisp sample.lisp sym.lisp num.lisp about.lisp row.lisp data.lisp  \
     lib/macros.lisp lib/maths.lisp lib/strings.lisp \
     lib/settings.lisp lib/structs.lisp lib/demos.lisp eg.lisp
	@rm -f  _all.lisp
	@$(foreach f,$^,(figlet -W -f contessa `dirname $f`/`basename $f .lisp`>>_all.lisp;cat $f>>_all.lisp;);)
	@$(MAKE) ../../docs/_all.pdf
	@rm _all.lisp
	@open   ../../docs/_all.pdf

../../docs/%.pdf : %.lisp  ## make pdf
	@mkdir -p $R/docs
	@echo "pdf-ing $@ ... "
	@a2ps -Bjr             \
		-q                    \
		-L 125                 \
		--line-numbers=1        \
		--highlight-level=normal \
		--borders=no              \
		--pro=color                \
		--columns 3                 \
		--right-footer=""            \
		--left-footer=""              \
		--footer="page %p."             \
		-M letter                        \
		-o $@.ps $<
	@ps2pdf $@.ps $@; rm $@.ps; git add $@
