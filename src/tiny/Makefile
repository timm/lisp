R=$(shell git rev-parse --show-toplevel)

want = sample sym num about row data  \
     lib/macros lib/maths lib/strings \
     lib/settings lib/structs lib/demos eg

all: 
	@figlet -W -f larry3d tiny.lisp>_all.lisp; cat tiny.lisp>>_all.lisp
	@$(foreach f,$(want),(figlet -W -f contessa $f>>_all.lisp;cat $f.lisp>>_all.lisp;);)
	@$(MAKE) ../../docs/_all.pdf
	@rm _all.lisp
	@open   ../../docs/_all.pdf

../../docs/%.pdf : %.lisp  ## make pdf
	@mkdir -p $R/docs
	@echo "pdf-ing $@ ... "
	@a2ps -Bjr             \
		-q                  \
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
