R=$(shell git rev-parse --show-toplevel)

want = col/cols col/num col/sample col/sym    \
			 row/row row/rows \
     lib/macros lib/maths lib/strings lib/lists \
     lib/settings lib/structs lib/egs eg

all: 
	@figlet -W -f larry3d tiny.lisp>_all.lisp; cat tiny.lisp>>_all.lisp
	@$(foreach f,$(want),(figlet -W -f contessa $f>>_all.lisp;cat $f.lisp>>_all.lisp;);)
	@$(MAKE) ../../docs/_all.pdf
	@rm _all.lisp
	@open   ../../docs/_all.pdf
		
#--highlight-level=normal \

../../docs/%.pdf : %.lisp  ## make pdf
	@mkdir -p $R/docs
	@echo "pdf-ing $@ ... "
	@a2ps -B             \
		-q                 \
	--chars-per-line=82 \
	--file-align=fill \
		--line-numbers=1        \
		--borders=no              \
		--pro=color                \
		--columns 3                 \
		--right-footer="%Q"     \
		--left-footer="%W"              \
		--footer=""             \
		-M letter                        \
		-o $@.ps $<
	@ps2pdf $@.ps $@; rm $@.ps; git add $@
