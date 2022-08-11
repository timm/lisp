R=$(shell git rev-parse --show-toplevel)

all: *.lisp
	@rm -f  _all.lisp
	#@$(foreach f,$^,(figlet -W -f cybermedium $f>>_all.lisp;cat $f>>_all.lisp; echo "">>_all.lisp);)
	@$(foreach f,$^,(figlet -W -f ogre $f>>_all.lisp;cat $f>>_all.lisp;);)
	$(MAKE) ../../docs/_all.pdf
	@rm _all.lisp

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
