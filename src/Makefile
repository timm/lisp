# vi: set ts=4 sw=2 sts=4  :
$(HOME)/tmp/%.html : %.lisp 
	cp pygments*.css $(dir $@)
	(sh header.sh $^; gawk -f 2md.awk $^  | python3 markup.py; echo "</body></html>") > $@

$(HOME)/tmp/%.pdf : %.lisp  ## make pdf
	@mkdir -p $(dir $@)
	@echo "pdf-ing $@ ... "
	@a2ps -BjRk             \
		-q                    \
		--chars-per-line=80 \
		--line-numbers=1        \
		--highlight-level=normal \
		--borders=no              \
		--pro=color                \
		--columns 2                 \
		--right-footer=""            \
		--left-footer=""              \
		--footer="page %p."             \
		-M letter                        \
		--file-align=virtual  \
		-o $@.ps $< tabu-lib.lisp
	@ps2pdf $@.ps $@; rm $@.ps
