# vi: set ts=4 sw=2 sts=4  :
$(HOME)/tmp/%.html : %.lisp 
	cp pygments*.css $(dir $@)
	(sh header.sh $^; gawk -f 2md.awk $^  | python3 markup.py; echo "</body></html>") > $@

~/tmp/%.pdf : %.lisp  ## make pdf
	@mkdir -p $(dir $@)
	@echo "pdf-ing $@ ... "
	@a2ps -jr             \
		-q                    \
		--chars-per-line=80 \
		--line-numbers=1        \
		--highlight-level=normal \
		--borders=no              \
		--pro=color                \
		--columns 3                 \
		--right-footer=""            \
		--left-footer=""              \
		--footer="page %p."             \
		-M letter                        \
		--file-align=fill  \
		-o $@.ps $^
	@ps2pdf $@.ps $@.pdf; rm $@.ps
