# vi: set ts=4 sw=2 sts=4  :
SHELL:= /bin/bash#
.SILENT: # no output

LOUD = \033[1;32m#
HIGH = \033[1;33m#
SOFT = \033[0m#

help: ## show help
	grep '^[~\.a-z].*:.*##' $(MAKEFILE_LIST) \
	| sort \
	| gawk 'BEGIN {FS="##"; print "\n$(LOUD)make$(SOFT) [OPTIONS]\n"} \
	              {sub(/:.*/,"",$$1); \
                 printf("$(LOUD)%14s$(SOFT) %s\n",$$1,$$2)}'

sh: ## run a shell
	bash --init-file ../etc/dotshellrc -i

push: ## commit to main
	- echo -en "$(LOUD)Why this push? $(SOFT)" ;  read x ; git commit -am "$$x" ;  git push
	- git status

../docs/%.html: %.lisp  ## lisp to html in ../docs
	cp ../etc/pygments*.css $(dir $@)
	(sh ../etc/header.sh $^; gawk -f ../etc/2md.awk $^  \
     | python3 ../etc/markup.py; echo "</body></html>") > $@

~/tmp/%.pdf: %.lisp  ## lisp to pdf in ~/tmp
	@mkdir -p $(dir $@)
	@echo "pdf-ing $@ ... "
	@a2ps -jrBq                \
		--chars-per-line=81    \
		--line-numbers=1        \
		--highlight-level=normal \
		--borders=no              \
		--pro=color                \
		--columns 3                 \
		--right-footer=""            \
		--left-footer=""              \
		--footer="page %p."            \
		-M letter                       \
		--file-align=fill                \
		-o $@.ps $^
	@ps2pdf $@.ps $@; rm $@.ps
	open $@
