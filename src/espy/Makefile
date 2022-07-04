MAKEFLAGS += --silent
SHELL:= /bin/bash#
R=$(shell git rev-parse --show-toplevel)
D=$R/docs
all:
	echo $D

LISPS = $(wildcard [a-z]*.lisp)

$D/%.md : ../../docs/%.md

../../docs/%.md : %.lisp 
	mkdir -p $D
	echo "$< ==> $@"
	cat README.md > $@
	clisp tiny.lisp -f $< -g doc | gawk 'BEGIN   { FS="[ \t]*->[ \t]" } \
	        NF==3   { gsub(/^[;]*/,""); gsub(/:/," :"); \
	                  print "<b>"$$1" -> " $$2 "</b> "$$3 ; next}\
	        1' >> $@
	git add $@


htmls:  $(subst .lisp,.md,../../docs/$(subst lisp ,md ../../docs/,$(LISPS))) ## make all htmls
