include .dot/Makefile

GIT_ROOT := $(shell git rev-parse --show-toplevel 2>/dev/null)
ETC      := $(GIT_ROOT)/etc
A2PS_DIR := $(shell a2ps --list=defaults 2>/dev/null \
              | awk '/library path/{getline; print $$1; exit}')

$(A2PS_DIR)/def.ssh: $(ETC)/def.ssh ## install a2ps style
	@cp $< $@

Chars ?= 65
~/tmp/%.pdf: %.lisp Makefile $(GIT_ROOT)/etc/def.ssh ## .lisp ==> .pdf
	@mkdir -p ~/tmp
	@echo "pdf-ing $@ ... "
	@a2ps --pretty-print=$(GIT_ROOT)/etc/def.ssh -Br --quiet --landscape   \
	      --pro=color --chars-per-line=$(Chars)        \
	      --line-numbers=1 --borders=no --columns=3    \
	      -M letter -o - $< | ps2pdf - $@
	@open $@
