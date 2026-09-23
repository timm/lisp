# gh/dot/Makefile -- rules shared by every ~/gh/* repo.
#
# gh/dot/* is the one true copy of the shared files (this Makefile,
# bashrc, init.lua, ...). `make dot` in a sibling repo mirrors them
# into that repo's .dot/, so a lone clone still has them. Each
# sibling's Makefile starts with:   include .dot/Makefile
# then adds its own rules below.
#
# Convention: `rule`  acts on this repo;
#             `rules` (rule + "s") runs `rule` in every sibling repo.

SHELL := bash
THIS  := $(abspath $(lastword $(MAKEFILE_LIST)))
TOP   := $(shell git rev-parse --show-toplevel 2>/dev/null)
ifeq ($(TOP),)
  $(error not inside a git repo (run: git init))
endif
GH    := $(abspath $(TOP)/..)
DOT   := $(GH)/dot
REPOS := $(dir $(wildcard $(GH)/*/.git))
ME    := $(patsubst %/,%,$(dir $(THIS)))
RULE  ?= usage
MSG   ?= saving
BANNER?= etc/banner.txt
M     := $(MAKE) --no-print-directory
MK    := $(M) -f $(THIS)

.DEFAULT_GOAL := usage
.PHONY: usage dot pull push sh status walks dots pulls pushs statuss

usage: ## show these targets (this repo first, then shared .dot ones)
	@awk -f $(ME)/help.awk $(MAKEFILE_LIST)

## ------------------------------------------------ this repo
dot: ## .dot := mirror of gh/dot; Makefile line 1 includes it
	@[ $(TOP) = $(DOT) ] || { \
	  rsync -a --delete --exclude .git $(DOT)/ $(TOP)/.dot/; \
	  grep -qs '^include .dot/Makefile' $(TOP)/Makefile || { \
	    { echo 'include .dot/Makefile'; cat $(TOP)/Makefile 2>/dev/null; :; } \
	      > $(TOP)/Makefile.new && mv $(TOP)/Makefile.new $(TOP)/Makefile; }; }

pull: ## git pull, then refresh .dot
	@git -C $(TOP) pull -q
	@$(MK) dot

push: ## refresh .dot, commit everything (MSG=...), push
	@$(MK) dot
	@git -C $(TOP) add -A
	@git -C $(TOP) diff-index --quiet HEAD -- || git -C $(TOP) commit -qm "$(MSG)"
	@git -C $(TOP) push -q -u origin HEAD

sh: ## bash tuned by .dot/bashrc; shows $(BANNER) if it exists
	@here=$(TOP) dot=$(ME) BANNER=$(abspath $(BANNER)) bash --rcfile $(ME)/bashrc -i

status: ## short git status
	@git -C $(TOP) status -sb

~/tmp/%.pdf: %.py ## .py ==> .pdf (make ~/tmp/x.pdf)
	@mkdir -p ~/tmp
	@echo "pdf-ing $@ ..."
	@a2ps -Br --chars-per-line=80 --file-align=fill --line-numbers=1 \
	  --borders=no --pro=color --columns=3 -M letter -o - $< | ps2pdf - $@
	@open $@

## ------------------------------------------------ siblings
walks: ## run RULE=x in every ~/gh/*/.git (make walks RULE=demo)
	@for r in $(REPOS); do echo "== $$r"; \
	  f=; [ -f $$r/Makefile ] || f="-f $(DOT)/Makefile"; \
	  $(M) -C $$r $$f -n $(RULE) >/dev/null 2>&1 || { echo "   (no $(RULE) here)"; continue; }; \
	  $(M) -C $$r $$f $(RULE) || exit 1; done

dots:    ## every sibling: dot
	@$(MK) walks RULE=dot
pulls:   ## every sibling: pull (gh/dot first, so .dot is fresh)
	@$(M) -C $(DOT) pull
	@$(MK) walks RULE=pull
pushs:   ## every sibling: push
	@$(MK) walks RULE=push
statuss: ## every sibling: status
	@$(MK) walks RULE=status
