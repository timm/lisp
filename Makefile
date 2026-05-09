SHELL    := /bin/bash
GIT_ROOT := $(shell git rev-parse --show-toplevel 2>/dev/null)
ETC      := $(GIT_ROOT)/etc
A2PS_DIR := $(shell a2ps --list=defaults 2>/dev/null \
              | awk '/library path/{getline; print $$1; exit}')

CLS     := '\033[H\033[J'
cRESET  := '\033[0m'
cYELLOW := '\033[1;33m'

help: ## show help
	@awk 'BEGIN{FS=":.*##"} \
	      /^[a-zA-Z_%\/.~$$-]+:.*##/ \
	      {printf "  \033[36m%-20s\033[0m %s\n",$$1,$$2}' \
	      $(MAKEFILE_LIST)

sh: ## launch dev shell (banner + etc/bash.rc)
	@-echo -e $(CLS)$(cYELLOW); figlet -W -f mini '(lisp)'; \
	  echo -e $(cRESET)
	@-bash --init-file $(ETC)/bash.rc -i

push: ## commit with prompted msg and push
	@read -p "Reason? " msg; git commit -am "$$msg"; git push; git status

$(A2PS_DIR)/def.ssh: $(ETC)/def.ssh ## install a2ps style
	@cp $< $@

## ezr-plus.lisp examples
EZR   := sbcl --script ezr-plus.lisp
DATA  ?= auto93.csv
CLASS ?= $(HOME)/gits/timm/moot/classify/heart.c.csv

tree: ## decision/regression tree on $$DATA
	@$(EZR) -f $(DATA) --tree

nb: ## naive Bayes on $$CLASS
	@$(EZR) -f $(CLASS) --nb

soybean diabetes: ## naive Bayes per-class metrics
	@$(EZR) -f $(HOME)/gits/timm/moot/classify/$@.csv --nb

acquire: ## active learning on $$DATA
	@$(EZR) -f $(DATA) --acquire

the: ## print options
	@$(EZR) --the

all: ## run every main example
	@for t in tree nb soybean diabetes acquire; do \
	   echo; echo "=== $$t ==="; $(MAKE) -s $$t; \
	 done

Chars ?= 65
~/tmp/%.pdf: %.lisp Makefile $(GIT_ROOT)/etc/def.ssh ## .lisp ==> .pdf
	@mkdir -p ~/tmp
	@echo "pdf-ing $@ ... "
	@a2ps --pretty-print=$(GIT_ROOT)/etc/def.ssh -Br --quiet --landscape   \
	      --pro=color --chars-per-line=$(Chars)        \
	      --line-numbers=1 --borders=no --columns=3    \
	      -M letter -o - $< | ps2pdf - $@
	@open $@
