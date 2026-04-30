SHELL    := /bin/bash
GIT_ROOT := $(shell git rev-parse --show-toplevel 2>/dev/null)
ETC      := $(GIT_ROOT)/etc

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

Lines?=60
~/tmp/%.pdf: %.lisp  Makefile ## .py ==> .pdf
	@mkdir -p ~/tmp
	@echo "pdf-ing $@ ... "
	@a2ps               \
		-Br               \
		--quiet            \
		--landscape          \
    --chars-per-line=$(Lines) \
		--line-numbers=1      \
		--borders=no           \
		--pro=color             \
		--columns=3              \
		-M letter                 \
		-o - $< | ps2pdf - $@
	@open $@
