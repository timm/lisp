GIT_ROOT := $(shell git rev-parse --show-toplevel 2>/dev/null)

push: ## save to cloud
	@read -p "Reason? " msg; git commit -am "$$msg"; git push; git status

sh: ## demo of my shell
	@-bash --init-file $(GIT_ROOT)/ezr/.bashrc -i

Lines?=95

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
