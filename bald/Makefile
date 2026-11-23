# bald (lisp): hair, less hair. "make" = help.
SHELL := bash
.SILENT:

help: ## show this help
	grep -E '^[a-z.%/]+:.*##' Makefile | sed 's/:.*## /\t/'

msg ?= saving

push: ## git add this dir, commit (msg="..."), push
	git add . && { git commit -m "$(msg)" || true; } && git push

run: ## run every eg (sbcl --script bald.lisp all)
	sbcl --script bald.lisp all
