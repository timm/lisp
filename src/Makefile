SHELL=/bin/bash

f ?= wicked.lisp
l ?= $f

F ?=""
HERE=$(shell git rev-parse --show-toplevel)
DATA=$(HERE)/etc/data
SRC=$(HERE)/src

help: ## show help
	@grep -E '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort | gawk '\
	BEGIN {FS = ":.*?## "};{printf "make \033[36m %-7s\033[0m : %s\n",$$1,$$2}'

h1:; @figlet -W -flarry3d     $(read) | sed 's/^/#  /' ## large heading
h2:; @figlet -W -fcybermedium $(read) | sed 's/^/#  /' ## medium heading
h3:; @figlet -W -fsmall       $(read) | sed 's/^/#  /' ## small heading
h4:; @figlet -W -fmini        $(read) | sed 's/^/#  /' ## tiny heading

ed:;    @vim  -u $(HERE)/etc/dotvimrc $f ## edit
clisp:; @clisp -q $F $l                     ## run clisp
repl:;  @rlwrap sbcl --noinform          ## repl
tmux:;  @tmux -f $(HERE)/etc/dottmux     ## run tmux
sbcl:;  sbcl --noinform --script $l $F 2> >( gawk ' {print} /^5:/ {exit}' )  ## run sbcl
bye:;   @git add *; git commit -am save;git push;git status ## save to github


eg1:
	F="--seed 23" $(MAKE) sbcl
