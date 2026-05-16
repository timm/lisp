# vim: set ft=make ts=2 sw=2 noet :
# teach/Makefile -- dev shell, tasks, push, pdf in one file.

SHELL := /bin/bash

GIT_ROOT := $(shell git rev-parse --show-toplevel 2>/dev/null)
ETC      := $(GIT_ROOT)/etc

## Defaults: override at command line, e.g. make tree O=cars.csv
O ?= ../auto93.csv                                # optimization file
C ?= $(HOME)/gits/timm/moot/classify/diabetes.csv # classification file

WISE := sbcl --script wise.lisp
NB   := sbcl --script nb.lisp

NVIM_INIT := /tmp/teach-nvim-init.lua
Chars     ?= 70

.PHONY: help the tree atree active check nb diabetes soybean heart ncheck push sh

## ===========================================================
## help
## ===========================================================

help: ## show this help
	@awk 'BEGIN{FS=":.*##"; \
	            printf "\nusage: make TARGET [VAR=val]\n\n"} \
	      /^[a-zA-Z][a-zA-Z0-9_-]*:.*##/ { \
	            printf "  \033[36m%-12s\033[0m %s\n", $$1, $$2 \
	      }' $(MAKEFILE_LIST)
	@printf "\npdf: make ~/tmp/wise.pdf  (Chars=$(Chars))\n\n"

## ===========================================================
## wise.lisp tasks
## ===========================================================

the: ## print *the* config
	@$(WISE) --the

tree: ## plain-budget tree on $$O
	@$(WISE) -f $(O) --tree

atree: ## active-learning tree on $$O
	@$(WISE) -f $(O) --atree

active: ## 20 active-learning train/test runs on $$O
	@$(WISE) -f $(O) --active

check: ## run wise.lisp invariants on $$O
	@$(WISE) -f $(O) --check

## ===========================================================
## nb.lisp tasks
## ===========================================================

nb: ## naive Bayes on $$C
	@$(NB) -f $(C) --nb

diabetes: ## NB on diabetes.csv
	@$(MAKE) -s nb C=$(HOME)/gits/timm/moot/classify/diabetes.csv

soybean: ## NB on soybean.csv
	@$(MAKE) -s nb C=$(HOME)/gits/timm/moot/classify/soybean.csv

heart: ## NB on heart.c.csv
	@$(MAKE) -s nb C=$(HOME)/gits/timm/moot/classify/heart.c.csv

ncheck: ## run nb.lisp assertions
	@$(NB) --check

## ===========================================================
## git
## ===========================================================

push: ## prompt msg + commit -am + push + status
	@read -p "Reason? " msg; \
	 git commit -am "$$msg" && git push && git status

## ===========================================================
## pdf  (make ~/tmp/foo.pdf  =>  finds foo.lisp here)
## ===========================================================

~/tmp/%.pdf: %.lisp $(ETC)/def.ssh ## .lisp -> .pdf via a2ps
	@mkdir -p $(@D); \
	 echo "pdf-ing $@ ..."; \
	 a2ps --pretty-print=$(ETC)/def.ssh -Br --quiet --landscape \
	      --pro=color --chars-per-line=$(Chars) \
	      --line-numbers=1 --borders=no --columns=3 \
	      -M letter -o - $< | ps2pdf - $@; \
	 command -v open >/dev/null && open $@ || true

## ===========================================================
## dev shell -- banner, aliases, prompt, embedded nvim init
## ===========================================================

define NVIM_INIT_LUA
-- netrw
vim.g.netrw_banner = 0
vim.g.netrw_liststyle = 3
vim.g.netrw_browse_split = 4
vim.g.netrw_winsize = 15

-- ui / editing
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.cursorline = true
vim.opt.mouse = "a"
vim.opt.clipboard = "unnamedplus"
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.expandtab = true
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.splitright = true
vim.opt.splitbelow = true

-- undo
vim.opt.undofile = true
vim.opt.undodir = vim.fn.expand("~/.vim/undo")

-- statusline
vim.opt.statusline = "%#StatusLine# ▶ %f %m%r%=%y ❖ %l:%c ❖ %p%% "
vim.opt.laststatus = 2

-- keymaps
vim.keymap.set("n", "Q", ":quitall<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>s", ":set spell!<CR>", { silent = true })

-- colors
vim.cmd.colorscheme("sorbet")

-- lisp indent: built-in + extra forms via lispwords
vim.api.nvim_create_autocmd("FileType", {
  pattern = "lisp",
  callback = function()
    vim.bo.lisp = true
    vim.bo.lispwords = vim.bo.lispwords .. ",defmethods,defeg,let+"
  end,
})
endef
export NVIM_INIT_LUA

define SHELL_RC
alias ls='ls --color'
alias grep='grep --color=auto'
alias blat='bat -l clojure --theme="Nord"'
alias vi='nvim -u $(NVIM_INIT)'

export PATH="$(CURDIR):$$PATH"
export EDITOR=nano
export HISTSIZE=10000
export HISTFILESIZE=20000

bold=$$(tput bold); col0=$$(tput sgr0)
col1=$$(tput setaf 6); col2=$$(tput setaf 3)
branch() { git branch 2>/dev/null | awk '/^\*/ {print $$2}'; }
dirty()  { [[ -n $$(git status -s 2>/dev/null) ]] && echo "*"; }
PROMPT_COMMAND='PS1="🧩\[$${bold}$${col1}\]$$(basename "$$(dirname "$$PWD")")/$$(basename "$$PWD")\[$${col0}\] \[$${col2}\]$$(branch)$$(dirty)\[$${col0}\] ▶ "'

printf '\033[H\033[2J\033[3J\033[1;33m\n'
cat <<'ART'
       ___
      (o,o)
      {\`"'}
      -"-"-
ART
printf '\033[1;36m\n'
printf " 'What we have to learn to\n"
printf "  do, we learn by doing.'\n"
printf '            -- Aristotle\n'
printf '\033[0m\n'
endef
export SHELL_RC

sh: ## launch dev shell (banner + aliases + nvim)
	@printf '%s\n' "$$NVIM_INIT_LUA" > $(NVIM_INIT); \
	 tmprc=$$(mktemp); \
	 printf '%s\n' "$$SHELL_RC" > $$tmprc; \
	 bash --init-file $$tmprc -i; \
	 rm -f $$tmprc
