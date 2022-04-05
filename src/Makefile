SHELL = /bin/bash
PATH := /opt/homebrew/bin:$(PATH)
LISP  = /opt/homebrew/bin/sbcl --script
f    ?= sublime
t    ?= all
SOME  = 2> >( awk '{print} /^5:/ {exit}') 

bye:; git add *;git commit -am save;git push;git status

lisp:;     @$(LISP) $f.lisp               $(SOME) 
test:;     @$(LISP) sublime.lisp    -t $f $(SOME)
cautious:; @$(LISP) sublime.lisp -c -t $t $(SOME)

COMMENT= gawk ' /^[ \t]*$$/ {next} \
                            {print "---    "$$0} \
                        END {print""}'

help:
	@printf "\n\033[33mmake [OPTIONS]\033[0m\n\nOPTIONS:\n"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%10s :\033[0m %s\n", $$1, $$2}'

h1:; @figlet -W -flarry3d     $t   | $(COMMENT)
h4:; @figlet -W -fmini $${t%%.*}   | $(COMMENT)
h3:; @figlet -W -fsmall  $${t%%.*} | $(COMMENT)
h2:; @figlet -W -fcybermedium $t   | $(COMMENT)

#----------------------------------------------------
pdfs: ../docs/pdf/sublime.pdf
ok:
	@mkdir -p ../docs/pdf

../docs/pdf/%.pdf : %.lisp ok
	@a2ps -q -BjR --line-numbers=1               \
           --borders=no --pro=color --columns 2 \
           --right-footer="" --left-footer=""    \
           --footer="page %p."                    \
           -M letter -o $@.ps $<
	@ps2pdf $@.ps $@; rm $@.ps ; git add $@



