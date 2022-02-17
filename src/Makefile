SHELL = /bin/bash
LISP  = /opt/homebrew/bin/sbcl --script
f    ?= sublime
t    ?= all
SOME  = 2> >( awk '{print} /^5:/ {exit}') 

bye:; git add *;git commit -am save;git push;git status

lisp:;     @$(LISP) $f.lisp               $(SOME) 
test:;     @$(LISP) sublime.lisp    -t $f $(SOME)
cautious:; @$(LISP) sublime.lisp -c -t $t $(SOME)

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



