SHELL = /bin/bash
LISP  = /opt/homebrew/bin/sbcl --script
f    ?= sublime
t    ?= all
SOME  = 2> >( awk '{print} /^5:/ {exit}') 

bye:; git add *;git commit -am save;git push;git status

lisp:;     @$(LISP) $f.lisp               $(SOME) 
test:;     @$(LISP) sublime.lisp    -t $f $(SOME)
cautious:; @$(LISP) sublime.lisp -c -t $t $(SOME)
