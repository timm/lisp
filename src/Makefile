SHELL=/bin/bash
LISP=/opt/homebrew/bin/sbcl --script

bye:; git add *;git commit -am save;git push;git status

lisp:;     $(LISP) $f.lisp               2> >( awk ' {print} /^5:/ {exit}' )
test:;     $(LISP) sublime.lisp    -t $f 2> >( awk ' {print} /^5:/ {exit}' )
cautious:; $(LISP) sublime.lisp -c -t $t 2> >( awk ' {print} /^5:/ {exit}' )
