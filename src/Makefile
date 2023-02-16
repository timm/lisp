$(HOME)/tmp/%.html : %.lisp
	cp pygments*.css $(HOME)/tmp
	cat $^ | gawk -f 2md.awk | python3 markup.py > $@

