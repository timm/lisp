$(HOME)/tmp/%.html : %.lisp
	cat $^ | gawk -f 2md.awk | python3 markup.py > $@

