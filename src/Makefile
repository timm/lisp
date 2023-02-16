# vi: set ts=4 sw=2 sts=4  :
$(HOME)/tmp/%.html : %.lisp 
	cp pygments*.css $(dir $@)
	cat $^ | gawk -f 2md.awk | python3 markup.py > $@

