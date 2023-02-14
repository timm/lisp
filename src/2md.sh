BEGIN { FS="\n"; RS="";print "<html><body>"}
     { 
       gsub(/[(](eg|defmacro|defun|defmethod|defvar|defpackage) /,"<pre><code>&")
       gsub(/^[;]+ /,"\n")
       gsub(/\n[;]+ /,"\n")
       print "\n\n" $0 "\n</code></pre>" }
