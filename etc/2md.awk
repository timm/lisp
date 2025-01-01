# vi: set ts=2 sw=2 sts=2 et :
/vi: set/            { next }
/^.(setf|eg|def|in-package)/ { if (!IN) print "\n<pre>"; IN=1 }
/^$/                 { if (IN)  print "</pre>";IN=0 }
IN                   { gsub(/"[^"]*"/,"<font color=darkred><b>&</b></font>")
                       gsub(/\y(eg|defun|defvar|defstruct|defmacro|in-package)\y/,"<font color=darkblue><b>&</b></font>") }
                     { gsub(/(#\||\|#)/,"")
                       gsub(/^[;]*/,"")
                       print }
