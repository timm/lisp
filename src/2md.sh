BEGIN { print "<html><head></head><body>" }
END   { print "</body></html>" }

/vi: set/            { next }
/^.(def|in-package)/ { if (!IN) print "\n<pre>"; IN=1 }
/^$/                 { if (IN)  print "</pre>";IN=0 }
                     { gsub(/(#\||\|#)/,"")
                       gsub(/^[;]*/,"")
                       print }
