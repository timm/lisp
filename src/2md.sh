BEGIN { print "<html><head>"
       print "<style type=\"text/css\">"
        print  "body {"
         print  "  font-family: Optima, Segoe, "Segoe UI", Candara, Calibri, Arial, sans-serif;"
         print  "  line-height: 1.4;"
         print  "  max-width: 45rem;"
         print  "  padding: 2rem;"
         print  "  margin: auto;"
         print  "    background: #edeef2;"
         print  "}"
         print  "pre { font: small; border: 1px dotted lightblue; background: #E0E0E0;}"
         print  "img.right200 { width: 200px; border:1px solid #999; padding:3px; margin:5px; float:right; }"      
         print "</style>" 
         print "</head><body>" }
END   { print "</body></html>" }

/vi: set/            { next }
/^.(def|in-package)/ { if (!IN) print "\n<pre>"; IN=1 }
/^$/                 { if (IN)  print "</pre>";IN=0 }
                     { gsub(/(#\||\|#)/,"")
                       gsub(/^[;]*/,"")
                       print }




  
