# vi: set ts=2 sw=2 sts=2 et :
END  { print "</body></html>" }
BEGIN { 
  print "<html><head>"
  print "<title>"FILENAME"</title>"
  print "<link media=all rel=stylesheet href='pygments-tango.css' />"
  print "<style type=\"text/css\">"
  print "body {"
  print "  font-family:  Optima, Segoe, 'Segoe UI', Candara, Calibri, Arial, sans-serif;"
  print "  line-height: 1.3;"
  print "  max-width: 700px;"
  print "  padding: 5px;"
  print "  margin: auto;"
  print "  background: #EEE;"
  print "}"
  print "pre { padding:3px; border-bottom: 1px solid #; margin-left: 25px;"
  print "      border-radius: 10px;"
  print "      box-shadow: 0 3px 7px rgb(0 0 0 / 0.2);"
  print "      background:   #EEE; }"
  print "img.right200 { width: 200px; border:1px solid #0039a6; padding:3px; margin:5px; float:right; }"      
  print "</style>" 
  print "</head><body>" }

/vi: set/            { next }
/^.(setf|eg|def|in-package)/ { if (!IN) print "\n```lisp"; IN=1 }
/^$/                 { if (IN)  print "```";IN=0 }
                     { gsub(/(#\||\|#)/,"")
                       gsub(/^[;]*/,"")
                       print }








  
