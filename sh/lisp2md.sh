echo "# " $1
cat - | gawk '
BEGIN { on=0 }
END   { if (on) print "```" }

NR == 1 && /^#!/ { next }        # Skip shebang
    { sub(/^\f$/, "") }              # Treat ^L as blank
    { if (/^\(/ && !on) { print "\n```lisp"; on = 1 }
      if (on) {
        if (/^$/) { print "```\n"; on = 0
        } else { print }
      } else {
        gsub(/\n;*/, "\n")
        sub(/^;+[ ]*/, "")
        print }}'
