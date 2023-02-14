cat $1 | gawk '/^--/ {next} 1' | gawk '
BEGIN {FS="\n"; RS=""}
sub(/^[ \t]*$/,"") 
$1 !~ /^function/ {next}
/function COL\(/,/function selects\(/{ print "ai",NF; fun1++; lines1+= NF;next}
{ print"misc",NF; fun2++; lines2+= NF}
END {
   print "ai",fun1, lines1, lines1/fun1
   print "misc",fun2, lines2, lines2/fun2

 }'
