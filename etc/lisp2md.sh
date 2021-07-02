#!/usr/bin/env gawk -f

cat $1 | gawk '
/; vim:/      {next}
gsub(/; /,"") {Com=1}
gsub(/;/,"")  {Com=1}
$0 ~/^\(/     {print "\n"}
              {print $0}
' | gawk ' 
BEGIN         {RS=""; FS="\n"}
              {Now=0}
$0 ~ /^\(/    {Now=1}
 Now &&  B4   {print "" }
 Now && !B4   {print "\n<ul><details><summary>CODE</summary>\n\n```lisp"}
!Now &&  B4   {print "```\n\n</details</ul>>\n"}
              {print $0}
              {B4=Now}
END           {if (Now) print "```"}
'
#if (!Code) {print "\n```lisp\n" $0; Code=1;} next}
#Code && !B4Code     {print "\n```lisp" }
#!Com && !B4Com      {print "```\n"}
#                    {print $0}
#                    {B4Com=Com; B4Code=Code; Last=$0}
#END                 {if (B4Code) print "```"}

