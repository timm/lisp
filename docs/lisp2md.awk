#!/usr/bin/env  bash
cat $1 |
gawk '
/^; vim:/ { next }
{ gsub( /^;;; /,"## "); 
  gsub( /^;; /,"### "); 
  gsub( /^; /,""); 
  gsub( /^(/,"\n(")
  gsub(/^#\| /,"\n")
  gsub(/^\|# /,"\n")
  print $0 } ' | 
gawk '
BEGIN { FS="\n"; RS=""}
      { R[++r]  = $0
        Code[r] = $0 ~ /)[ \t]*$/ }
END   { for(i=1;i<=r;i++)  {
           if (Code[r] && !Code[r-1])
              print("\n<details><summary>Source</summary>\n```lisp\n"$0)
           if (Code[r] && !Code[r+1])
              print($0"\n```\n</details>")
           else print("\n"$0) }
        if (Code[r]) print("```")
}'
            
