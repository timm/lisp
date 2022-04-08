#!/usr/bin/env  bash
cat $1 |
gawk '
/^; vim:/ { next }
/^;-/     { next}
{ gsub( /^;;;; /,"# ") 
  gsub( /^;;; /,"## ") 
  gsub( /^;; /,"### ") 
  gsub( /^; /,"")  
  gsub( /^\(/,"\n(")
  print $0 } '  |
gawk '
BEGIN { FS="\n"; RS=""}
      { R[++r]  = $0
        Code[r] = $0 ~ /)[ \t]*$/ }
END   { for(i=1;i<=r;i++)  {
           if (Code[i] && !Code[i-1])
              print("\n```lisp\n" R[i])
              #print("\n<details><summary>Source</summary>\n```lisp\n"$0)
           else if (Code[i] && !Code[i+1])
              print(R[i]"\n```\n")
              #print($0"\n```\n</details>")
           else print("\n"R[i]) }
        if (Code[i]) print("```")
}'
