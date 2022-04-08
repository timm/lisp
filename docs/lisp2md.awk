#!/usr/bin/env  bash
cat $1 |
gawk '
/^; vim:/ { next }
/^;\./     { next}
{ gsub( /^;;;; /,"# ") 
  gsub( /^;;; /,"## ") 
  gsub( /^;; /,"### ") 
  gsub( /^; /,"")  
  gsub( /^\(/,"\n(")
  print $0 } '  |
gawk '
BEGIN { FS="\n"; RS=""}
      { R[++r]  = $0
        Code[r] = $0 ~ /)[ \t]*$/ }
END   { for(i=1;i<=r;i++)  
        {   if (!Code[i] &&  Code[i+1]) { print(R[i]"\n\n```lisp"); continue }
            if ( Code[i] && !Code[i+1]) { print(R[i]"\n```\n"); continue }
            print R[i]"\n\n"_
        }
}'
