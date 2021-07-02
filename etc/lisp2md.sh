#!/usr/bin/env  bash
cat $1 |
gawk ' CODE=1
       
{R[++r] = $0 }
END { for(i=1;i<=r;i++) {
if (R[i] ~ /^[ \t]*$/) go=1
if (go){
  
}}

/^; vim:/ { next }
{ gsub(/^; /,"")
  gsub(/^;;; /, "## "); 
  gsub(/^;; / , "### "); 
  gsub(/^; /  , ""); 
  gsub(/^.defun/, "\n(defun")
  gsub(/^.defmacro/, "\n(defmacro")
  gsub(/^#\| /, "\n")
  gsub(/^\|# /, "\n")
  print $0  
}' | 
gawk '
BEGIN { FS="\n"; RS=""}
      { R[++r]  = $0
        Code[r] = $0 ~ /)[ \t]*$/ }
END   { for(i=1;i<=r;i++)  {
           if (Code[i] && !Code[i-1])
              print("\n<details><summary>Source</summary>\n\n```lisp\n"R[i])
           else if (Code[i] && !Code[i+1])
              print(R[i]"\n```\n\n</details>")
           else 
              print("\n"R[i]) 
        }
        if (Code[r]) print("```")
}'

