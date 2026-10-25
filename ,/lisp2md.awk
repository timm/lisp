# lisp2md.awk -- commented LISP ==> markdown.
#
#     gawk -f lisp2md.awk lib.lisp > lib.md
#     gawk -f lisp2md.awk lib.lisp | pandoc -o lib.pdf
#
# Lines starting ";;" are markdown; everything else is code.
# RS="" walks the file one blank-line-separated block at a time,
# so each doc-plus-definition stays together.
#
# One special line. A ";; -- " one-liner is the byline, and its
# "<br>"-separated fields become author/email/url metadata:
#
#     ;; -- Tim Menzies<br>timm@ieee.org<br>http://timm.fyi
#
# And tips are auto-numbered. Write the category, not the number:
#
#     ;; TIP (lisp): macros go near the top of the file.
#
# becomes "[TIP 7]{.tip .lisp}", which etc/code.lua turns into an
# icon badge in LaTeX, and a <span class="tip lisp"> in HTML.

BEGIN { RS=""; FS="\n" }

      { code=0
        for(i=1; i<=NF; i++) {
          s = $i
          if (s ~ /^;;<!--/)      continue          # vim modelines
          if (s ~ /^;;[ \t]*-- /) { byline(s); continue }
          if (s ~ /^;;/)          { if (code) {out("```"); code=0}
                                    sub(/^;;/,"",s); sub(/^ /,"",s)
                                    if (s ~ /^-{4,}[ \t]*$/) continue
                                    out(tips(s)) }
          else                    { if (!code) {out(""); out("```lisp"); code=1}
                                    out(s) }}
        if (code) out("```")
        out("") }

END   { if (Author) { print "---"
                      print "author: \"" Author "\""
                      if (Email) print "email: \""  Email "\""
                      if (Url)   print "url: \""    Url   "\""
                      print "---"; print "" }
        printf "%s", Md }

function out(s) { Md = Md s "\n" }

# TIP (cat) and TIPn (cat) both ==> "[TIP i]{.tip .cat}"
function tips(s,   pre,cat,rest) {
  while (match(s, /TIP[0-9]*[ \t]*\([a-zA-Z]+\)/)) {
    pre  = substr(s, 1, RSTART-1)
    cat  = substr(s, RSTART, RLENGTH)
    rest = substr(s, RSTART+RLENGTH)
    gsub(/^TIP[0-9]*[ \t]*\(|\)$/, "", cat)
    s = pre "[TIP " ++Tip "]{.tip ." tolower(cat) "}" rest }
  return s }

function byline(s,   n,a) {
  sub(/^;;[ \t]*-- /,"",s)
  n = split(s, a, /<br>[ \t]*/)
  Author = a[1]; Email = a[2]; Url = a[3] }
