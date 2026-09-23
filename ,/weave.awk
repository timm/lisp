# weave.awk -- pull code out of LISP, into markdown prose.
#
#     gawk -f weave.awk lib.lisp lib.md > tmp && mv tmp lib.md
#
# The .md holds your prose and updates itself in place. Any
# paragraph holding a "```lisp" fence whose first line names a
# form in the .lisp is replaced by that form's doc, then its
# code, freshly fenced. A fence naming no known form is yours,
# kept verbatim.
#
# Both files are read a paragraph (blank line) at a time, which
# is what keeps this tool small. The author carries the rules:
#
# .lisp -- one empty line between top-level paragraphs; a blank
#   INSIDE a form is a line holding one space. A form's doc is
#   the ";;" lines atop its paragraph, else its docstring: the
#   opening quote at column 3, at most 3 lines below the header
#   (an arglist may wrap), continuations aligned under the
#   quote, and no line but the last may end in a quote.
#
#   Docstrings are markdown, so an example needs no machinery
#   here: a one-space line, then lines indented 4 past the
#   alignment, and markdown itself makes the code block.
#
# .md -- a woven region is ONE paragraph: doc, fence, code, with
#   any blank in it a line holding one space. So leave an empty
#   line between your own prose and a generated block, or your
#   prose is inside the region and gets overwritten.
#
# Flags:
#   -v strict=1  exit 1 if a def/set form is never woven; this
#                is the tripwire that makes a renamed defun
#                break the build instead of rotting silently.

BEGIN { RS = ""; ORS = "\n\n" }

NR == FNR {                                 # pass 1: index the lisp
  n = split($0, L, "\n"); doc = ""
  for (i = 1; L[i] ~ /^;/; i++)             # ";;" atop the para is
    if (L[i] ~ /^;;([^;]|$)/)               # doc; ";" and ";;;" are
      { s = L[i]; sub(/^;; ?/, "", s); doc = doc s "\n" }   # not
  if ((key = formkey(L[i])) == "") next     # headers, banners, etc
  hdr = i
  for (ds = hdr + 1; ds <= hdr + 3 && L[ds] ~ /^[ \t]*[^ \t"]/; ds++) ;
  if (L[ds] ~ /^[ \t]*"/) {                 # docstring: lines ds..de
    for (de = ds; de < n && L[de] !~ /"[ \t)]*$/; de++) ;
    tail = L[de]; sub(/^.*"/, "", tail); gsub(/[ \t]/, "", tail)
    L[hdr] = L[hdr] tail                    # re-close (defvar x 1 "d")
    for (i = ds; i <= de; i++) {
      s = substr(L[i], 4)                   # strip the alignment
      if (i == ds) sub(/^[ \t]*"/, "", s)
      if (i == de) sub(/"[ \t)]*$/, "", s)
      doc = doc (s == "" ? " " : s) "\n"    # no "^$" inside a region
    }
  } else ds = de = 0
  code = ""
  for (i = hdr; i <= n; i++)
    if (i < ds || i > de) code = code L[i] "\n"
  Rep[key] = doc "```lisp\n" code "```"
  next
}

{                                           # pass 2: rewrite the md
  if (match($0, /(^|\n)```lisp\n/) &&
      (key = formkey(substr($0, RSTART + RLENGTH))) in Rep) {
    Used[key] = 1
    print Rep[key]; next
  }
  print
}

END {
  if (strict)
    for (k in Rep)
      if (!(k in Used) && k ~ /^(def|set)/)
        { print "weave: never woven: (" k > "/dev/stderr"; Bad = 1 }
  exit Bad
}

# "(defun fred (x)" ==> "defun fred"
function formkey(s,   a) {
  if (s !~ /^\(/) return ""
  sub(/\n.*/, "", s); sub(/^\(/, "", s)
  if (split(s, a, /[ \t]+/) < 2) return ""
  sub(/\(.*/, "", a[2])
  return a[1] " " a[2]
}
