# weave.awk -- pull code out of LISP, into markdown prose.
#
#     gawk -f weave.awk lib.lisp lib.md > tmp && mv tmp lib.md
#
# lib.md holds your prose and updates itself in place. A line at
# column 0 naming a form is a directive:
#
#     (defun fred
#     (defvar *seed*
#     (set-macro-character #\$
#
# ...or, equivalently, a fenced block naming one:
#
#     ```lisp
#     (defun fred
#     ```
#
# weave replaces either with the form's doc (docstring, else the
# ";;" block above it) and then its code, fenced. There are no
# markers. A generated region is self-describing: the fence says
# which form it holds, and the region runs from the last truly
# empty line down to that fence's close. So it must contain no
# empty line of its own -- any blank inside the doc or the code
# is written as a line holding one space, which every markdown
# reader treats as a break but which is not "^$".
#
# Two rules follow. Leave an empty line between your own prose
# and a generated block, or your prose is inside the region and
# gets overwritten. And do not hand-write a lisp fence whose
# first line names a real form: weave will assume it wrote it.
#
# Docstrings are markdown. Continuation lines align under the
# opening quote, and that alignment is stripped. A line ending
# in "e.g." fences the lines below it as code, until a blank
# line or the end of the docstring; e.g.
#
#     (defmacro aif (test then &optional else)
#       "THEN and ELSE read TEST's value as `it`; e.g.
#        (aif (parse thing) (print it))"
#       `(let ((it ,test)) (if it ,then ,else)))
#
# Two rules follow from reading the .lisp as text, not as lisp:
# a top-level form must start at column 0, so no line inside a
# docstring may; and the docstring must be the first line of the
# body whose first character is a quote.
#
# Flags:
#   -v jekyll=1  prepend empty YAML front matter (GitHub Pages)
#   -v keepdoc=1 leave the docstring inside the shown code
#   -v strict=1  exit 1 if a form in the .lisp is never woven
#
# Exit 1 on an unresolved directive -- that is the whole point.
# A renamed defun must break the build, not rot silently.

BEGIN { Fence = "```" }

# ---- pass 1: index the lisp -------------------------------
# A top-level form starts at column 0 with "(" and runs until
# the next column-0 "(" or ";", or EOF.
FNR == NR {
  Src[FNR] = $0
  Nsrc = FNR
  Lisp = FILENAME
  next
}

FNR == 1 && NR > 1 { index_forms(); if ($0 != "---") frontmatter() }

# ---- pass 2: rewrite the markdown -------------------------
# Lines are held back until the next empty line, because a fence
# naming a form retro-actively claims the doc buffered above it.
/^$/ { spill(); print ""; next }

$0 ~ /^```/ { fence(); next }

# a bare directive: "(defun fred" -- two tokens, no ")"
$0 ~ /^\([^ \t)]+[ \t]+[^ \t)]+[ \t]*$/ && (formkey($0) in Form) {
  Nbuf = 0
  emit($0)
  next
}

{ Buf[++Nbuf] = $0 }

END {
  spill()
  if (Nsrc && !Npass2) frontmatter()          # the .md was empty
  if (strict)
    for (k in Form)
      if (!(k in Used) && k ~ /^(def|set)/) nag("never woven: (" k)
  exit Bad
}

# ---- indexing ---------------------------------------------
function index_forms(   i, s, key, start) {
  Npass2 = 1
  for (i = 1; i <= Nsrc; i++) {
    s = Src[i]
    if (s !~ /^\(/) continue
    key = formkey(s)
    if (key == "") continue
    if (key in Form) warn("duplicate form: (" key)
    start = i
    for (i++; i <= Nsrc; i++)
      if (Src[i] ~ /^[(;#]/) break     # "#" catches #+sbcl (...)
    Form[key] = slurp(start, i - 1)
    Note[key] = comments_above(start)
    i--                      # let the outer i++ land on the boundary
  }
}

# "(defun fred (x)" ==> "defun fred"
function formkey(s,   a, n) {
  sub(/^\(/, "", s)
  n = split(s, a, /[ \t]+/)
  if (n < 2) return ""
  sub(/\(.*$/, "", a[2])
  return a[1] " " a[2]
}

# lines start..stop, trailing blanks trimmed
function slurp(start, stop,   i, out) {
  while (stop > start && Src[stop] ~ /^[ \t]*$/) stop--
  for (i = start; i <= stop; i++) out = out Src[i] "\n"
  return out
}

# the ";;" block directly above a form (no blank line between)
function comments_above(start,   i, out, s) {
  for (i = start - 1; i >= 1 && Src[i] ~ /^;;/; i--) {
    s = Src[i]
    if (s ~ /^;;;/)            break   # section banner, not doc
    if (s ~ /^;;<!--/)         break   # vim modeline
    if (s ~ /^;;[ \t]*-{4,}/)  break   # rule
    sub(/^;;/, "", s); sub(/^ /, "", s)
    out = s "\n" out
  }
  return out
}

# ---- emitting ---------------------------------------------
function emit(dir,   key, code, doc) {
  key = formkey(dir)
  if (!(key in Form)) { warn("no such form: (" key); return }
  Used[key] = 1
  code = Form[key]
  doc  = examples(docstring(code))
  if (doc == "") doc = Note[key]
  if (doc == "") nag("no doc for: (" key)
  if (!keepdoc) code = undoc(code)

  if (doc != "") { sub(/\n+$/, "", doc); print nogap(doc) }
  print Fence "lisp"
  printf "%s", nogap(code)
  print Fence
}

# No empty line may survive inside a generated region, or the
# next run will mistake it for the region's upper edge.
function nogap(s) {
  gsub(/\n\n/, "\n \n", s)
  sub(/^\n/, " \n", s)
  return s
}

# A fenced block. If its first line names a form we know, it is
# one of ours: drop it, and the doc buffered above it, and write
# the form out fresh.  Anything else is yours, kept verbatim.
function fence(   open, first, line) {
  open = $0
  if ((getline first) <= 0) { spill(); print open; return }
  if (formkey(first) in Form) {
    Nbuf = 0
    emit(first)
    while ((getline line) > 0) if (line ~ /^```/) break
    return
  }
  Buf[++Nbuf] = open
  Buf[++Nbuf] = first
  if (first !~ /^```/)
    while ((getline line) > 0) { Buf[++Nbuf] = line
                                 if (line ~ /^```/) break }
}

function spill(   i) {
  for (i = 1; i <= Nbuf; i++) print Buf[i]
  Nbuf = 0
}

# Locate the docstring: the first line of the body (searching at
# most 5 lines in, so an arglist may wrap) whose first non-blank
# character is a quote.  Sets Ds/De to its line range, else Ds=0.
function docrange(a, n,   i, seen) {
  Ds = De = 0
  for (i = 2; i <= n && seen < 5; i++) {
    if (a[i] !~ /[^ \t]/) continue
    seen++
    if (a[i] !~ /^[ \t]*"/) continue
    Ds = i
    for (De = i; De <= n; De++)
      if (closes(a[De], De == i)) return
    De = n
    return
  }
}

# does this line carry the docstring's closing quote?
function closes(s, first,   q) {
  q = gsub(/\\"/, "", s)            # ignore escaped quotes
  q = gsub(/"/, "", s)
  return first ? q >= 2 : q >= 1
}

# The docstring is a string literal, so its whitespace is its
# content: an example indented 4 inside the quotes is meant to be
# a markdown code block.  Only the opening line gets trimmed.
function docstring(code,   a, n, i, s, base, out) {
  n = split(code, a, "\n")
  docrange(a, n)
  if (!Ds) return ""
  match(a[Ds], /^[ \t]*"/)      # continuations align under the quote
  base = RLENGTH
  for (i = Ds; i <= De; i++) {
    s = (i == Ds) ? a[i] : chop(a[i], base)
    if (i == Ds) sub(/^[ \t]*"/, "", s)
    if (i == De) sub(/"[ \t]*\)*[ \t]*$/, "", s)
    gsub(/\\"/, "\"", s)
    out = out s "\n"
  }
  sub(/\n+$/, "", out)
  return out
}

# Drop the docstring from the code we display -- but only when
# it is pure prose.  In (defvar *x* 1 "doc") the closing paren
# rides on the docstring line, so removing it breaks the form.
function undoc(code,   a, n, i, out, tail) {
  n = split(code, a, "\n")
  docrange(a, n)
  if (Ds) {
    tail = a[De]
    sub(/^.*"/, "", tail)
    gsub(/[ \t]/, "", tail)
    if (tail ~ /^\)*$/) a[Ds - 1] = a[Ds - 1] tail   # (defvar x 1 "doc")
    else               Ds = 0                       # give up, keep it
  }
  for (i = 1; i <= n; i++) {
    if (i == n && a[i] == "") continue
    if (Ds && i >= Ds && i <= De) continue
    out = out a[i] "\n"
  }
  return out
}

# A docstring line ending in "e.g." turns the lines below it into
# a code block, closed by a blank line or by the end of the
# docstring.  So an example needs no blank line and no 4-space
# indent inside the quotes -- it just follows the "e.g.", aligned
# under the opening quote.  That alignment is stripped here.
function examples(doc,   a, n, i, out, incode) {
  n = split(doc, a, "\n")
  for (i = 1; i <= n; i++) {
    if (incode && a[i] !~ /[^ \t]/) {      # blank closes the block,
      out = out Fence "\n"                 # and is swallowed: a gap
      incode = 0                           # would end the region
      continue
    }
    out = out a[i] "\n"
    if (!incode && a[i] ~ /[eE]\.?[gG]\.:?[ \t]*$/) {
      out = out Fence "lisp\n"
      incode = 1
    }
  }
  if (incode) out = out Fence "\n"
  sub(/\n+$/, "", out)
  return out
}

function chop(s, n,   i) {
  for (i = 0; i < n && s ~ /^[ \t]/; i++) s = substr(s, 2)
  return s
}

function frontmatter() {
  if (!jekyll) return
  print "---"
  print "layout: default"
  base = Lisp; sub(/^.*\//, "", base)
  print "title: " base
  print "---"
  print ""
}

# nag = report it, keep going.  warn = report it, fail the build.
function nag(msg)  { print "weave: " msg > "/dev/stderr" }
function warn(msg) { nag(msg); Bad = 1 }
