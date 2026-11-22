# hi: lisp as tiny-type, N-column, colored html (a2ps for the browser).
# One pass: named-group alternation, first rule wins, so strings
# beat comments beat keywords. Each ";;;; name ---" banner starts a
# block with the next pale background. Usage: python3 hi.py COLS FILE
import re, sys, html

KW = ("defun defmacro defmethod defm defstruct defvar defparameter "
      "defpackage let if when unless cond case and or not loop dolist "
      "dotimes lambda setf incf push progn aif flet labels in-package "
      "load return-from destructuring-bind")
RULES = [
  ("str", r'"(?:\\.|[^"\\])*"'),                     # "..." with \" escapes
  ("com", r";.*"),
  ("fun", r"(?<=\(defun )[^\s()]+|(?<=\(defm )[^\s()]+"
          r"|(?<=\(defmacro )[^\s()]+|(?<=\(defstruct )[^\s()]+"),
  ("key", r"(?<![\w-]):[\w=-]+"),                   # :for :collect :=
  ("kw",  r"(?<![\w-])(?:%s)(?![\w-])" % "|".join(map(re.escape, KW.split()))),
  ("num", r"(?<![\w-])\d+\.?\d*(?![\w-])")]
COLOR = dict(str="#a31515", com="#6a737d", fun="#795e26",
             kw="#0033b3",  num="#098658", n="#b0b0b0", key="#8250df")
BG  = ["#fff4e0","#eaf8ec","#f1ecfd","#e2f6f6",
       "#fdfbd8","#fdecf3","#f1f1f1","#ffe9e2"]
PAT = re.compile("|".join("(?P<%s>%s)" % kv for kv in RULES), re.M)

cols, path = sys.argv[1], sys.argv[2]
src  = open(path).read()
out, i = "", 0       # match raw text, escape after: "<=" must not hit ";.*"
for m in PAT.finditer(src):
  out += html.escape(src[i:m.start()], quote=False)
  out += '<span style="color:%s">%s</span>' % (
           COLOR[m.lastgroup or "n"], html.escape(m.group(), quote=False))
  i = m.end()
out += html.escape(src[i:], quote=False)

print('<meta charset=utf-8><title>%s</title>'
      '<meta name=viewport content="width=device-width,initial-scale=1">'
      '<link rel=stylesheet href="https://fonts.googleapis.com/css2'
      '?family=JetBrains+Mono:wght@400&display=swap">'
      '<style>@page{size:letter landscape;margin:.4in}'
      'html{-webkit-text-size-adjust:100%%;text-size-adjust:100%%}'
      '@media screen and (max-width:700px){body{margin:4px}'
      'pre{columns:1!important;font-size:calc(100vw/50)!important;'
      'white-space:pre-wrap;overflow-wrap:anywhere}}</style>'
      '<body style="background:#ffffff">' % path.split("/")[-1])
print('<pre style="font:6.5pt/1.4 JetBrains Mono,Monaco,Menlo,monospace;'
      'color:#1f2328;columns:%s 80ch;column-gap:2em;'
      'column-rule:1px solid #dddddd">' % cols)
bg, k = "#ffffff", 0
for n, (raw, line) in enumerate(zip(src.split("\n"), out.split("\n")), 1):
  if raw.startswith(";;;;"): bg, k = BG[k % len(BG)], k + 1
  print('<span style="display:block;background:%s">'
        '<span style="color:%s">%4d</span> %s</span>'
        % (bg, COLOR["n"], n, line), end="")
print("</pre>")
