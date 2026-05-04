#!/usr/bin/env python3
"""splot2lisp.py: SPLOT feature-model XML -> nested-list lisp.

Mapping:
  :r/:m name        -> (and name kids...)
  :o name           -> (opt name kids...)
  :g [1,1]          -> (xor kids...)
  :g [N,*] or [N,M] -> (or  kids...)
  : leaf            -> bare atom

Usage: python3 splot2lisp.py FILE.xml > FILE.lisp
"""
import re, sys, pathlib

def slug(name, ident):
    if ident: return ident
    return re.sub(r'\W+', '_', name).strip('_').lower()

def parse_line(ln):
    d = 0
    while d < len(ln) and ln[d] == '\t': d += 1
    s = ln[d:].rstrip()
    sp = s.find(' ')
    kt   = s[:sp] if sp >= 0 else s
    rest = s[sp+1:].strip() if sp >= 0 else ''
    if kt == ':g':
        m = re.search(r'\[(\d+),([\d*]+)\]', rest)
        assert m, f"bad :g cardinality in {rest!r}"
        lo = int(m.group(1))
        hi = 999 if m.group(2) == '*' else int(m.group(2))
        return d, 'g', None, lo, hi
    kind = kt[1:] if len(kt) > 1 else 'leaf'
    m = re.match(r'^(.*?)\(([^()]*)\)\s*$', rest)
    if m:
        name, ident = m.group(1).strip(), m.group(2).strip()
    else:
        name, ident = rest.strip(), ''
    return d, kind, slug(name, ident), 0, 0

def build(lines):
    stack, root = [], None
    for ln in lines:
        if not ln.strip(): continue
        d, kind, ident, lo, hi = parse_line(ln)
        n = dict(d=d, kind=kind, id=ident, lo=lo, hi=hi, kids=[])
        while stack and stack[-1]['d'] >= d: stack.pop()
        if not stack: root = n
        else:         stack[-1]['kids'].append(n)
        stack.append(n)
    return root

def sx(n, indent=0):
    pad = '  ' * indent
    kids = [sx(k, indent+1) for k in n['kids']]
    if n['kind'] == 'g':
        op = 'xor' if (n['lo'], n['hi']) == (1,1) else 'or'
        body = ' '.join(k.lstrip() for k in kids)
        return f"{pad}({op} {body})"
    name = n['id']
    if not kids: return f"{pad}{name}"
    op = 'opt' if n['kind'] == 'o' else 'and'
    inner = '\n'.join(kids)
    return f"{pad}({op} {name}\n{inner})"

def main():
    path = sys.argv[1]
    txt = open(path).read()
    a = txt.find('<feature_tree>') + len('<feature_tree>')
    b = txt.find('</feature_tree>')
    root = build(txt[a:b].split('\n'))
    var = '*' + pathlib.Path(path).stem.lower().replace('-', '_') + '*'
    print(f"(defparameter {var}\n  '{sx(root).lstrip()})")

if __name__ == '__main__':
    main()
