#!/usr/bin/env lua
-- vim: set ts=2 sw=2 sts=2 et cc=75 :
local the,help = {}, [[
jiffy.lua: a good guess from a few dozen labels.
Range means, shrunk to the grand mean, weighed by variance.
(c) 2026 Tim Menzies <timm@ieee.org> MIT license.

usage: lua jiffy.lua [name=value ...] [file.csv]

flags:
  bins=7           numeric columns get this many bins
  budget=50        labels, all in
  check=5          of those, saved for the test rows
  m=2              m-estimate: prior weight for a thin bin
  p=2              minkowski coefficient
  seed=1234567891  random number seed
  w=0              1: weight x by info gain, 2: sqrt, 3: select
  parts=4          nbc: how many clusters to aim for
  stop=30          nbc: never cluster below this many rows
  enough=50        nbc: rows per klass needed, to cluster
  sup=0            1: mdl cuts, 2: equal count, 4: merge up
  small=8          a bin thinner than this always merges
  knn=5            how many neighbours vote
  per=40           neighbours are sought in this many per klass
  pick=0           1: spread the sample over x, not by klass
  file=~/gits/moot/optimize/misc/auto93.csv
  train=           a cached training set
  test=            a cached test set
  go=help          start-up action: help, secs]]

local _ENV = setmetatable({}, {__index = _G})
if setfenv then setfenv(1, _ENV) end   -- lua 5.1, luajit

floor, exp, sqrt, log = math.floor, math.exp, math.sqrt, math.log
abs, min, max = math.abs, math.min, math.max

TBL,NUM,SYM,BINS,RMEANS,NB,AODE,BAG,SAME = {},{},{},{},{},{},{},{},{}

function new(meta,i)                -- i is now one of meta's kind
  meta.__index = meta; return setmetatable(i, meta) end

-- ---- general ---------------------------------------------------------
function cli(k,v)                   -- awk-ish operands: name=value, file
  for _, s in ipairs(arg) do
    k, v = s:match"^(%a%w*)=(.*)$"
    if k then the[k] = thing(v) else the.file = s end end 
  return the end

function rint(n)                    -- 1..n, repeatable from the.seed
  the.seed = (16807 * the.seed) % 2147483647
  return 1 + floor(n * the.seed / 2147483647) end

function shuffle(t,    j)           -- in place, Fisher-Yates
  for i = #t, 2, -1 do j = rint(i); t[i], t[j] = t[j], t[i] end
  return t end

function map(t,f,    u)             -- f over t's items, in order
  u = {}
  for j, v in ipairs(t) do u[j] = f(v) end
  return u end

function thing(s)                   -- one cell: number, or the string
  s = s:match"^%s*(.-)%s*$"
  return tonumber(s) or s end

function cells(s,    row) row = {}
  for v in (s .. ","):gmatch"(.-)," do row[#row + 1] = thing(v) end
  return row end

function csv(file,    rows,s) rows = {} -- first row is the names
  for line in io.lines((file:gsub("^~", os.getenv"HOME" or "~"))) do
    s = line:match"^%s*(.-)%s*$"
    if s ~= "" and s:sub(1, 1) ~= "#" then
      rows[#rows + 1] = cells(s) end end
  return rows end

-- ---- nums ------------------------------------------------------------
function Num(txt,at)                -- ! + -: goals.  X: skip.  A-Z: num
  txt, at = txt or "N", at or 0    -- no args: an anonymous num
  return new(NUM, {txt = txt, at = at, n = 0, mu = 0, m2 = 0,
                   w = txt:find"%-$" and 0 or 1}) end

function NUM.add(i,v,    d)         -- Welford's running mean and sd
  if v == "?" then return end
  i.n = i.n + 1
  d = v - i.mu
  i.mu = i.mu + d / i.n
  i.m2 = i.m2 + d * (v - i.mu) end

function NUM.div(i)                 -- standard deviation
  return i.n < 2 and 0 or sqrt(i.m2 / (i.n - 1)) end

function NUM.norm(i,v,    z)        -- logistic cdf, clipped at 3 sds
  if v == "?" then return v end
  z = (v - i.mu) / (i:div() + 1e-32)
  z = z > 3 and 3 or (z < -3 and -3 or z)
  return 1 / (1 + exp(-1.7 * z)) end

function NUM.bin(i,v,    b)         -- 1..the.bins, or supervised cuts
  if v == "?" then return v end
  if i.cut then                     -- klass-aware edges, if we have them
    for b = 1, #i.cut do if v <= i.cut[b] then return b end end
    return #i.cut + 1 end
  b = 1 + floor(the.bins * i:norm(v))
  return b > the.bins and the.bins or b end

function ent(has,n,    e,v)         -- entropy of a klass tally
  e = 0
  for _, m in pairs(has) do v = m / n; e = e - v * log(v) / log(2) end
  return e end

function NUM.cuts(i,xy,lo,hi,out,    best,cut,a,b,n1,n2,j,k,e,e1,e2,gain,d)
  -- Fayyad-Irani: split [lo,hi] of the sorted (value,klass) list xy at
  -- the point that most reduces klass entropy, if MDL says it is worth it
  a, b, n1, n2 = {}, {}, 0, 0
  for j = lo, hi do b[xy[j].y] = (b[xy[j].y] or 0) + 1; n2 = n2 + 1 end
  if n2 < 4 then return out end
  e = ent(b, n2)
  for j = lo, hi - 1 do
    d = xy[j].y
    a[d], b[d] = (a[d] or 0) + 1, b[d] - 1
    if b[d] == 0 then b[d] = nil end
    n1, n2 = n1 + 1, n2 - 1
    if xy[j].x ~= xy[j + 1].x then
      e1, e2 = ent(a, n1), ent(b, n2)
      gain = e - (n1 * e1 + n2 * e2) / (n1 + n2)
      if not best or gain > best then
        best, cut, k = gain, j, {e1 = e1, e2 = e2, n1 = n1, n2 = n2} end end end
  if not cut then return out end
  n1, n2 = k.n1, k.n2
  j = 0                              -- klasses on each side, for the MDL
  for _ in pairs(b) do j = j + 1 end
  d = 0
  for _ in pairs(a) do d = d + 1 end
  gain = (log(n1 + n2 - 1) / log(2)
          + log(3 ^ (d + j) - 2) / log(2)
          - ((d + j) * e - d * k.e1 - j * k.e2)) / (n1 + n2)
  if best <= gain then return out end            -- MDL says: do not cut
  out = i:cuts(xy, lo, cut, out)
  out[#out + 1] = xy[cut].x
  return i:cuts(xy, cut + 1, hi, out) end

function nums(lst,f,    i)          -- lst, summarized in one num
  i=Num(); for _,x in ipairs(lst) do i:add(f(x)) end; return i end

-- ---- syms ------------------------------------------------------------
function Sym(txt,at)                -- a-z names; counts, not Welford
  txt, at = txt or "S", at or 0
  return new(SYM, {txt = txt, at = at, n = 0, has = {},
                   w = txt:find"%-$" and 0 or 1}) end

function SYM.add(i,v)               -- one more of v
  if v == "?" then return end
  i.n = i.n + 1
  i.has[v] = (i.has[v] or 0) + 1 end

function SYM.div(i,    e)           -- entropy
  e = 0
  for _, n in pairs(i.has) do e = e - n/i.n*log(n/i.n) end
  return e / log(2) end

function SYM.nVals(i,    n)         -- how many values i has seen
  if not i.kinds then
    n = 0
    for _ in pairs(i.has) do n = n + 1 end
    i.kinds = n > 1 and n or 2 end  -- one value still splits two ways
  return i.kinds end

function SYM.norm(i,v) return v end -- syms pass through
function SYM.bin(i,v)  return v end   -- a sym is its own bin

-- ---- tables ----------------------------------------------------------
function Tbl(src,    i)             -- a file name, or names then rows
  i = new(TBL, {rows = {}})
  for _, row in ipairs(type(src) == "string" and csv(src) or src) do
    i:addRow(row) end
  return i end

function TBL.head(i,names,    col)  -- the first row, read as names
  i.names, i.cols, i.x, i.y = names, {}, {}, {}
  for at, txt in ipairs(names) do
    col = (txt:find"^%u" and Num or Sym)(txt, at)
    i.cols[#i.cols + 1] = col
    if     txt:find"X$"     then                 -- summarized, skipped
    elseif txt:find"[-+!]$" then i.y[#i.y + 1] = col
      if txt:find"!$" then i.klass = col end  -- last ! is the klass
    else                         i.x[#i.x + 1] = col end end end

function TBL.addRow(i,row)          -- first row is the names
  if not i.cols then return i:head(row) end
  i.rows[#i.rows + 1] = row
  for _, col in ipairs(i.cols) do col:add(row[col.at]) end
  return row end

function TBL.clone(i,rows,how,    out) -- same header, fresh summaries
  out = {how and map(i.cols, function(col) return col.txt:lower() end)
             or i.names}         -- how makes nums discrete, so sym
  for _, row in ipairs(rows or {}) do
    out[#out + 1] = not how and row or map(i.cols,
      function(col) return col[how](col, row[col.at]) end) end
  return Tbl(out) end

function TBL.ydist(i,row,    d,v)   -- goals to heaven; 0 is best
  d = 0
  for _, col in ipairs(i.y) do
    v = col:norm(row[col.at]) - col.w
    d = d + abs(v) ^ the.p end
  return (d / #i.y) ^ (1 / the.p) end

function TBL.split(i,    rows,n,tr,te) -- never shuffles i.rows itself
  rows, tr, te = {}, {}, {}
  for j, row in ipairs(i.rows) do rows[j] = row end
  n = floor(.8 * #shuffle(rows))
  for j, row in ipairs(rows) do
    if j <= n then tr[#tr + 1] = row else te[#te + 1] = row end end
  return tr, te end

-- ---- rmeans ----------------------------------------------------------
function Bins(col,rows,y,all,    i,b) -- col's bins, and their worth
  i = new(BINS, {has = {}, w = 0})
  for _, row in ipairs(rows) do
    b = col:bin(row[col.at])
    i.has[b] = i.has[b] or Num()
    i.has[b]:add(y(row)) end
  i.w = i:weight(all)
  return i end

function BINS.weight(i,all,    s)   -- variance i's bins cut off
  s = 0
  for _,v in pairs(i.has) do s = s + (v.n / all.n) * v:div() ^ 2 end
  s = all:div() ^ 2 - s
  return s > 0 and s or 0 end

function Rmeans(tbl,rows,y,    sub,all,i) -- one bins per x column
  sub = tbl:clone(rows)
  all = nums(sub.rows, y)
  i   = new(RMEANS, {all = all, has = {}, cols = sub.x})
  for _, col in ipairs(sub.x) do
    i.has[col.at] = Bins(col, sub.rows, y, all) end
  return i end

function RMEANS.guess(i,row,    s,w,bins,v,mu) -- bin means, m-estimated,
  s, w, mu = 0, 0, i.all.mu               -- weighed by variance won
  for _, col in ipairs(i.cols) do
    bins = i.has[col.at]
    v    = bins and bins.has[col:bin(row[col.at])]
    if v and bins.w > 0 then
      s = s + bins.w * (v.n * v.mu + the.m * mu) / (v.n + the.m)
      w = w + bins.w end end
  if w > 0 then return s / w end
  return i.all.mu end

-- ---- bayes -----------------------------------------------------------
function Nb(tbl,rows,    i,col,v)   -- one sub-table per klass, and
  col = tbl.klass                   -- how many klasses that turned
  i   = new(NB, {tbl = tbl, subs = {}, n = #rows, nKlass = 0})
  for _, row in ipairs(rows) do
    v = row[col.at]
    if not i.subs[v] then
      i.subs[v], i.nKlass = tbl:clone(), i.nKlass + 1 end
    i.subs[v]:addRow(row) end
  return the.w > 0 and i:weigh() or i end

function NB.weigh(i,    all,here,s,c,n) -- info gain, one per x column
  i.w, s, all = {}, 0, Sym()        -- how much each x says about klass
  for v, sub in pairs(i.subs) do
    all.has[v], all.n = #sub.rows, all.n + #sub.rows end
  for at, col in ipairs(i.tbl.x) do
    n = 0
    for v, _ in pairs(col.has) do    -- what that value leaves unsaid
      here = Sym()
      for k, sub in pairs(i.subs) do
        c = sub.x[at].has[v] or 0
        if c > 0 then here.has[k], here.n = c, here.n + c end end
      if here.n > 0 then n = n + here.n / all.n * here:div() end end
    i.w[at] = math.max(0, all:div() - n)
    s = s + i.w[at] end
  s = s / #i.tbl.x                  -- a mean weight of 1 keeps the
  for at, _ in ipairs(i.w) do       -- likelihoods on their old scale
    i.w[at] = s > 0 and i.w[at] / s or 1
    if     the.w == 2 then i.w[at] = sqrt(i.w[at])     -- softer
    elseif the.w == 3 then i.w[at] = i.w[at] < 1 and 0 or 1 end end -- pick
  return i end

function NB.like(i,sub,row,    l,col,v) -- log P(row|sub), m-estimated
  l = log((#sub.rows + the.m) / (i.n + the.m * i.nKlass)) -- m per klass
  for at, col1 in ipairs(sub.x) do
    col, v = i.tbl.x[at], row[col1.at]   -- col knows every value;
    if v ~= "?" then                     -- col1 knows this klass's
      l = l + (i.w and i.w[at] or 1)                 -- and m per value
              * log(((col1.has[v] or 0) + the.m)
                    / (col1.n + the.m * col:nVals())) end end
  return l end

function NB.guess2(i,row,all,    best,out,l) -- local, backed by global
  for v, sub in pairs(i.subs) do
    l = i:like(sub, row)
    if all.subs[v] then l = l + all:like(all.subs[v], row) end
    if not best or l > best then best, out = l, v end end
  return out or all:guess(row) end

function NB.guess(i,row,    best,out,l) -- row's likeliest klass
  for v, sub in pairs(i.subs) do
    l = i:like(sub, row)
    if not best or l > best then best, out = l, v end end
  return out end

function TBL.supervise(i,rows,    xy,y)  -- give each num its own cuts
  y = i.klass
  for _, col in ipairs(i.x) do
    if getmetatable(col) == NUM then
      xy = {}
      for _, row in ipairs(rows) do
        if row[col.at] ~= "?" then
          xy[#xy + 1] = {x = row[col.at], y = row[y.at]} end end
      table.sort(xy, function(p, q) return p.x < q.x end)
      if     the.sup == 2 then col.cut = col:pkid(xy)    -- equal count
      elseif the.sup == 4 then col.cut = col:chop(xy)    -- merge up
      else col.cut = #xy > 8 and col:cuts(xy, 1, #xy, {}) or nil end
      if col.cut and #col.cut < (the.sup == 3 and 2 or 1) then
        col.cut = nil end end end                   -- too few: fall back
  return i end

function NUM.merge1(a,b,    c,k,v) -- one bin, if it says as much as two
  c = {lo = a.lo, hi = b.hi, n = a.n + b.n, has = {}}
  for _, t in ipairs{a, b} do
    for k, v in pairs(t.has) do c.has[k] = (c.has[k] or 0) + v end end
  if a.n < the.small or b.n < the.small then return c end  -- too thin
  return ent(c.has, c.n) <= (a.n * ent(a.has, a.n)       -- no worse than
                           + b.n * ent(b.has, b.n)) / c.n -- the two apart
         and c or nil end

function NUM.merge(i,b4,    now,j,a,b,ab) -- merge up, till nothing merges
  now, j = {}, 1
  while j <= #b4 do
    a = b4[j]
    if j < #b4 then
      ab = NUM.merge1(a, b4[j + 1])
      if ab then a, j = ab, j + 1 end end
    now[#now + 1] = a
    j = j + 1 end
  return #now == #b4 and now or i:merge(now) end

function NUM.chop(i,xy,    k,out,at,b) -- fine bins, then merge them back
  k = max(the.bins, floor(sqrt(#xy)))
  out = {}
  for j = 1, #xy do
    at = 1 + floor((j - 1) * k / #xy)
    b  = out[at]
    if not b then b = {lo = xy[j].x, n = 0, has = {}}; out[at] = b end
    b.hi, b.n = xy[j].x, b.n + 1
    b.has[xy[j].y] = (b.has[xy[j].y] or 0) + 1 end
  for j = 1, k do if not out[j] then return nil end end  -- ties made holes
  out = i:merge(out)
  if #out < 2 then return nil end
  k = {}
  for j = 1, #out - 1 do k[#k + 1] = out[j].hi end
  return k end

function NUM.pkid(i,xy,    k,out,at)  -- sqrt(n) bins of equal count
  k, out = floor(sqrt(#xy)), {}
  if k < 2 then return nil end
  for j = 1, k - 1 do
    at = floor(j * #xy / k)
    if at >= 1 and xy[at].x ~= xy[at + 1].x then out[#out + 1] = xy[at].x end end
  return out end

-- ---- clusters --------------------------------------------------------
function TBL.xdist(i,row1,row2,    d,a,b) -- minkowski over the x columns
  d = 0
  for _, col in ipairs(i.x) do
    a, b = row1[col.at], row2[col.at]
    if a == "?" and b == "?" then d = d + 1
    elseif getmetatable(col) == SYM then      -- syms: same, or not
      d = d + (a == b and 0 or 1)
    else a, b = col:norm(a), col:norm(b)      -- nums: the gap, 0..1
         if     a == "?" then a = b > .5 and 0 or 1
         elseif b == "?" then b = a > .5 and 0 or 1 end
         d = d + abs(a - b) ^ the.p end end
  return (d / #i.x) ^ (1 / the.p) end

function TBL.far(i,rows,row,    out,most,d) -- the row furthest from row
  most = -1
  for _, r in ipairs(rows) do      -- a sample would do; these are small
    d = i:xdist(row, r)
    if d > most then most, out = d, r end end
  return out, most end

function TBL.tree(i,rows,stop,    a,b,c,out,x,lo,hi) -- halve, recursively
  if #rows <= stop then return {rows = rows} end
  a    = i:far(rows, rows[1 + floor(#rows / 2)])
  b, c = i:far(rows, a)
  if c == 0 then return {rows = rows} end
  out = {}
  for _, r in ipairs(rows) do       -- cosine rule: where r falls on a-b
    out[#out + 1] = {x = i:project(r, a, b, c), row = r} end
  table.sort(out, function(p, q) return p.x < q.x end)
  lo, hi = {}, {}
  for j, p in ipairs(out) do
    local to = j <= floor(#out / 2) and lo or hi
    to[#to + 1] = p.row end
  if #lo == 0 or #hi == 0 then return {rows = rows} end
  return {a = a, b = b, c = c, cut = out[floor(#out / 2)].x,
          lo = i:tree(lo, stop), hi = i:tree(hi, stop)} end

function TBL.project(i,row,a,b,c)   -- where row falls on the a-b line
  return (i:xdist(row, a) ^ 2 + c ^ 2 - i:xdist(row, b) ^ 2)
         / (2 * c + 1e-32) end

function TBL.leaf(i,node,row)       -- run row down the training tree
  while not node.rows do
    node = i:project(row, node.a, node.b, node.c) <= node.cut
           and node.lo or node.hi end
  return node end

function TBL.halve(i,rows,    a,b,c,out,x) -- split on the a-b line
  a    = i:far(rows, rows[1 + floor(#rows / 2)])
  b, c = i:far(rows, a)
  out  = {}
  for _, r in ipairs(rows) do      -- cosine rule: where r falls on a-b
    x = (i:xdist(r, a) ^ 2 + c ^ 2 - i:xdist(r, b) ^ 2) / (2 * c + 1e-32)
    out[#out + 1] = {x = x, row = r} end
  table.sort(out, function(p, q) return p.x < q.x end)
  a, b = {}, {}
  for j, p in ipairs(out) do
    local to = j <= floor(#out / 2) and a or b
    to[#to + 1] = p.row end
  return a, b end

function TBL.cluster(i,rows,stop,    a,b,out) -- leaves of repeated halving
  if #rows <= stop then return {rows} end
  a, b = i:halve(rows)
  if #a == 0 or #b == 0 then return {rows} end
  out = {}
  for _, half in ipairs({a, b}) do
    for _, leaf in ipairs(i:cluster(half, stop)) do out[#out + 1] = leaf end end
  return out end

function TBL.spread(i,rows,n,    out,leaf) -- n rows, spread over x
  out = {}                          -- one from each leaf of a tree cut
  for _, leaf in ipairs(i:cluster(rows, max(2, floor(#rows / n)))) do
    out[#out + 1] = leaf[1 + floor(#leaf / 2)] end
  return out end

-- ---- knn -------------------------------------------------------------
-- The tree is an index: a test row only meets the rows that landed in
-- its leaf, so this is O(leaf), not O(all rows).
function TBL.votes(i,node,row,    all,got,best,out,n) -- knn, inside a leaf
  all = {}
  for _, r in ipairs(i:leaf(node, row).rows) do
    all[#all + 1] = {d = i:xdist(row, r), row = r} end
  table.sort(all, function(p, q) return p.d < q.d end)
  got = {}
  for j = 1, min(the.knn, #all) do                  -- nearer rows count
    n = all[j].row[i.klass.at]                      -- more, as 1/(d+eps)
    got[n] = (got[n] or 0) + 1 / (all[j].d + 1e-9) end
  for k, v in pairs(got) do
    if not best or v > best then best, out = v, k end end
  return out end

-- ---- aode ------------------------------------------------------------
-- One naive Bayes per (attribute,value) parent, averaged.  Each model
-- is still naive, but every pair of attributes is joint in one of them,
-- so interactions that defeat NB survive in the average.
function Aode(tbl,rows,    i,y,va,vb,k1,k2) -- counts, two deep
  i = new(AODE, {tbl = tbl, n = #rows, nKlass = 0, ks = {},
                 c1 = {}, c2 = {}})
  for _, row in ipairs(rows) do
    y = row[tbl.klass.at]
    if not i.ks[y] then i.ks[y], i.nKlass = 0, i.nKlass + 1 end
    i.ks[y] = i.ks[y] + 1
    for a, cola in ipairs(tbl.x) do
      va = row[cola.at]
      if va ~= "?" then
        k1 = y .. "\1" .. a .. "\1" .. va
        i.c1[k1] = (i.c1[k1] or 0) + 1
        for b, colb in ipairs(tbl.x) do
          vb = row[colb.at]
          if b ~= a and vb ~= "?" then
            k2 = k1 .. "\1" .. b .. "\1" .. vb
            i.c2[k2] = (i.c2[k2] or 0) + 1 end end end end end
  return i end

function AODE.guess(i,row,    best,out,s,lo,p,k1,k2,va,vb,n) -- argmax y
  for y, _ in pairs(i.ks) do
    s = nil                         -- sum over parents, in log space
    for a, cola in ipairs(i.tbl.x) do
      va = row[cola.at]
      if va ~= "?" then
        k1 = y .. "\1" .. a .. "\1" .. va
        n  = i.c1[k1] or 0
        if n > 0 then
          p = log((n + the.m) / (i.n + the.m * i.nKlass * cola:nVals()))
          for b, colb in ipairs(i.tbl.x) do
            vb = row[colb.at]
            if b ~= a and vb ~= "?" then
              k2 = k1 .. "\1" .. b .. "\1" .. vb
              p = p + log(((i.c2[k2] or 0) + the.m)
                          / (n + the.m * colb:nVals())) end end
          if not s then s = p        -- log(exp(s) + exp(p)), kept stable
          else lo = s > p and s or p
               s  = lo + log(exp(s - lo) + exp(p - lo)) end end end end
    if s and (not best or s > best) then best, out = s, y end end
  return out or i:anyKlass() end

function AODE.anyKlass(i,    best,out) -- nothing matched: the commonest
  for y, n in pairs(i.ks) do
    if not best or n > best then best, out = n, y end end
  return out end

-- ---- scoring ---------------------------------------------------------
function Bag()                      -- blank counts, for one test set
  return new(BAG, {n = 0, ok = 0, got = {}, want = {}, hit = {}}) end

function BAG.tally(i,want,got)      -- one guess, into the bag
  i.n          = i.n + 1
  i.ok         = i.ok + (want == got and 1 or 0)
  i.want[want] = (i.want[want] or 0) + 1
  i.got[got]   = (i.got[got] or 0) + 1
  if want == got then i.hit[got] = (i.hit[got] or 0) + 1 end end

function BAG.parts(i,x,    tp,fp,fn) -- x's tp, fp, fn, tn, so far
  tp = i.hit[x] or 0
  fp = (i.got[x] or 0) - tp
  fn = (i.want[x] or 0) - tp
  return tp, fp, fn, i.n - fn - fp - tp end

function BAG.abcd1(i,x,    tp,fp,fn,tn,r1,f1) -- x's pd, pf, and d2h
  tp, fp, fn, tn = i:parts(x)
  r1 = tp / (tp + fn)
  f1 = fp + tn > 0 and fp / (fp + tn) or 0
  return r1, f1, sqrt((1 - r1) ^ 2 + f1 ^ 2) / sqrt(2) end

function BAG.abcd(i,    k,pd,pf,d2h,r1,f1,d1) -- macro: one vote each
  k, pd, pf, d2h = 0, 0, 0, 0
  for x in pairs(i.want) do
    r1, f1, d1 = i:abcd1(x)
    k, pd, pf, d2h = k + 1, pd + r1, pf + f1, d2h + d1 end
  return 100*i.ok/i.n, 100*pd/k, 100*pf/k, d2h/k, k end

-- ---- stats -----------------------------------------------------------
function Same(xs,ys,    f)          -- two samples, sorted, to compare
  f = function(t,    out)           -- on copies: the caller's list is
        out = map(t, function(x) return x end)   -- not ours to disturb
        table.sort(out)
        return out end
  return new(SAME, {xs = f(xs), ys = f(ys)}) end

function SAME.ok(i)                 -- do all three tests say "same"?
  return i:cohen() <= .35 and i:cliffs() <= .195 and i:ks() <= 1.36 end

function SAME.cohen(i,    f,a,b,sd) -- gap of means, in pooled sds
  f    = function(x) return x end   -- nums() wants a selector
  a, b = nums(i.xs, f), nums(i.ys, f)
  sd = sqrt(((a.n - 1) * a:div() ^ 2
             + (b.n - 1) * b:div() ^ 2) / (a.n + b.n - 2))
  return abs(a.mu - b.mu) / (sd + 1e-32) end

function SAME.cliffs(i,    xs,ys,gt,lt,j,k) -- how often xs out-ranks ys
  xs, ys, gt, lt, j, k = i.xs, i.ys, 0, 0, 0, 0
  for _, x in ipairs(xs) do
    while j < #ys and ys[j+1] <  x do j = j + 1; k = j end
    while k < #ys and ys[k+1] == x do k = k + 1 end
    gt, lt = gt + j, lt + #ys - k end
  return abs(gt - lt) / (#xs * #ys) end

function SAME.ks(i,    xs,ys,p,q,d,v) -- widest gap of two cdfs, in crits
  xs, ys, p, q, d = i.xs, i.ys, 0, 0, 0
  while p < #xs and q < #ys do
    v = min(xs[p+1], ys[q+1])
    while p < #xs and xs[p+1] == v do p = p + 1 end
    while q < #ys and ys[q+1] == v do q = q + 1 end
    d = max(d, abs(p/#xs - q/#ys)) end
  return d / sqrt((#xs + #ys) / (#xs * #ys)) end

-- ---- start up ------------------------------------------------------
eg = {}                            -- eg[the.go]() runs at start up

function eg.help() print(help) end

function eg.secs(    file,tbl,reps,tot,t0,tr,te,sub,y,model,lim,seed)
  file = the.file                   -- time rmeans: train, then guess
  tbl, reps, tot, seed = Tbl(file), 3, 0, the.seed
  for s = 1, reps do
    the.seed = seed + s
    tr, te = tbl:split()
    sub = tbl:clone(tr)
    y   = function(row) return sub:ydist(row) end
    lim = the.budget - the.check
    t0  = os.clock()
    do
      local few = {}
      for j = 1, (#tr < lim and #tr or lim) do few[j] = tr[j] end
      model = Rmeans(tbl, few, y)
      for _, row in ipairs(te) do model:guess(row) end
    end
    tot = tot + (os.clock() - t0) end
  print(string.format("%s rows=%6d secs=%.4f",
                      file:match"([^/]+)%.csv$", #tbl.rows,
                      tot / reps)) end

for k, v in help:gmatch"\n%s+(%a+)=(%S+)" do the[k] = thing(v) end
if (arg and arg[0] or ""):find"jiffy%.lua$" then eg[cli().go]() end

return _ENV
