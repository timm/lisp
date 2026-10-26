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
  file=~/gits/moot/optimize/misc/auto93.csv
  train=           a cached training set
  test=            a cached test set
  go=help          start-up action: help, secs]]

local _ENV = setmetatable({}, {__index = _G})
if setfenv then setfenv(1, _ENV) end   -- lua 5.1, luajit

floor, exp, sqrt, log = math.floor, math.exp, math.sqrt, math.log
abs, min, max = math.abs, math.min, math.max

TBL,NUM,SYM,BINS,RMEANS,NB,BAG,SAME = {},{},{},{},{},{},{},{}

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

function NUM.bin(i,v,    b)         -- 1..the.bins
  if v == "?" then return v end
  b = 1 + floor(the.bins * i:norm(v))
  return b > the.bins and the.bins or b end

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
  return i end

function NB.like(i,sub,row,    l,col,v) -- log P(row|sub), m-estimated
  l = log((#sub.rows + the.m) / (i.n + the.m * i.nKlass)) -- m per klass
  for at, col1 in ipairs(sub.x) do
    col, v = i.tbl.x[at], row[col1.at]   -- col knows every value;
    if v ~= "?" then                     -- col1 knows this klass's
      l = l + log(((col1.has[v] or 0) + the.m)       -- and m per value
                  / (col1.n + the.m * col:nVals())) end end
  return l end

function NB.guess(i,row,    best,out,l) -- row's likeliest klass
  for v, sub in pairs(i.subs) do
    l = i:like(sub, row)
    if not best or l > best then best, out = l, v end end
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
function Same(xs,ys)                -- two samples, sorted, to compare
  table.sort(xs); table.sort(ys)
  return new(SAME, {xs = xs, ys = ys}) end

function SAME.ok(i)                 -- do all three tests say "same"?
  return i:cohen() <= .35 and i:cliffs() <= .195 and i:ks() <= 1.36 end

function SAME.cohen(i,    a,b,sd)   -- gap of means, in pooled sds
  a, b = nums(i.xs, same1), nums(i.ys, same1)
  sd = sqrt(((a.n - 1) * a:div() ^ 2
             + (b.n - 1) * b:div() ^ 2) / (a.n + b.n - 2))
  return abs(a.mu - b.mu) / (sd + 1e-32) end

function same1(x) return x end      -- nums() wants a selector

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

-- ---- rig -------------------------------------------------------------
function secs(file,    tbl,reps,tot,t0,tr,te,sub,y,model,lim,seed)
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

-- ---- start up ------------------------------------------------------
eg = {}                            -- eg[the.go]() runs at start up

function eg.help() print(help) end
function eg.secs() secs(the.file) end

for k, v in help:gmatch"\n%s+(%a+)=(%S+)" do the[k] = thing(v) end
if (arg and arg[0] or ""):find"jiffy%.lua$" then eg[cli().go]() end

return _ENV
