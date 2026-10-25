#!/usr/bin/env lua
-- vim: set ts=2 sw=2 sts=2 et cc=75 :
local the,help = {}, [[
min.lua: a good guess from a few dozen labels.
Range means, shrunk to the grand mean, weighed by variance.
(c) 2026 Tim Menzies <timm@ieee.org> MIT license.

usage: lua min.lua [name=value ...] [file.csv]

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

function thing(s)                   -- one cell: number, or the string
  s = s:match"^%s*(.-)%s*$"
  return tonumber(s) or s end

function cells(s,    t) t = {}
  for v in (s .. ","):gmatch"(.-)," do t[#t + 1] = thing(v) end
  return t end

function csv(file,    t,s) t = {} -- rows; first one is the names
  for line in io.lines((file:gsub("^~", os.getenv"HOME" or "~"))) do
    s = line:match"^%s*(.-)%s*$"
    if s ~= "" and s:sub(1, 1) ~= "#" then t[#t + 1] = cells(s) end end
  return t end

-- ---- columns ---------------------------------------------------------
function col(txt,at)                -- ! + - : goals.  X: skip.  A-Z: num
  txt, at = txt or "N", at or 0    -- no args: an anonymous num
  return {txt = txt, at = at, n = 0, mu = 0, m2 = 0, has = nil,
          w = txt:find"%-$" and 0 or 1,
          num = txt:find"^%u" ~= nil} end

function add(c,v,    d)             -- counts for syms, Welford for nums
  if v == "?" then return end
  c.n = c.n + 1
  if not c.num then
    c.has = c.has or {}
    c.has[v] = (c.has[v] or 0) + 1
  else
    d = v - c.mu
    c.mu = c.mu + d / c.n
    c.m2 = c.m2 + d * (v - c.mu) end end

function div(c,    e)               -- sd for nums, entropy for syms
  if not c.num then
    e = 0
    for _, n in pairs(c.has or {}) do e = e - n / c.n * log(n / c.n) end
    return e / log(2) end
  if c.n < 2 then return 0 end
  return sqrt(c.m2 / (c.n - 1)) end

function norm(c,v,    z)            -- logistic cdf; syms pass through
  if not c.num or v == "?" then return v end
  z = (v - c.mu) / (div(c) + 1e-32)
  z = z > 3 and 3 or (z < -3 and -3 or z)
  return 1 / (1 + exp(-1.7 * z)) end

function bin(c,v,    b)             -- 1..the.bins; a sym is its own bin
  if not c.num or v == "?" then return v end
  b = 1 + floor(the.bins * norm(c, v))
  return b > the.bins and the.bins or b end

-- ---- tables ----------------------------------------------------------
function tbl(src,    i)             -- a file name, or names then rows
  i = {rows = {}}
  for _, r in ipairs(type(src) == "string" and csv(src) or src) do
    row(i, r) end
  return i end

function head(i,names,    c)
  i.names, i.cols, i.x, i.y = names, {}, {}, {}
  for at, txt in ipairs(names) do
    c = col(txt, at)
    i.cols[#i.cols + 1] = c
    if     txt:find"X$"    then                  -- summarized, skipped
    elseif txt:find"[-+!]$" then i.y[#i.y + 1] = c
    else                         i.x[#i.x + 1] = c end end end

function row(i,r)                   -- first row is the names
  if not i.cols then return head(i, r) end
  i.rows[#i.rows + 1] = r
  for _, c in ipairs(i.cols) do add(c, r[c.at]) end
  return r end

function clone(i,rows,    out)      -- same header, fresh summaries
  out = {i.names}
  for _, r in ipairs(rows or {}) do out[#out + 1] = r end
  return tbl(out) end

function ydist(i,r,    n,d,v)       -- goals to heaven; 0 is best
  n, d = 0, 0
  for _, c in ipairs(i.y) do
    n = n + 1
    v = norm(c, r[c.at]) - c.w
    d = d + (v < 0 and -v or v) ^ the.p end
  return (d / n) ^ (1 / the.p) end

-- ---- rmeans ----------------------------------------------------------
function nums(lst,f,    c)          -- lst, summarized in one num
  c = col()
  for _, x in ipairs(lst) do add(c, f(x)) end
  return c end

function cut(c,rows,y,all,    a,b)
  a = {bins = {}, w = 0}
  for _, r in ipairs(rows) do
    b = bin(c, r[c.at])
    a.bins[b] = a.bins[b] or col()
    add(a.bins[b], y(r)) end
  a.w = vcut(a, all)
  return a end

function vcut(a,all,    s)          -- variance a's bins cut from all
  s = 0
  for _, v in pairs(a.bins) do s = s + (v.n / all.n) * div(v) ^ 2 end
  s = div(all) ^ 2 - s
  return s > 0 and s or 0 end

function train(i,rows,y,    j,all,m)
  j   = clone(i, rows)
  all = nums(j.rows, y)
  m   = {all = all, cuts = {}, cols = j.x}
  for _, c in ipairs(j.x) do m.cuts[c.at] = cut(c, j.rows, y, all) end
  return m end

function guess(m,r,    s,w,a,v,mu)  -- bin means, m-estimated, weighed
  s, w, mu = 0, 0, m.all.mu
  for _, c in ipairs(m.cols) do
    a = m.cuts[c.at]
    v = a and a.bins[bin(c, r[c.at])]
    if v and a.w > 0 then
      s = s + a.w * (v.n * v.mu + the.m * mu) / (v.n + the.m)
      w = w + a.w end end
  if w > 0 then return s / w end
  return m.all.mu end

-- ---- bayes -----------------------------------------------------------
function klass(i)                   -- the one sym goal column
  for _, c in ipairs(i.y) do if not c.num then return c end end end

function discrete(i,    names,out,r1) -- x nums become bin ids, so syms
  names = {}
  for _, c in ipairs(i.cols) do
    names[c.at] = c.num and c.txt:lower() or c.txt end
  out = {names}
  for _, r in ipairs(i.rows) do
    r1 = {}
    for _, c in ipairs(i.cols) do r1[c.at] = bin(c, r[c.at]) end
    out[#out + 1] = r1 end
  return tbl(out) end

function learn(i,rows,    h,k,n,v)  -- a clone of i per klass, and how
  h, k, n = {}, klass(i), 0         -- many klasses that turned out to be
  for _, r in ipairs(rows) do
    v = r[k.at]
    if not h[v] then h[v], n = clone(i), n + 1 end
    row(h[v], r) end
  return h, n end

function card(c,    n)              -- how many values c has shown
  if not c.card then
    n = 0
    for _ in pairs(c.has or {}) do n = n + 1 end
    c.card = n > 1 and n or 2 end
  return c.card end

function like(i,j,r,n,k,    l,c,v)  -- log P(r | j), everything m-ed
  l = log((#j.rows + the.m) / (n + the.m * k))
  for at, c1 in ipairs(j.x) do
    c, v = i.x[at], r[c1.at]
    if v ~= "?" then
      l = l + log((((c1.has or {})[v] or 0) + the.m)
                  / (c1.n + the.m * card(c))) end end
  return l end

function label(i,h,r,n,k,    best,out,l) -- the likeliest klass for r
  for v, j in pairs(h) do
    l = like(i, j, r, n, k)
    if not best or l > best then best, out = l, v end end
  return out end

-- ---- scoring ---------------------------------------------------------
function tally(a,want,got)          -- one guess, into a's counts
  a.n, a.ok = a.n + 1, a.ok + (want == got and 1 or 0)
  a.want[want] = (a.want[want] or 0) + 1
  a.got[got]   = (a.got[got] or 0) + 1
  if want == got then a.hit[got] = (a.hit[got] or 0) + 1 end end

function parts(a,x,    tp,fp,fn)    -- x's tp, fp, fn, tn, from counts
  tp = a.hit[x] or 0
  fp = (a.got[x] or 0) - tp
  fn = (a.want[x] or 0) - tp
  return tp, fp, fn, a.n - fn - fp - tp end

function one(a,x,    tp,fp,fn,tn,r1,f1) -- x's recall, false alarm, d2h
  tp, fp, fn, tn = parts(a, x)
  r1 = tp / (tp + fn)
  f1 = fp + tn > 0 and fp / (fp + tn) or 0
  return r1, f1, sqrt((1 - r1) ^ 2 + f1 ^ 2) / sqrt(2) end

function abcd(a,    k,pd,pf,d2h,r1,f1,d1) -- macro: a klass counts once
  k, pd, pf, d2h = 0, 0, 0, 0
  for x in pairs(a.want) do
    r1, f1, d1 = one(a, x)
    k, pd, pf, d2h = k + 1, pd + r1, pf + f1, d2h + d1 end
  return 100 * a.ok / a.n, 100 * pd / k, 100 * pf / k, d2h / k, k end

-- ---- stats -----------------------------------------------------------
function cohen(xs,ys,    a,b,sd)    -- gap of the means, in pooled sds
  a, b = nums(xs, same1), nums(ys, same1)
  sd = sqrt(((a.n-1) * div(a)^2 + (b.n-1) * div(b)^2) / (a.n+b.n-2))
  return math.abs(a.mu - b.mu) / (sd + 1e-32) end

function same1(x) return x end      -- nums() wants a selector

function cliffs(xs,ys,    gt,lt,j,k) -- how often xs out-ranks ys
  gt, lt, j, k = 0, 0, 0, 0
  for _, x in ipairs(xs) do
    while j < #ys and ys[j+1] <  x do j = j + 1; k = j end
    while k < #ys and ys[k+1] == x do k = k + 1 end
    gt, lt = gt + j, lt + #ys - k end
  return math.abs(gt - lt) / (#xs * #ys) end

function ks(xs,ys,    p,q,d,v)      -- widest gap of two cdfs, in crits
  p, q, d = 0, 0, 0
  while p < #xs and q < #ys do
    v = math.min(xs[p+1], ys[q+1])
    while p < #xs and xs[p+1] == v do p = p + 1 end
    while q < #ys and ys[q+1] == v do q = q + 1 end
    d = math.max(d, math.abs(p/#xs - q/#ys)) end
  return d / sqrt((#xs + #ys) / (#xs * #ys)) end

function same(xs,ys)                -- do all three tests say "same"?
  table.sort(xs); table.sort(ys)
  return cohen(xs,ys)  <= .35  and 
         cliffs(xs,ys) <= .195 and 
         ks(xs,ys)     <= 1.36 end

-- ---- rig -------------------------------------------------------------
function split(i,    rows,n,tr,te) -- never shuffles i.rows in place
  rows, tr, te = {}, {}, {}
  for j, r in ipairs(i.rows) do rows[j] = r end
  n = floor(.8 * #shuffle(rows))
  for j, r in ipairs(rows) do
    if j <= n then tr[#tr + 1] = r else te[#te + 1] = r end end
  return tr, te end

function secs(file,    all,reps,tot,t0,tr,te,tr2,y,m,lim,seed)
  all, reps, tot, seed = tbl(file), 3, 0, the.seed
  for s = 1, reps do
    the.seed = seed + s
    tr, te = split(all)
    tr2 = clone(all, tr)
    y   = function(r) return ydist(tr2, r) end
    lim = the.budget - the.check
    t0  = os.clock()
    do
      local few = {}
      for j = 1, (#tr < lim and #tr or lim) do few[j] = tr[j] end
      m = train(all, few, y)
      for _, r in ipairs(te) do guess(m, r) end
    end
    tot = tot + (os.clock() - t0) end
  print(string.format("%s rows=%6d secs=%.4f",
                      file:match"([^/]+)%.csv$", #all.rows,
                      tot / reps)) end

-- ---- start up ------------------------------------------------------
go = {}                            -- go[the.go]() runs at start up

function go.help() print(help) end
function go.secs() secs(the.file) end

for k, v in help:gmatch"\n%s+(%a+)=(%S+)" do the[k] = thing(v) end
if (arg and arg[0] or ""):find"min%.lua$" then go[cli().go]() end

return _ENV
