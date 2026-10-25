#!/usr/bin/env lua
-- min.lua: a good guess from a few dozen labels.
-- Range means, shrunk to the grand mean, weighed by variance.
-- (c) 2026 Tim Menzies <timm@ieee.org> MIT license.
-- usage: lua min.lua [Seed=1 Budget=50 Check=5 Bins=5 M=2 P=2] f.csv
-- vim: set ts=2 sw=2 sts=2 et cc=75 :
local _ENV = setmetatable({}, {__index = _G})
if setfenv then setfenv(1, _ENV) end   -- lua 5.1, luajit
local floor, exp, sqrt, log = math.floor, math.exp, math.sqrt, math.log

the = {Seed = 1234567891, Budget = 50, Check = 5,
       Bins = 5, M = 2, P = 2, file = nil}

-- ---- general ---------------------------------------------------------
function knobs(   k, v)            -- awk-ish operands: Name=value, file
  for _, s in ipairs(arg) do
    k, v = s:match"^(%a%w*)=(.*)$"
    if k then the[k] = thing(v) else the.file = s end end end

function rint(n)                   -- 1..n, repeatable from the.Seed
  the.Seed = (16807 * the.Seed) % 2147483647
  return 1 + floor(n * the.Seed / 2147483647) end

function shuffle(t,   j)           -- in place, Fisher-Yates
  for i = #t, 2, -1 do j = rint(i); t[i], t[j] = t[j], t[i] end
  return t end

function thing(s)                  -- one cell: number, or the string
  s = s:match"^%s*(.-)%s*$"
  return tonumber(s) or s end

function cells(s,   t) t = {}
  for v in (s .. ","):gmatch"(.-)," do t[#t + 1] = thing(v) end
  return t end

function csv(file,   t, s) t = {}  -- rows; first one is the names
  for line in io.lines(file) do
    s = line:match"^%s*(.-)%s*$"
    if s ~= "" and s:sub(1, 1) ~= "#" then t[#t + 1] = cells(s) end end
  return t end

-- ---- columns ---------------------------------------------------------
function col(txt, at)              -- ! + - : goals.  X: skip.  A-Z: num
  txt, at = txt or "N", at or 0    -- no args: an anonymous num
  return {txt = txt, at = at, n = 0, mu = 0, m2 = 0, has = nil,
          w = txt:find"%-$" and 0 or 1,
          num = txt:find"^%u" ~= nil} end

function add(c, v,   d)            -- counts for syms, Welford for nums
  if v == "?" then return end
  c.n = c.n + 1
  if not c.num then
    c.has = c.has or {}
    c.has[v] = (c.has[v] or 0) + 1
  else
    d = v - c.mu
    c.mu = c.mu + d / c.n
    c.m2 = c.m2 + d * (v - c.mu) end end

function div(c,   e)               -- sd for nums, entropy for syms
  if not c.num then
    e = 0
    for _, n in pairs(c.has or {}) do e = e - n / c.n * log(n / c.n, 2) end
    return e end
  if c.n < 2 then return 0 end
  return sqrt(c.m2 / (c.n - 1)) end

function norm(c, v,   z)           -- logistic cdf; syms pass through
  if not c.num or v == "?" then return v end
  z = (v - c.mu) / (div(c) + 1e-32)
  z = z > 3 and 3 or (z < -3 and -3 or z)
  return 1 / (1 + exp(-1.7 * z)) end

function bin(c, v,   b)            -- 1..the.Bins; a sym is its own bin
  if not c.num or v == "?" then return v end
  b = 1 + floor(the.Bins * norm(c, v))
  return b > the.Bins and the.Bins or b end

-- ---- tables ----------------------------------------------------------
function tbl(src,   i)             -- a file name, or names then rows
  i = {rows = {}}
  for _, r in ipairs(type(src) == "string" and csv(src) or src) do
    row(i, r) end
  return i end

function head(i, names,   c)
  i.names, i.cols, i.x, i.y = names, {}, {}, {}
  for at, txt in ipairs(names) do
    c = col(txt, at)
    i.cols[#i.cols + 1] = c
    if     txt:find"X$"    then                  -- summarized, skipped
    elseif txt:find"[-+!]$" then i.y[#i.y + 1] = c
    else                         i.x[#i.x + 1] = c end end end

function row(i, r)                 -- first row is the names
  if not i.cols then return head(i, r) end
  i.rows[#i.rows + 1] = r
  for _, c in ipairs(i.cols) do add(c, r[c.at]) end
  return r end

function clone(i, rows,   out)     -- same header, fresh summaries
  out = {i.names}
  for _, r in ipairs(rows or {}) do out[#out + 1] = r end
  return tbl(out) end

function ydist(i, r,   n, d, v)    -- goals to heaven; 0 is best
  n, d = 0, 0
  for _, c in ipairs(i.y) do
    n = n + 1
    v = norm(c, r[c.at]) - c.w
    d = d + (v < 0 and -v or v) ^ the.P end
  return (d / n) ^ (1 / the.P) end

-- ---- rmeans ----------------------------------------------------------
function nums(lst, f,   c)         -- lst, summarized in one num
  c = col()
  for _, x in ipairs(lst) do add(c, f(x)) end
  return c end

function arm(c, rows, y, all,   a, b)
  a = {bins = {}, w = 0}
  for _, r in ipairs(rows) do
    b = bin(c, r[c.at])
    a.bins[b] = a.bins[b] or col()
    add(a.bins[b], y(r)) end
  a.w = vcut(a, all)
  return a end

function vcut(a, all,   s)         -- variance a's bins cut from all
  s = 0
  for _, v in pairs(a.bins) do s = s + (v.n / all.n) * div(v) ^ 2 end
  s = div(all) ^ 2 - s
  return s > 0 and s or 0 end

function est(v, all)               -- v's mean, pulled to all's
  return (v.n * v.mu + the.M * all.mu) / (v.n + the.M) end

function train(i, rows, y,   j, all, m)
  j   = clone(i, rows)
  all = nums(j.rows, y)
  m   = {all = all, arms = {}, cols = j.x}
  for _, c in ipairs(j.x) do m.arms[c.at] = arm(c, j.rows, y, all) end
  return m end

function guess(m, r,   s, w, a, v)
  s, w = 0, 0
  for _, c in ipairs(m.cols) do
    a = m.arms[c.at]
    v = a and a.bins[bin(c, r[c.at])]
    if v and a.w > 0 then
      s = s + a.w * est(v, m.all)
      w = w + a.w end end
  if w > 0 then return s / w end
  return m.all.mu end

-- ---- rig -------------------------------------------------------------
function split(i,   rows, n, tr, te)
  rows = shuffle(i.rows)
  n    = floor(.8 * #rows)
  tr, te = {}, {}
  for j, r in ipairs(rows) do
    if j <= n then tr[#tr + 1] = r else te[#te + 1] = r end end
  return tr, te end

function secs(file,   all, reps, tot, t0, tr, te, tr2, y, m, cut)
  all, reps, tot = tbl(file), 3, 0
  for s = 1, reps do
    the.Seed = 1234567891 + s
    tr, te = split(all)
    tr2 = clone(all, tr)
    y   = function(r) return ydist(tr2, r) end
    cut = the.Budget - the.Check
    t0  = os.clock()
    do
      local few = {}
      for j = 1, (#tr < cut and #tr or cut) do few[j] = tr[j] end
      m = train(all, few, y)
      for _, r in ipairs(te) do guess(m, r) end
    end
    tot = tot + (os.clock() - t0) end
  print(string.format("%s rows=%6d secs=%.4f",
                      file:match"([^/]+)%.csv$", #all.rows,
                      tot / reps)) end

knobs()
if the.file then secs(the.file) end
return _ENV
