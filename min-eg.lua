#!/usr/bin/env lua
-- vim: set ts=2 sw=2 sts=2 et cc=75 :
-- min-eg.lua: the rigs.  A split is a seed, not a file: `rint` is
-- Park-Miller and stays under 2^53, so lua, python and lisp all
-- shuffle a table into the same order.  Only scores reach the disk.
-- Everything here is idempotent: work already cached is never
-- redone (pass force=y to insist).
-- usage: luajit min-eg.lua go=nb     file=f.csv
--        luajit min-eg.lua go=rmeans file=f.csv
--        luajit min-eg.lua go=splits file=f.csv  (what seed s gives)
package.path = (arg[0]:match"^(.*)/" or ".") .. "/?.lua;" .. package.path
local min = require "min"
if setfenv then setfenv(1, min) end          -- min's _ENV is ours too
local _ENV = min
the = cli()                         -- min's options, off our cli

cache = (os.getenv"HOME" or "~") .. "/tmp/cache"
reps  = 20

-- ---- files -----------------------------------------------------------
function exists(f,    h)            -- is f a file we can read?
  h = io.open(f)
  if h then h:close() end
  return h ~= nil end

function home(f)                    -- cache dir, mirroring moot's tree
  return cache .. "/" ..
         (f:match"moot/(.+)%.csv$" or f:match"([^/]+)%.csv$") end

-- ---- cache -----------------------------------------------------------
function pair(i,s,    tr,te)        -- rep s's two halves of i, by seed
  the.seed = 1234567891 + s
  tr, te = split(i)
  return clone(i, tr), clone(i, te) end

function scored(file,who,fun,what,    dir,f,h,i,t0,line,ok) -- run once
  dir = home(file)
  f   = ("%s/%s.csv"):format(dir, who)
  if exists(f) and not the.force then return f end
  os.execute("mkdir -p " .. dir)
  i = tbl(file)
  h = io.open(f, "w")
  h:write("rep,secs," .. what .. "\n")
  for s = 1, reps do
    t0 = os.clock()
    ok, line = pcall(fun, pair(i, s))        -- a broken csv is a skip
    if not ok then h:close(); os.remove(f); error(line, 0) end
    h:write(("%d,%.4f,%s\n"):format(s, os.clock() - t0, line)) end
  h:close()
  return f end

-- ---- learners --------------------------------------------------------
function bayes(i,j,    k,h,n,a)     -- learn on i, guess every row of j
  i = discrete(i)
  k, a = klass(i), {n = 0, ok = 0, got = {}, want = {}, hit = {}}
  h, n = learn(i, i.rows)
  for _, r in ipairs(discrete(j).rows) do
    tally(a, r[k.at], label(i, h, r, #i.rows, n)) end
  return ("%.1f,%.1f,%.1f,%.3f"):format(abcd(a)) end

function goal(i,    c)              -- one num goal: it.  else: ydist
  c = #i.y == 1 and i.y[1].num and i.y[1]
  if c then return function(r) return tonumber(r[c.at]) or c.mu end end
  return function(r) return ydist(i, r) end end

function rmeans(i,j,    y,lim)      -- the frugal arm: budget labels
  y   = goal(i)
  lim = the.budget - the.check
  return score(train(i, head1(i.rows, lim), y), j, y) end

function rmeansall(i,j,    y)       -- the rich arm: every train row
  y = goal(i)
  return score(train(i, i.rows, y), j, y) end

function head1(t,n,    out)         -- t's first n rows, or all of them
  out = {}
  for j = 1, (#t < n and #t or n) do out[j] = t[j] end
  return out end

function score(m,j,y)               -- where its top 5 sat, and its mae
  return ("%.3f,%.3f"):format(top(m, j, y), mae(m, j, y)) end

function top(m,j,y,    ys,g,best)   -- where the best of the top 5 sits
  ys, g = {}, {}
  for n, r in ipairs(j.rows) do ys[n], g[n] = y(r), r end
  table.sort(ys)
  table.sort(g, function(a,b) return guess(m,a) < guess(m,b) end)
  for n = 1, (#g < the.check and #g or the.check) do
    best = (not best or y(g[n]) < best) and y(g[n]) or best end
  for n, v in ipairs(ys) do
    if v == best then return 100 * (n-1) / math.max(1, #ys-1) end end
  return 100 end

function mae(m,j,y,    e)           -- mean error of the guesses
  e = col()
  for _, r in ipairs(j.rows) do add(e, math.abs(guess(m, r) - y(r))) end
  return e.mu end

-- ---- reading it back -------------------------------------------------
function dirs(kind,    out,h)       -- every cached set of this kind
  out, h = {}, io.popen("find " .. cache .. "/" .. (kind or "") ..
                        " -name '*.csv' 2>/dev/null")
  for line in h:lines() do out[line:match"^(.*)/"] = true end
  h:close()
  return sorted(out) end

function sorted(t,    out)          -- t's keys, in order
  out = {}
  for k in pairs(t) do out[#out + 1] = k end
  table.sort(out)
  return out end

function col1(dir,who,what,    f,out,at,n)
  f = io.open(("%s/%s.csv"):format(dir, who))
  if not f then return nil end
  out = {}
  for line in f:lines() do
    n = 0
    for s in line:gmatch"[^,]+" do
      n = n + 1
      if not at and s == what then at = n end
      if at and n == at then out[#out + 1] = tonumber(s) end end end
  f:close()
  return #out > 0 and out or nil end

function mid(t,    u)               -- the median of t
  u = {}
  for i, v in ipairs(t) do u[i] = v end
  table.sort(u)
  return #u == 0 and 0 or (#u % 2 == 1 and u[(#u+1)/2]
                           or (u[#u/2] + u[#u/2+1]) / 2) end

function versus(kind,a,b,what,big,    w,t,l,ma,mb,x,y)
  w, t, l, ma, mb = 0, 0, 0, {}, {}
  for _, dir in ipairs(dirs(kind)) do
    x, y = col1(dir, a, what), col1(dir, b, what)
    if x and y then
      ma[#ma+1], mb[#mb+1] = mid(x), mid(y)
      if same(x, y)                 then t = t + 1
      elseif (mid(x) > mid(y)) == big then w = w + 1
      else                               l = l + 1 end end end
  print(("%-11s %-10s %7.3f   %-10s %7.3f   %d win %d tie %d loss")
        :format(kind, a, mid(ma), b, mid(mb), w, t, l)) end

-- ---- start up --------------------------------------------------------
function go.splits(    i,tr,te)     -- what seed s gives, for checking
  i = tbl(the.file)
  tr, te = pair(i, tonumber(the.rep) or 1)
  print(#tr.rows, #te.rows, table.concat(tr.rows[1], ",")) end

function go.report()                -- what the cache says, so far
  versus("classify",   "nb",     "lgbm",      "acc",  true)
  versus("regression", "rmeans", "lgbm",      "mae",  false)
  versus("regression", "rmeans", "rmeansall", "rank", false)
  versus("optimize",   "rmeans", "rmeansall", "rank", false) end

function go.nb()
  print(scored(the.file, "nb", bayes, "acc,pd,pf,d2h")) end

function go.rmeans()
  print(scored(the.file, "rmeans", rmeans, "rank,mae")) end

function go.rmeansall()
  print(scored(the.file, "rmeansall", rmeansall, "rank,mae")) end

if (arg and arg[0] or ""):find"min%-eg%.lua$" then
  ok, err = pcall(go[the.go])       -- a broken csv is a skip, not a stop
  if not ok then io.stderr:write(the.file, ": ", tostring(err), "\n") end end
return _ENV
