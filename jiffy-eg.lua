#!/usr/bin/env lua
-- vim: set ts=2 sw=2 sts=2 et cc=75 :
-- jiffy-eg.lua: the rigs.  A split is a seed, not a file: `rint` is
-- Park-Miller and stays under 2^53, so lua, python and lisp all
-- shuffle a table into the same order.  Only scores reach the disk.
-- Everything here is idempotent: work already cached is never
-- redone (pass force=y to insist).
-- usage: luajit jiffy-eg.lua go=nb     file=f.csv
--        luajit jiffy-eg.lua go=rmeans file=f.csv
--        luajit jiffy-eg.lua go=splits file=f.csv  (what seed s gives)
package.path = (arg[0]:match"^(.*)/" or ".") .. "/?.lua;" .. package.path
local min = require "jiffy"
if setfenv then setfenv(1, min) end          -- jiffy's _ENV is ours too
local _ENV = min
the = cli()                         -- jiffy's options, off our cli

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
function pair(tbl,s,    tr,te)      -- rep s's halves of tbl, by seed
  the.seed = 1234567891 + s
  tr, te = tbl:split()
  return tbl:clone(tr), tbl:clone(te) end

function scored(file,who,fun,what,    dir,f,h,tbl,t0,line,ok) -- once
  dir = home(file)
  f   = ("%s/%s.csv"):format(dir, who)
  if exists(f) and not the.force then return f end
  os.execute("mkdir -p " .. dir)
  tbl = Tbl(file)
  h    = io.open(f, "w")
  h:write("rep,secs," .. what .. "\n")
  for s = 1, reps do
    t0 = os.clock()
    ok, line = pcall(fun, pair(tbl, s))     -- a broken csv is a skip
    if not ok then h:close(); os.remove(f); error(line, 0) end
    h:write(("%d,%.4f,%s\n"):format(s, os.clock() - t0, line)) end
  h:close()
  return f end

-- ---- learners --------------------------------------------------------
function bayes(tr,te,    nb,col,bag,trb) -- learn tr, guess all of te
  if the.sup > 0 then tr:supervise(tr.rows) end  -- klass-aware bin edges
  trb, bag = tr:clone(tr.rows, "bin"), Bag()
  nb, col = Nb(trb, trb.rows), trb.klass
  for _, row in ipairs(tr:clone(te.rows, "bin").rows) do
    bag:tally(row[col.at], nb:guess(row)) end
  return ("%.1f,%.1f,%.1f,%.3f"):format(bag:abcd()) end

function goal(tbl,    col)          -- one num goal: it.  else: ydist
  col = #tbl.y == 1 and getmetatable(tbl.y[1]) == NUM and tbl.y[1]
  if col then
    return function(row) return tonumber(row[col.at]) or col.mu end end
  return function(row) return tbl:ydist(row) end end

function rmeans(tr,te,    y,lim)    -- the frugal arm: budget labels
  y   = goal(tr)
  lim = the.budget - the.check
  return score(Rmeans(tr, head1(tr.rows, lim), y), te, y) end

function rmeansall(tr,te,    y)     -- the rich arm: every train row
  y = goal(tr)
  return score(Rmeans(tr, tr.rows, y), te, y) end

function head1(rows,n,    out)      -- rows' first n, or all of them
  out = {}
  for j = 1, (#rows < n and #rows or n) do out[j] = rows[j] end
  return out end

function score(model,te,y)          -- where its top 5 sat, and its mae
  return ("%.3f,%.6g"):format(top(model, te, y), mae(model, te, y)) end

function top(model,te,y,    ys,rows,g,best) -- where its top 5 landed
  ys, rows, g = {}, {}, {}
  for n, row in ipairs(te.rows) do
    ys[n], rows[n], g[row] = y(row), row, model:guess(row) end
  table.sort(ys)
  table.sort(rows, function(row1,row2) return g[row1] < g[row2] end)
  for n = 1, (#rows < the.check and #rows or the.check) do
    best = (not best or y(rows[n]) < best) and y(rows[n]) or best end
  for n, v in ipairs(ys) do
    if v == best then return 100 * (n-1) / max(1, #ys-1) end end
  return 100 end

function mae(model,te,y,    num)    -- mean error of model's guesses
  num = Num()
  for _, row in ipairs(te.rows) do
    num:add(abs(model:guess(row) - y(row))) end
  return num.mu end

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

function readcol(dir,who,what,    f,out,at,n) -- one result column
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

function mid(xs,    ys)             -- the median of xs
  ys = {}
  for j, v in ipairs(xs) do ys[j] = v end
  table.sort(ys)
  return #ys == 0 and 0 or (#ys % 2 == 1 and ys[(#ys+1)/2]
                            or (ys[#ys/2] + ys[#ys/2+1]) / 2) end

function versus(kind,who1,who2,what,big,    w,t,l,ma,mb,xs,ys)
  w, t, l, ma, mb = 0, 0, 0, {}, {}
  for _, dir in ipairs(dirs(kind)) do
    xs, ys = readcol(dir, who1, what), readcol(dir, who2, what)
    if xs and ys then
      ma[#ma+1], mb[#mb+1] = mid(xs), mid(ys)
      if Same(xs, ys):ok()             then t = t + 1
      elseif (mid(xs) > mid(ys)) == big then w = w + 1
      else                                  l = l + 1 end end end
  print(("%-11s %-10s %7.3f   %-10s %7.3f   %d win %d tie %d loss")
        :format(kind, who1, mid(ma), who2, mid(mb), w, t, l)) end

-- ---- start up --------------------------------------------------------
function eg.splits(    tbl,tr,te)  -- what seed s gives, for checking
  tbl = Tbl(the.file)
  tr, te = pair(tbl, tonumber(the.rep) or 1)
  print(#tr.rows, #te.rows, table.concat(tr.rows[1], ",")) end

function eg.report()                -- what the cache says, so far
  versus("classify",   "nb",     "lgbm",      "acc",  true)
  versus("regression", "rmeans", "lgbm",      "mae",  false)
  versus("regression", "rmeans", "rmeansall", "rank", false)
  versus("optimize",   "rmeans", "rmeansall", "rank", false) end

function few(tbl,n,    rows)         -- tbl's first n rows, cloned
  rows = {}                         -- (pair already shuffled them, so
  for j = 1, math.min(n, #tbl.rows) do rows[j] = tbl.rows[j] end
  return tbl:clone(rows) end        -- these are a random n)

function bayes50(tr,te)             -- bayes, on a budget of labels
  return bayes(few(tr, the.budget), te) end

function aode(tr,te,    md,col,bag,trb) -- as bayes, one parent deeper
  trb, bag = tr:clone(tr.rows, "bin"), Bag()
  md, col  = Aode(trb, trb.rows), trb.klass
  for _, row in ipairs(tr:clone(te.rows, "bin").rows) do
    bag:tally(row[col.at], md:guess(row)) end
  return ("%.1f,%.1f,%.1f,%.3f"):format(bag:abcd()) end

function worth(tr,    nums)         -- is this set worth clustering?
  nums = 0                          -- distance needs numbers, and every
  for _, col in ipairs(tr.x) do     -- leaf needs rows enough to count
    if getmetatable(col) == NUM then nums = nums + 1 end end
  return 2 * nums >= #tr.x
     and #tr.rows / tr.klass:nVals() >= the.enough end

function nbc(tr,te,    trb,bag,col,all,tree,row2)
  if not worth(tr) then return bayes(tr, te) end
  trb, bag = tr:clone(tr.rows, "bin"), Bag() -- cluster in x space, then
  col      = trb.klass                       -- one naive Bayes per leaf
  all      = Nb(trb, trb.rows)               -- plus the global one
  tree     = tr:tree(tr.rows,          -- a leaf has to hold enough
             math.max(the.stop * col:nVals(),  -- rows to see every
                      #tr.rows / the.parts))   -- klass a few times
  for _, row in ipairs(te.rows) do
    local at = tr:leaf(tree, row)            -- the leaf training would
    if not at.nb then                        -- have put this row in
      at.nb = Nb(trb, tr:clone(at.rows, "bin").rows) end
    row2 = tr:clone({row}, "bin").rows[1]
    bag:tally(row2[col.at], at.nb:guess2(row2, all)) end
  return ("%.1f,%.1f,%.1f,%.3f"):format(bag:abcd()) end

function eg.nbc()
  print(scored(the.file, "nbc", nbc, "acc,pd,pf,d2h")) end

function perKlass(tbl,n,col,    seen,rows,k) -- up to n rows of each klass
  seen, rows = {}, {}               -- pair() shuffled, so these are a
  for _, r in ipairs(tbl.rows) do   -- random n, not the first n seen
    k = r[col.at]
    seen[k] = (seen[k] or 0) + 1
    if seen[k] <= n then rows[#rows + 1] = r end end
  return rows end

function spreadPer(tbl,n,col,    at,out,k) -- n per klass, spread over x
  at = {}                           -- the balance of perKlass, and the
  for _, r in ipairs(tbl.rows) do   -- coverage of spread, together
    k = r[col.at]
    at[k] = at[k] or {}
    at[k][#at[k] + 1] = r end
  out = {}
  for _, rows in pairs(at) do
    for _, r in ipairs(#rows <= n and rows or tbl:spread(rows, n)) do
      out[#out + 1] = r end end
  return out end

function knn(tr,te,    bag,tree,col,ref) -- k nearest, in a capped sample
  bag, col = Bag(), tr.klass
  ref  = the.pick == 1 and tr:spread(tr.rows, the.per * col:nVals())
      or the.pick == 2 and spreadPer(tr, the.per, col)
      or perKlass(tr, the.per, col) -- every klass gets the same say, so
  tree = tr:tree(ref, #ref)         -- a rare one is not voted down
  for _, row in ipairs(te.rows) do
    bag:tally(row[col.at], tr:votes(tree, row)) end
  return ("%.1f,%.1f,%.1f,%.3f"):format(bag:abcd()) end

function eg.knn()
  print(scored(the.file, "knn", knn, "acc,pd,pf,d2h")) end

function hold(tr,    n,a,b)         -- tr, split for inner tuning
  n, a, b = floor(.75 * #tr.rows), {}, {}
  for j, row in ipairs(tr.rows) do
    if j <= n then a[#a + 1] = row else b[#b + 1] = row end end
  return tr:clone(a), tr:clone(b) end

function tuned(tr,te,    sub,val,best,at,was,now) -- pick bins,m on tr alone
  sub, val = hold(tr)
  was = {bins = the.bins, m = the.m}
  for _, bins in ipairs{5, 7, 10, 16} do
    for _, m in ipairs{.25, .5, 1, 2} do
      the.bins, the.m = bins, m
      now = tonumber(bayes(sub, val):match"[^,]+,[^,]+,[^,]+,(.+)") -- d2h
      if not best or now < best then best, at = now, {bins = bins, m = m} end end end
  the.bins, the.m = at.bins, at.m
  now = bayes(tr, te)               -- refit on all of tr, with the winner
  the.bins, the.m = was.bins, was.m
  return now end

function eg.nbt()
  print(scored(the.file, "nbt", tuned, "acc,pd,pf,d2h")) end

function eg.nb()
  print(scored(the.file, "nb", bayes, "acc,pd,pf,d2h")) end

function eg.nb50()
  print(scored(the.file, "nb50", bayes50, "acc,pd,pf,d2h")) end

function eg.aode()
  print(scored(the.file, "aode", aode, "acc,pd,pf,d2h")) end

function eg.rmeans()
  print(scored(the.file, "rmeans", rmeans, "rank,mae")) end

function eg.rmeansall()
  print(scored(the.file, "rmeansall", rmeansall, "rank,mae")) end

if (arg and arg[0] or ""):find"jiffy%-eg%.lua$" then
  ok, err = pcall(eg[the.go])       -- a broken csv is a skip, not a stop
  if not ok then
    io.stderr:write(the.file, ": ", tostring(err), "\n") end end
return _ENV
