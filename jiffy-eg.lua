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
function bayes(tr,te,    nb,col,bag) -- learn tr, guess all of te
  tr, bag = tr:clone(tr.rows, "bin"), Bag()
  nb, col = Nb(tr, tr.rows), tr.klass
  for _, row in ipairs(te:clone(te.rows, "bin").rows) do
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

function arm(kind,bins,most,how)    -- rm: rmeans. rmx: bins set from
  return function(tr,te,              -- every train row's x. fm: fastmap
                  b4,y,lab,xs,model,glob,fm,out) -- leaves, rmeans in
    b4, the.bins = the.bins, bins     -- each. fmp: fm, leaf bins shrunk
    y   = goal(tr)                    -- to the global guess. most: x
    lab = head1(tr.rows, the.budget - the.check) -- rows for fastmap
    xs  = most and some(tr.rows, lab, most) or tr.rows -- (all if nil).
    if     kind == "rm"  then model = Rmeans(tr, lab, y) -- how: prune
    elseif kind == "rmx" then model = Rmeans(tr, lab, y, tr.rows) -- cols
    else
      glob  = Rmeans(tr, lab, y, xs)
      if how then glob:prune(how) end
      fm    = Fm(tr, xs, lab, y, how)
      model = {guess = function(_, row)
                 return fm:guess(row, kind == "fmp" and glob:guess(row)
                                      or nil) end} end
    out = score(model, te, y) .. "," .. (fm and fm:leaves() or 1) ..
          "," .. (fm and ("%.3f"):format(
                    fm:cols() / fm:leaves() / #tr.x) or 1)
    the.bins = b4
    return out end end

function some(rows,lab,n,    seen,out,pool) -- lab, then others, to n
  seen, out, pool = {}, {}, {}
  for _, row in ipairs(lab) do seen[row], out[#out+1] = true, row end
  for _, row in ipairs(rows) do
    if not seen[row] then pool[#pool+1] = row end end
  shuffle(pool)
  for j = 1, math.min(#pool, n - #out) do out[#out+1] = pool[j] end
  return out end

arms = {"rm", "rmx", "fm", "fmp"}

function head1(rows,n,    out)      -- rows' first n, or all of them
  out = {}
  for j = 1, (#rows < n and #rows or n) do out[j] = rows[j] end
  return out end

function score(model,te,y)          -- where its top 5 sat, and its mae
  return ("%.3f,%.3f"):format(top(model, te, y), mae(model, te, y)) end

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

function eg.arms()                  -- every arm, at 3 and 7 bins
  for _, kind in ipairs(arms) do
    for _, bins in ipairs{3, 7} do
      scored(the.file, kind .. bins, arm(kind, bins),
             "rank,mae,leaves,cols") end end end

prunes = {fmps3  = {2000},           -- fmp3, fastmap on 2000 rows
          fmpsm3 = {2000, "max"},    -- and only cols >= 10% of best
          fmpsc3 = {2000, "chance"}} -- and only cols beating noise

function eg.prunes()                -- fmp3, sampled, then pruned
  for who, o in pairs(prunes) do
    scored(the.file, who, arm("fmp", 3, o[1], o[2]),
           "rank,mae,leaves,cols") end end

function eg.prunesreport()          -- each against fmp3, then rm7
  for _, kind in ipairs{"regression", "optimize"} do
    for _, base in ipairs{"fmp3", "rm7"} do
      for _, who in ipairs{"fmps3", "fmpsm3", "fmpsc3"} do
        versus(kind, who, base, kind == "regression" and "mae"
                                  or "rank", false) end end
    print() end end

function eg.armsreport(    b)       -- each arm against rm7 and rm3
  for _, kind in ipairs{"regression", "optimize"} do
    for _, base in ipairs{"rm7", "rm3"} do
      for _, kind1 in ipairs(arms) do
        for _, bins in ipairs{3, 7} do
          b = kind1 .. bins
          if b ~= base then
            versus(kind, b, base, kind == "regression" and "mae"
                                    or "rank", false) end end end
      print() end end end

function eg.nb()
  print(scored(the.file, "nb", bayes, "acc,pd,pf,d2h")) end

function eg.rmeans()
  print(scored(the.file, "rmeans", rmeans, "rank,mae")) end

function eg.rmeansall()
  print(scored(the.file, "rmeansall", rmeansall, "rank,mae")) end

if (arg and arg[0] or ""):find"jiffy%-eg%.lua$" then
  ok, err = pcall(eg[the.go])       -- a broken csv is a skip, not a stop
  if not ok then
    io.stderr:write(the.file, ": ", tostring(err), "\n") end end
return _ENV
