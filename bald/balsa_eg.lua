-- balsa_eg.yue: the dialog. Experiments, stats and printing;
-- balsa.yue is the model it talks to.
-- usage: yue -c -s *.yue, then
--   luajit balsa_eg.lua [data...]        5x2 xval, 30 trees, 16 bins
--   luajit balsa_eg.lua --replay F.dat   rerun F's recorded folds
-- (c) 2026 Tim Menzies <timm@ieee.org>, MIT license
local floor, sqrt, max = math.floor, math.sqrt, math.max
local b = require("balsa")
local k, srand, shuffle, bisect, csv, Tbl, adds, div, forest, votes, aucs = b.k, b.srand, b.shuffle, b.bisect, b.csv, b.Tbl, b.adds, b.div, b.forest, b.votes, b.aucs
local fmt = string.format
local kl
kl = function(row)
  return row.kl
end

-------------------------------------------------------------- stats
local median
median = function(xs)
  local ys
  do
    local _accum_0 = { }
    local _len_0 = 1
    for _index_0 = 1, #xs do
      local x = xs[_index_0]
      _accum_0[_len_0] = x
      _len_0 = _len_0 + 1
    end
    ys = _accum_0
  end
  table.sort(ys)
  return ys[1 + floor(#ys / 2)]
end

local cohen
cohen = function(xs, ys)
  local a = adds(xs, {
    nm = true,
    n = 0,
    mu = 0,
    m2 = 0
  })
  local c = adds(ys, {
    nm = true,
    n = 0,
    mu = 0,
    m2 = 0
  })
  local sp = sqrt(((a.n - 1) * div(a) ^ 2 + (c.n - 1) * div(c) ^ 2) / max(1, a.n + c.n - 2))
  return math.abs(a.mu - c.mu) / (sp + 1e-32)
end

local cliffs
cliffs = function(xs, ys)
  local d = 0
  for _index_0 = 1, #xs do
    local x = xs[_index_0]
    d = d + (bisect(ys, x) - (#ys - bisect(ys, x, true)))
  end
  return math.abs(d) / (#xs * #ys)
end

local ks
ks = function(xs, ys)
  local n, m, d = #xs, #ys, 0
  local _list_0 = {
    xs,
    ys
  }
  for _index_0 = 1, #_list_0 do
    local v = _list_0[_index_0]
    for _index_1 = 1, #v do
      local x = v[_index_1]
      d = max(d, math.abs(bisect(xs, x, true) / n - bisect(ys, x, true) / m))
    end
  end
  return d / sqrt((n + m) / (n * m))
end

local same
same = function(xs, ys, eps)
  if eps == nil then
    eps = k.eps
  end
  do
    local _accum_0 = { }
    local _len_0 = 1
    for _index_0 = 1, #xs do
      local x = xs[_index_0]
      _accum_0[_len_0] = x
      _len_0 = _len_0 + 1
    end
    xs = _accum_0
  end
  do
    local _accum_0 = { }
    local _len_0 = 1
    for _index_0 = 1, #ys do
      local y = ys[_index_0]
      _accum_0[_len_0] = y
      _len_0 = _len_0 + 1
    end
    ys = _accum_0
  end
  table.sort(xs)
  table.sort(ys)
  return cohen(xs, ys) <= eps and cliffs(xs, ys) <= 0.195 and ks(xs, ys) <= 1.36
end

local rank
rank = function(d)
  table.sort(d, function(x, y)
    return median(x[2]) < median(y[2])
  end)
  local r, pool, out = 1, nil, { }
  for _index_0 = 1, #d do
    local kv = d[_index_0]
    if pool and not same(pool, kv[2]) then
      r, pool = r + 1, nil
    end
    pool = pool or { }
    local _list_0 = kv[2]
    for _index_1 = 1, #_list_0 do
      local x = _list_0[_index_1]
      pool[#pool + 1] = x
    end
    out[kv[1]] = r
  end
  return out
end

--------------------------------------------------------------- rigs
local xval
xval = function(rows, fn, m, n)
  if m == nil then
    m = 5
  end
  if n == nil then
    n = 2
  end
  for _ = 1, m do
    local u = shuffle(rows)
    for kf = 0, n - 1 do
      local train, test = { }, { }
      for i, row in ipairs(u) do
        if (i - 1) % n == kf then
          test[#test + 1] = row
        else
          train[#train + 1] = row
        end
      end
      fn(train, test)
    end
  end
end

local ens
ens = function(sets)
  print("#n30 ")
  for _index_0 = 1, #sets do
    local f = sets[_index_0]
    local tb = Tbl(csv("~/gits/moot/classify/" .. tostring(f) .. ".csv"))
    xval(tb.rows, function(train, test)
      local t0 = os.clock()
      local a = aucs(votes(forest(tb, train, kl), test, kl, tb.kn), tb.kn)
      io.write(fmt("%s|%.3f |%.3f |\n", f, a, os.clock() - t0))
      return io.flush()
    end)
  end
end

local replay
replay = function(dat)
  print("#1:q16 n30 ")
  local tb, v, name = nil, nil, nil
  for line in io.lines(dat) do
    if line:match("^#") then
      goto _continue_0
    end
    local f = line:match("^([^|]*)|")
    local ids = line:match("|([^|]*)$")
    if f ~= name then
      name = f
      tb = Tbl(csv("~/gits/moot/classify/" .. tostring(f) .. ".csv"))
      v = { }
      for i, row in ipairs(tb.rows) do
        v[#tb.rows - i] = row
      end
    end
    local test, inh = { }, { }
    for s in ids:gmatch("%d+") do
      local row = v[tonumber(s)]
      test[#test + 1] = row
      inh[row] = true
    end
    local train
    do
      local _accum_0 = { }
      local _len_0 = 1
      local _list_0 = tb.rows
      for _index_0 = 1, #_list_0 do
        local row = _list_0[_index_0]
        if not inh[row] then
          _accum_0[_len_0] = row
          _len_0 = _len_0 + 1
        end
      end
      train = _accum_0
    end
    local aucs_, secs = { }, { }
    local _list_0 = {
      {
        1,
        false
      },
      {
        30,
        true
      }
    }
    for _index_0 = 1, #_list_0 do
      local arm = _list_0[_index_0]
      k.trees, k.rf = arm[1], arm[2]
      local t0 = os.clock()
      aucs_[#aucs_ + 1] = aucs(votes(forest(tb, train, kl), test, kl, tb.kn), tb.kn)
      secs[#secs + 1] = os.clock() - t0
    end
    io.write(fmt("%s|%.3f %.3f |%.3f %.3f |%s\n", f, aucs_[1], aucs_[2], secs[1], secs[2], ids))
    io.flush()
    ::_continue_0::
  end
end

local ENS = {
  "balance.scale",
  "blood-transfusion-service-center",
  "breast.w",
  "diabetes",
  "ecoli",
  "glass",
  "ionosphere",
  "sonar",
  "vehicle",
  "wdbc",
  "vote",
  "soybean",
  "tic-tac-toe",
  "german",
  "car",
  "solar.flare2",
  "primary.tumor",
  "breast.cancer",
  "spect.test",
  "audiology",
  "credit.a",
  "eucalyptus",
  "ilpd",
  "heart.c",
  "hepatitis",
  "colic",
  "autos",
  "cylinder.bands",
  "credit.g",
  "cmc",
  "hypothyroid",
  "sick",
  "Internet-Advertisements",
  "churn",
  "COMPAS53",
  "nomao",
  "bank-marketing",
  "electricity",
  "adult"
}

srand(k.seed)
if arg[1] == "--replay" then
  return replay(arg[2])
else
  return ens(#arg > 0 and arg or ENS)
end
