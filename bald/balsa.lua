-- balsa.yue: the learner, a Yue port of bald (lisp). Data, bins,
-- trees, forest, auc. Same Park-Miller rand, so same shuffles.
-- The library: no printing, no files beyond csv, no experiments.
-- The rigs (5x2 xval, fold replay, stats) live in balsa_eg.yue.
-- Names say shape: tb col row; v = one cell value, b = a bin #,
-- n = a count, s = a string; fn mk = functions; plurals are lists;
-- sym1, sym2 where only Syms will do; y reads a row's label.
-- (c) 2026 Tim Menzies <timm@ieee.org>, MIT license
local floor, sqrt, max, min, log = math.floor, math.sqrt, math.max, math.min, math.log
local LOG2 = log(2)

local k = {
  seed = 1234567891,
  eps = 0.35,
  bins = 16,
  trees = 30,
  cap = 200,
  leaf = 2,
  maxd = 30,
  rf = true
}

------------------------------------------------------------- random
local seed = k.seed
local srand
srand = function(n)
  seed = n
end
local rand
rand = function(n)
  if n == nil then
    n = 1
  end
  seed = (16807 * seed) % 2147483647
  return n * seed / 2147483647
end

local shuffle
shuffle = function(rows)
  local u
  do
    local _accum_0 = { }
    local _len_0 = 1
    for _index_0 = 1, #rows do
      local row = rows[_index_0]
      _accum_0[_len_0] = row
      _len_0 = _len_0 + 1
    end
    u = _accum_0
  end
  for i = #u, 2, -1 do
    local j = 1 + floor(rand(i))
    u[i], u[j] = u[j], u[i]
  end
  return u
end

local pick
pick = function(n, m)
  if not (m and m < n) then
    local _accum_0 = { }
    local _len_0 = 1
    for i = 0, n - 1 do
      _accum_0[_len_0] = i
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end
  local got, out = { }, { }
  while #out < m do
    local i = floor(rand(n))
    if not got[i] then
      got[i] = true
    end
    out[#out + 1] = i
  end
  table.sort(out)
  return out
end

---------------------------------------------------------------- csv
local thing
thing = function(cell)
  cell = cell:match("^%s*(.-)%s*$")
  return tonumber(cell) or cell
end

local csv
csv = function(file)
  file = file:gsub("^~", os.getenv("HOME"))
  local rows = { }
  for line in io.lines(file) do
    local txt = line:gsub("\r", ""):match("^%s*(.-)%s*$")
    if txt ~= "" then
      local _accum_0 = { }
      local _len_0 = 1
      for cell in (tostring(txt) .. ","):gmatch("(.-),") do
        _accum_0[_len_0] = thing(cell)
        _len_0 = _len_0 + 1
      end
      rows[#rows + 1] = _accum_0
    end
  end
  return rows
end

--------------------------------------------------------------- data
-- Header: Uppercase = Num, else Sym. Last char: ! class, X skip.
-- "?" cells unknown. A row IS its csv cells (numeric indexes),
-- plus one named field: cooked, its bin # per x column.
local Col
Col = function(txt, at)
  if txt == nil then
    txt = "N"
  end
  if at == nil then
    at = 0
  end
  if txt:match("^%u") then
    return {
      at = at,
      txt = txt,
      nm = true,
      n = 0,
      mu = 0,
      m2 = 0
    }
  else
    return {
      at = at,
      txt = txt,
      n = 0,
      has = { }
    }
  end
end

local add
add = function(col, v, inc)
  if inc == nil then
    inc = 1
  end
  if v == "?" then
    return
  end
  col.n = col.n + inc
  if col.nm then
    local d = v - col.mu
    col.mu = col.mu + (inc * d / col.n)
    col.m2 = max(0, col.m2 + inc * d * (v - col.mu))
  else
    col.has[v] = (col.has[v] or 0) + inc
  end
end

local adds
adds = function(vs, col)
  if col == nil then
    col = Col()
  end
  for _index_0 = 1, #vs do
    local v = vs[_index_0]
    add(col, v)
  end
  return col
end

local div
div = function(col)
  if col.nm then
    if col.n < 2 then
      return 0
    else
      return sqrt(col.m2 / (col.n - 1))
    end
  end
  local e = 0
  for _, m in pairs(col.has) do
    if m > 0 then
      e = e - (m / col.n * log(m / col.n))
    end
  end
  return e / LOG2
end

local fold
fold = function(sym1, sym2, inc)
  if inc == nil then
    inc = 1
  end
  for v, m in pairs(sym2.has) do
    sym1.has[v] = (sym1.has[v] or 0) + inc * m
  end
  sym1.n = sym1.n + (inc * sym2.n)
  return sym1
end

local Cols
Cols = function(names)
  local all
  do
    local _accum_0 = { }
    local _len_0 = 1
    for at, txt in ipairs(names) do
      _accum_0[_len_0] = Col(txt, at)
      _len_0 = _len_0 + 1
    end
    all = _accum_0
  end
  local x, y, klass = { }, { }, nil
  for i = #all, 1, -1 do
    local col = all[i]
    local e = col.txt:sub(-1)
    if col.txt:match("X$") then
      goto _continue_0
    end
    if e == "+" or e == "-" or e == "!" then
      y[#y + 1] = col
      if e == "!" then
        klass = col
      end
    else
      x[#x + 1] = col
    end
    ::_continue_0::
  end
  return {
    names = names,
    all = all,
    x = x,
    y = y,
    klass = klass
  }
end

------------------- bins: discretize once; trees then cut on bin #s
local bisect
bisect = function(vs, v, le)
  local lo, hi = 1, #vs + 1
  while lo < hi do
    local mid = floor((lo + hi) / 2)
    local below
    if le then
      below = vs[mid] <= v
    else
      below = vs[mid] < v
    end
    if below then
      lo = mid + 1
    else
      hi = mid
    end
  end
  return lo - 1
end

local choices
choices = function(col, rows, n)
  if col.nm then
    local vs
    do
      local _accum_0 = { }
      local _len_0 = 1
      for _index_0 = 1, #rows do
        local row = rows[_index_0]
        if row[col.at] ~= "?" then
          _accum_0[_len_0] = row[col.at]
          _len_0 = _len_0 + 1
        end
      end
      vs = _accum_0
    end
    table.sort(vs)
    local out, b4 = { }, nil
    for j = 0, (#vs > 0 and n - 1 or -1) do
      local v = vs[1 + floor(j * #vs / n)]
      if v ~= b4 then
        out[#out + 1] = v
      end
      b4 = v
    end
    return out
  else
    local vs
    do
      local _accum_0 = { }
      local _len_0 = 1
      for v, m in pairs(col.has) do
        _accum_0[_len_0] = {
          v,
          m
        }
        _len_0 = _len_0 + 1
      end
      vs = _accum_0
    end
    table.sort(vs, function(a, b)
      return a[2] > b[2] or (a[2] == b[2] and tostring(a[1]) < tostring(b[1]))
    end)
    local _accum_0 = { }
    local _len_0 = 1
    for i = 1, min(n - 1, #vs) do
      _accum_0[_len_0] = vs[i][1]
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end
end

local cook
cook = function(col, v)
  if v == "?" then
    return -1
  end
  if col.nm then
    return max(0, bisect(col.bins, v, true) - 1)
  end
  for i, b in ipairs(col.bins) do
    if b == v then
      return i - 1
    end
  end
  return #col.bins
end

local discretize
discretize = function(tb)
  local _list_0 = tb.cols.x
  for _index_0 = 1, #_list_0 do
    local col = _list_0[_index_0]
    col.bins = choices(col, tb.rows, k.bins)
  end
  local _list_1 = tb.rows
  for _index_0 = 1, #_list_1 do
    local row = _list_1[_index_0]
    row.cooked = { }
    local _list_2 = tb.cols.x
    for _index_1 = 1, #_list_2 do
      local col = _list_2[_index_1]
      row.cooked[col.at] = cook(col, row[col.at])
    end
  end
end

local Tbl
Tbl = function(rows)
  local tb = {
    cols = Cols(rows[1]),
    rows = { }
  }
  for i = #rows, 2, -1 do
    local row = rows[i]
    do
      local _obj_0 = tb.rows
      _obj_0[#_obj_0 + 1] = row
    end
    local _list_0 = tb.cols.all
    for _index_0 = 1, #_list_0 do
      local col = _list_0[_index_0]
      add(col, row[col.at])
    end
  end
  discretize(tb)
  return tb
end

--------------------------------------------------------------- tree
local argmost
argmost = function(better)
  local out, b4 = nil, nil
  return function(x, n)
    if x and (b4 == nil or better(n, b4)) then
      out, b4 = x, n
    end
    return out, b4
  end
end

local xpect
xpect = function(a, b)
  return (a.n * div(a) + b.n * div(b)) / (a.n + b.n + 1e-32)
end

local cuts
cuts = function(col, rows, ys, mk, leaf, best)
  local nb = #col.bins
  local bins
  do
    local _accum_0 = { }
    local _len_0 = 1
    for _ = 0, nb do
      _accum_0[_len_0] = mk()
      _len_0 = _len_0 + 1
    end
    bins = _accum_0
  end
  for i, row in ipairs(rows) do
    local b = row.cooked[col.at]
    if b >= 0 then
      add(bins[b + 1], ys[i])
    end
  end
  local yes = mk()
  local no = mk()
  for _index_0 = 1, #bins do
    local bin = bins[_index_0]
    fold(no, bin)
  end
  for b = 0, nb - 1 do
    local bin = bins[b + 1]
    fold(no, bin, -1)
    if not col.has then
      fold(yes, bin)
    end
    local lhs = col.has and bin or yes
    if leaf <= min(lhs.n, no.n) then
      best({
        col,
        b
      }, xpect(lhs, no))
    end
    if col.has then
      fold(no, bin)
    end
  end
end

local best_cut
best_cut = function(xcols, rows, ys, mk, m, leaf)
  local best = argmost(function(a, b)
    return a < b
  end)
  local _list_0 = pick(#xcols, m)
  for _index_0 = 1, #_list_0 do
    local i = _list_0[_index_0]
    cuts(xcols[i + 1], rows, ys, mk, leaf, best)
  end
  return best()
end

local _anon_func_0 = function(b, col, w)
  if col.has then
    return w == b
  else
    return w <= b
  end
end
local yes_
yes_ = function(col, b, row)
  local w = row.cooked[col.at]
  return w < 0 or (_anon_func_0(b, col, w))
end

local tree
tree = function(rows, y, xcols, mk, m, leaf, maxd, lvl)
  if lvl == nil then
    lvl = 0
  end
  local ys
  do
    local _accum_0 = { }
    local _len_0 = 1
    for _index_0 = 1, #rows do
      local row = rows[_index_0]
      _accum_0[_len_0] = y(row)
      _len_0 = _len_0 + 1
    end
    ys = _accum_0
  end
  local cut = lvl < maxd and best_cut(xcols, rows, ys, mk, m, leaf)
  local node = {
    ys = adds(ys, mk()),
    n = #rows
  }
  if cut then
    local col, b = cut[1], cut[2]
    node.v = b
    node.ati, node.sym = col.at, col.has ~= nil
    local yes
    do
      local _accum_0 = { }
      local _len_0 = 1
      for _index_0 = 1, #rows do
        local row = rows[_index_0]
        if yes_(col, b, row) then
          _accum_0[_len_0] = row
          _len_0 = _len_0 + 1
        end
      end
      yes = _accum_0
    end
    local no
    do
      local _accum_0 = { }
      local _len_0 = 1
      for _index_0 = 1, #rows do
        local row = rows[_index_0]
        if not yes_(col, b, row) then
          _accum_0[_len_0] = row
          _len_0 = _len_0 + 1
        end
      end
      no = _accum_0
    end
    node.yes = tree(yes, y, xcols, mk, m, leaf, maxd, lvl + 1)
    node.no = tree(no, y, xcols, mk, m, leaf, maxd, lvl + 1)
  end
  return node
end

local _anon_func_1 = function(node, w)
  if node.sym then
    return w == node.v
  else
    return w <= node.v
  end
end
local leaf_of
leaf_of = function(node, row)
  while node.ati do
    local w = row.cooked[node.ati]
    local ok = w < 0 or (_anon_func_1(node, w))
    node = ok and node.yes or node.no
  end
  return node
end

------------------------------------------------------------- forest
local capped
capped = function(rows, y, n)
  local h, out = { }, { }
  local _list_0 = shuffle(rows)
  for _index_0 = 1, #_list_0 do
    local row = _list_0[_index_0]
    local v = y(row) or "?"
    h[v] = (h[v] or 0) + 1
    if h[v] <= n then
      out[#out + 1] = row
    end
  end
  return out
end

local forest
forest = function(tb, rows, y)
  local xcols = tb.cols.x
  local m = k.rf and math.ceil(sqrt(#xcols))
  local mk
  mk = function()
    return Col("y")
  end
  if k.cap then
    rows = capped(rows, y, k.cap)
  end
  local half
  half = function()
    local u = shuffle(rows)
    local _accum_0 = { }
    local _len_0 = 1
    for i = 1, floor(#rows / 2) do
      _accum_0[_len_0] = u[i]
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end
  local out = { }
  for _ = 1, k.trees do
    out[#out + 1] = tree(k.trees == 1 and rows or half(), y, xcols, mk, m, k.leaf, k.maxd)
  end
  return out
end

local vote
vote = function(trees, row)
  local h, w = { }, #trees
  for _index_0 = 1, #trees do
    local node = trees[_index_0]
    local ys = leaf_of(node, row).ys
    for v, m in pairs(ys.has) do
      h[v] = (h[v] or 0) + m / ys.n / w
    end
  end
  return h
end

local votes
votes = function(trees, rows, y)
  local _accum_0 = { }
  local _len_0 = 1
  for _index_0 = 1, #rows do
    local row = rows[_index_0]
    _accum_0[_len_0] = {
      y(row),
      vote(trees, row)
    }
    _len_0 = _len_0 + 1
  end
  return _accum_0
end

---------------------------------------------------------------- auc
local auc
auc = function(pos, neg)
  if #pos == 0 or #neg == 0 then
    return nil
  end
  table.sort(neg)
  local s = 0
  for _index_0 = 1, #pos do
    local p = pos[_index_0]
    s = s + ((bisect(neg, p) + bisect(neg, p, true)) / 2)
  end
  return s / (#pos * #neg)
end

local aucs
aucs = function(wps)
  local seen, n, sum = { }, 0, 0
  for _index_0 = 1, #wps do
    local wp = wps[_index_0]
    local c = wp[1]
    if not seen[c] then
      seen[c] = true
      local pos
      do
        local _accum_0 = { }
        local _len_0 = 1
        for _index_1 = 1, #wps do
          local w = wps[_index_1]
          if w[1] == c then
            _accum_0[_len_0] = w[2][c] or 0
            _len_0 = _len_0 + 1
          end
        end
        pos = _accum_0
      end
      local neg
      do
        local _accum_0 = { }
        local _len_0 = 1
        for _index_1 = 1, #wps do
          local w = wps[_index_1]
          if w[1] ~= c then
            _accum_0[_len_0] = w[2][c] or 0
            _len_0 = _len_0 + 1
          end
        end
        neg = _accum_0
      end
      local a = auc(pos, neg)
      if a then
        n = n + (#pos)
        sum = sum + (#pos * a)
      end
    end
  end
  return 100 * sum / max(n, 1)
end

-- the module: what balsa_eg.yue (and any other rig) may touch
return {
  k = k,
  srand = srand,
  rand = rand,
  shuffle = shuffle,
  bisect = bisect,
  csv = csv,
  Tbl = Tbl,
  adds = adds,
  div = div,
  tree = tree,
  leaf_of = leaf_of,
  forest = forest,
  votes = votes,
  auc = auc,
  aucs = aucs
}
