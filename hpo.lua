#!/usr/bin/env lua
-- vim: set ts=2 sw=2 sts=2 et cc=75 :
-- hpo.lua: sweep one knob against the label budget.
-- usage: luajit hpo.lua file=f.csv [knob=m] [vals=0,1,2,4]
-- out:   file val budget median rank% then each seed's rank%
package.path = (arg[0]:match"^(.*)/" or ".") .. "/?.lua;" .. package.path
local min = require "min"
local the = min.cli()              -- name=value, file off the cli

local KNOB     = the.knob or "m"
local VALS     = {}
for v in (the.vals or "0,1,2,4"):gmatch"[^,]+" do
  VALS[#VALS + 1] = tonumber(v) end
local BUDGETS  = {25, 50, 100, 200, 400}
local REPEATS  = 20

local function median(t)
  table.sort(t)
  local n = #t
  return n % 2 == 1 and t[(n + 1) / 2]
                     or (t[n / 2] + t[n / 2 + 1]) / 2 end

local function top(m,rows,y,    ys,best,g)
  ys = {}
  for i, r in ipairs(rows) do ys[i] = y(r) end
  table.sort(ys)
  g = {}                                     -- rows, best guess first
  for i, r in ipairs(rows) do g[i] = r end
  local key = {}
  for _, r in ipairs(rows) do key[r] = min.guess(m, r) end
  table.sort(g, function(a, b) return key[a] < key[b] end)
  for i = 1, (#g < the.check and #g or the.check) do
    local v = y(g[i])
    if not best or v < best then best = v end end
  for i, v in ipairs(ys) do                  -- where best really sits
    if v == best then
      return 100 * (i - 1) / (#ys > 1 and #ys - 1 or 1) end end
  return 100 end

local file = the.file
local all  = min.tbl(file)
local name = file:match"([^/]+)%.csv$"
local seed = the.seed
for _, budget in ipairs(BUDGETS) do
  local out = {}
  for _, v in ipairs(VALS) do out[v] = {} end
  local enough = true
  for s = 1, REPEATS do
    the.seed = seed + s
    local tr, te = min.split(all)
    if #tr < budget then enough = false break end
    local tr2 = min.clone(all, tr)
    local y   = function(r) return min.ydist(tr2, r) end
    local few = {}
    for j = 1, budget - the.check do few[j] = tr[j] end
    for _, v in ipairs(VALS) do
      the[KNOB] = v
      local model = min.train(all, few, y)
      out[v][#out[v] + 1] = top(model, te, y) end end
  if enough then
    for _, v in ipairs(VALS) do
      print(string.format("%s %s %d %.3f %s", name, v, budget,
                          median(out[v]),
                          table.concat(out[v], ","))) end end end
