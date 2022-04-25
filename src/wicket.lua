local coerce,csv,fmt,goalp,lessp,nump,oo,o,sort,the
the={file="../data/aauto.cs"}

fmt=string.format
function ignorep(s) return s:find":$"     end
function lessp(s) return s:find"-$"     end
function goalp(s) return s:find"[!-+]$" end
function nump(s)  return s:find"^[A-Z]" end
function sort(t,f) table.sort(t,f) return t end

function coerce(x)
  x = x:match"^%s*(.-)%s*$"
  if x=="true" then return true elseif x=="false" then return false end
return math.tointeger(x) or tonumber(x) or x end

function csv(src)
  src = io.input(src)
  return function(line, row) 
    line=io.read()
    if not line then io.close(src) else
      row={}; for x in line:gmatch("([^,]+)") do row[1+#row]=coerce(x) end
      return row end end end 

function oo(t) print(o(t)) end
function o(t,    u,one,sorted)
  sorted = #t>0 -- true when array's indexes are 1,2...#t
  one= function(k,v) return sorted and tostring(v) or fmt(":%s %s",k,v) end
  u={}; for k,v in pairs(t) do u[1+#u] = one(k,v) end
  return (t.is or "").."{"..table.concat(sorted and u or sort(u)," ").."}" end

function rogues(   ok)
  for _,k in pairs{ "_G", "_VERSION", "arg", "assert", "collectgarbage",
  "coroutine", "debug", "dofile", "error", "getmetatable", "io", "ipairs",
  "load", "loadfile", "math", "next", "os", "package", "pairs", "pcall",
  "print", "rawequal", "rawget", "rawlen", "rawset", "require", "select",
  "setmetatable", "string", "table", "tonumber", "tostring", "type", "utf8",
  "warn", "xpcall"} do ok[k]=true end
  for k,v in pairs(_ENV) do if not ok[k] then print("?",k, type(v)) end end end

function obj(name,    t,new,str)
  function new(kl,...) local x=setmetatable({},kl); kl.new(x,...); return x end 
  t = {__tostring=o, is=name or ""}; t.__index=t
  return setmetatable(t, {__call=new}) end

----------------------------------------------------------------------
Num=obj"Num"
function Num:new(pos,s) 
  self.pos, self.txt, self.lo, self.hi = pos or 0,s or "",1E32, -1E32
  self.w = lessp(self.txt) and -1 or 1 end

function Num:add(x) 
  if x=="?" then return x end
  self.lo = math.min(x,self.lo)
  self.hi = math.max(x,self.hi) end

function Num:norm(x,   lo,hi)
  lo,hi= self.lo, self.hi
  return x=="?" and x or hi-lo < 1E-9 and 0 or (x - lo)/(hi - lo) end 

function Num:dist(x,y)
  if     x=="?" and y=="?" then return 1 end
  if     x=="?"            then y = self:norm(y); x = y<.5 and 1 or 0 
  elseif y=="?"            then x = self:norm(x); y = x<.5 and 1 or 0
  else x,y = self:norm(x), self:norm(y) end
  return math.abs(x - y) end

----------------------------------------------------------------------
Sym=obj"Sym"
function Sym:new(pos,s) self.pos, self.txt= pos or 0,s or "" end
function Sym:add(x)     return x end
function Sym:dist(x,y) return x=="?" and y=="?" and 1 or x==y and 0 or 1 end
function Sym:mid(rows,  u,out,hi)
  for _,row in pairs(rows) do
    x= row.cells[self.pos]
    if x ~= "?" then u[x]= 1+(u[x] or 0) end end
  hi=-1; for k,v in pairs(u) do if v>hi then hi,out=v,k end end 
  return out end 
----------------------------------------------------------------------
Cols=obj"Cols"
function Cols:new(names,       it,num,sym,col)
  self.names=names
  self.x, self.y, self.all, self.nums={},{},{},{}
  for pos,name in pairs(name) do 
    col = (nump(name) and Num or Sym)(pos,name)
    self.all[pos]=col
    if not ignorep(name) then
      if nump(name) then it.nums[pos] = col end
      (goalp(name) and it.y or it.x)[pos] = col end end end

----------------------------------------------------------------------
Row=obj"Row"
function Row:new(t) self.cells, self.evaluated = t, false end

 ----------------------------------------------------------------------
Egs=obj"Egs"
function Egs:new() self.rows,self.cols={},nil end

function Egs:add(row)
  row = row.cells and row or Row(row)
  if   self.cols
  then push(self.rows, row)
       for _,col in pairs(row.cells) do col:add(row[col.pos]) end 
  else self.cols = Cols(row.cells) end end
      
function Egs:load(file)
  for n,row in csv(the.file) do self:add(row) end end


rogues()
