BEGIN {FS=","}
      { gsub(/[ \t]*/,"") }
NR==1 { heads() }
NR>1  { datums() }
END   { report(); rogues() }

function report(   n,i,j) {
  n = asort(Data,Data,"betters")
  for(i=1;  i<=n;i++) Data[i]["rank"] = i
  for(i=1;  i<=5;i++) {for(j in Y)printf("%5s ",Data[i]["raw"][j]);print i}
  for(i=n-5;i<=n;i++) {for(j in Y)printf("%5s ",Data[i]["raw"][j]);print i}}

function lessp(s) { return s~/-$/          }
function nump(s)  { return s~/^\$/         }
function goalp(s) { return s~/[!+-]$/      }
function min(x,y) { return x<y  ? x : y    }
function max(x,y) { return x>y  ? x : y    }
function abs(x)   { return x>=0 ? x : -1*x }

function heads(  i) { for(i=1;i<=NF;i++) head(i,$i) }
function head(c,x) {
  Name[c] = x
  if (goalp(x)) { Y[c]; W[c] = lessp(x) ? -1 : 1 } else { X[c] }
  if (nump(x))  { Lo[c] = 1E31; Hi[c] = -1E31 }}

function datums(     i) { for(i=1;i<=NF;i++) datum(i,$i,NR-1) }
function datum(c,x,row) {
  Data[row]["raw"][c]=x
 if (x != "?" && c in Lo) { Lo[c] = min(x, Lo[c])
                            Hi[c] = max(x, Hi[c]) }}

function betters(i1,x,i2,y) { return better(x["raw"], y["raw"]) ? -1 : 1 }

function better(r1,r2,    n,s1,s2,a,b,c) {
  n=length(W); s1=s2=0
  for(c in W) {
    a   = norm(c, r1[c])
    b   = norm(c, r2[c])
    s1 -= 2.7183^(W[c]*(a-b)/n)
    s2 -= 2.7183^(W[c]*(b-a)/n) }
  return s1/n < s2/n }

function first(x,  i) { for(i in x) return x[i] } 

function norm(c,x,     lo,hi,tmp) {
  lo=Lo[c]; hi=Hi[c]
  return x=="?" ? x : (abs(lo-hi) < 1E-9 ? 0 : (x-lo)/(hi-lo+1E-32)) }

function oo(x,p,pre, i,txt) {
  txt = pre ? pre : (p ".")
  sortOrder(x)
  for(i in x) { if (isarray(x[i]))   {
                  print(txt i"" ); oo(x[i],"","|  " pre)
              } else print(txt i (x[i]==""?"": ": " x[i])) }}

function sortOrder(x) { PROCINFO["sorted_in"] = \
                typeof(first(x)+0)=="number" ? "@ind_num_asc" : "@ind_str_asc" }

function rogues(    s) {
  for(s in SYMTAB) if (s ~ /^[A-Z][a-z]/) print "#W> Global " s>"/dev/stderr"
  for(s in SYMTAB) if (s ~ /^[_a-z]/    ) print "#W> Rogue: " s>"/dev/stderr"}
