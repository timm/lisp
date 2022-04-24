BEGIN { FS="," }
      { gsub(/[ \t]*/,"") }
NR==1 { map("head")  }
NR>1  { map("datum") }
END   { report(); rogues() }

function report(   n,i,j) {
  n = asort(Data,Data,"betters")
  for(i in Data) Data[i]["klass"] = i < n^.5
  for(i=1;  i<=n;i++) Data[i]["rank"] = i
  for(i=1;  i<=5;i++) {for(j in Y)printf("%5s ",Data[i]["cells"][j]);print i}
  for(i=n-5;i<=n;i++) {for(j in Y)printf("%5s ",Data[i]["cells"][j]);print i}
  tree()
}
function tree(     rows) { 
  for(i in Data) rows[i]; tree1(rows,bins) 
}

function tree1(rows,    bins) {
  for(c in  X) {
    new(bins,c) 
    c in Lo ?  numBin(c,bins[c],rows) : symBin(c,bins[c],rows) }}

function symBin(c,bins,rows) {
  if (x in bins) 
#----------------------------------------------------------------------------
# Load data
function lessp(s) { return s~/-$/          }
function nump(s)  { return s~/^\$/         }
function goalp(s) { return s~/[!+-]$/      }

function head(c,x,_) {
  Name[c] = x
  goalp(x) ?  Y[c] = (lessp(x) ? -1 : 1 ) :  X[c] 
  if(nump(x)) Hi[c] = -(Lo[c] = 1E31) }

function datum(c,x,row) {
  Data[row-1]["cells"][c]=x
 if (x != "?" && c in Lo) { Lo[c]=min(x, Lo[c]); Hi[c]=max(x, Hi[c]) }}

#-------------------------------------------------------------------------
# Operations on loaded data
function norm(c,x,     lo,hi,tmp) {
  lo=Lo[c]; hi=Hi[c]
  return x=="?" ? x : (abs(lo-hi) < 1E-9 ? 0 : (x-lo)/(hi-lo+1E-32)) }

function betters(i1,x,i2,y) { return better(x["cells"], y["cells"]) ? -1 : 1 }

function better(r1,r2,    n,s1,s2,a,b,c) {
  n=length(Y); s1=s2=0
  for(c in Y) {
    a   = norm(c, r1[c])
    b   = norm(c, r2[c])
    s1 -= 2.7183^(Y[c]*(a-b)/n)
    s2 -= 2.7183^(Y[c]*(b-a)/n) }
  return s1/n < s2/n }

#------------------------------------------------------------------------------
# standard utils

function new(x,k)    { 
  if (k) { x[k][0]; delete a[k][0] } else { k=1+length(x); new(x,k) }
  return k }

function min(x,y)    { return x<y  ? x : y     }
function max(x,y)    { return x>y  ? x : y     }
function abs(x)      { return x>=0 ? x : -1*x  }
function first(x, i) { for(i in x) return x[i] } 
function map(f,   i) { for(i=1;i<=NF;i++) @f(i,$i,NR) }

function oo(x,p,pre, i,txt) {
  txt = pre ? pre : (p ".")
  PROCINFO["sorted_in"] = \
    typeof(first(x)+0)=="number" ? "@ind_num_asc" : "@ind_str_asc" 
  for(i in x) { if (isarray(x[i]))   {
                  print(txt i"" ); oo(x[i],"","|  " pre)
              } else print(txt i (x[i]==""?"": ": " x[i])) }}

function rogues(    s) {
  for(s in SYMTAB) if (s ~ /^[A-Z][a-z]/) print "#W> Global " s>"/dev/stderr"
  for(s in SYMTAB) if (s ~ /^[_a-z]/    ) print "#W> Rogue: " s>"/dev/stderr"}
