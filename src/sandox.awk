BEGIN { a(Data); a(Name); a(Num); a(Goal); a(W);
        a(F); a(Mu); a(Sd); a(M2); a(N); a(Hi); a(Lo);FS=","}
END   {rogues()}
NR==1 {header()}
      {gsub(/[ \t]*/,""); data()}

function a(x)     { split("",x,"") }
function min(x,y) { return x<y ? x : y }
function max(x,y) { return x>y ? x : y }
function o(a,   s,sep,j) {
  s=sep=""; for(j in a) { s= s sep a[j]; sep=", " }
  print s }

function keysort(a,k)         { __sortx = k; return asort(a,a,"__keysort") }
function compare(x,y)         { return x < y ? -1 : (x==y ? 0 : 1) }
function __keysort(i1,x,i2,y) { return compare(x[__sortx]+0, y[__sortx]+0) } 

function rogues(    s) {
  for(s in SYMTAB) if (s ~ /^[A-Z][a-z]/) print "#W> Global " s>"/dev/stderr"
  for(s in SYMTAB) if (s ~ /^[_a-z]/    ) print "#W> Rogue: " s>"/dev/stderr" }

function norm(c, x) {
   return x=="?" ? 0 : (Hi[c] - Lo[c] < 1E-9 ? 0 : (x-Lo[x])/(Hi[x] - Lo[c]))}

function header(  i) {
  for(i=1;i<=NF;i++) {
    W[i] = $i ~ /-$/ ? -1 : 1
    if($i~/^[A-Z]/) { Num[i];  Hi[i]=-1E32; Lo[i]=1E32 }
    if($i~/[!+-]$/) { Goal[i] }
    Name[i] = $i }}

function data(  i,d,inc) {
  for(i=1;i<=NF;i++) 
    if($i != "?") {
      Data[NR-1][i] = $i
      N[i]++
      if (i in Num)  {
        Hi[i] = max($i, Hi[i])
        Lo[i] = min($i, Lo[i])
        d     = $i - Mu[i]; Mu[i] += d/N[i]; M2[i] += d*(x-Mu[i]);
        Sd[i] = (M2[i]<0||N[i] < 2) ? 0 : (M2[i]/(N[i] - 1))^0.5
      } else { F[i][$i]++ }}}

function better(i1,row1,i2,row2,   a,b,n,s1,s2) {
  n = length(Goals)
  for(c in Goals) {
    a   = norm(c, row1[c])
    b   = norm(c, row2[c])
    s1 -= exp(W[c]*(a-b)/n)
    s2 -= exp(W[c]*(b-a)/n)}
  return compare(s1/n, s2/n) }


function classify(i,r,lst,    most,class,like,guess) {
  most = -10^64
  for(class in i.things) {
    guess = guess=="" ? class : guess
    like = bayestheorem( i, lst, i.n, 
                                length(i.things), 
                                i.things[class], class)
    if (like > most) {
      most  = like
      guess = class
  }}
  return guess
}

function bayestheorem(i,lst,nall,nthings,thing,class,    like,prior,c,x,inc,n1) {
    n1 = i.tbl.cols[ i.tbl.my.class ].cnt[class]
    like = prior = (n1  + i.k) / (nall + i.k * nthings)
    like = log(like)
    for(c in thing.my.xs) {
      x = lst[c]
      if (x == SKIPCOL) continue
      if (c in thing.my.nums)
        like += log( NumLike(thing.cols[c], x) )
      else
        like += log( SymLike(thing.cols[c], x, prior, i.m) )
    }
    return like
}
function numlike(c,x,     var) {
  var = Sd[c]^2; return exp(-(x-Mu[c])^2/(2*var+0.0001)) / (3.14159*2*var)^.5 }  


