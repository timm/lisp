cat <<EOF|gawk '{print gensub(/\.([^0-9\\*\\$\\+])([a-zA-Z0-9_]*)/, "[\"\\1\\2\"]","g",$0);}'>>/tmp/$$.awk
BEGIN {FS=","}
{NR==1 ? names():data()}

function names(   i) {for(i=1;i<+NF;i++) name(i,$i) }
function name(col,s) {
 Name[col]=s
 if (s~/^[A-Z]/) {Nump[col]; Hi[col]=-1E32; Lo[col]=1E32 }
 if (s~/^[A-Z]/) {Nump[col]; Hi[col]=-1E32; Lo[col]=1E32 }

function Cols(i)  {have(i,"names,all,x,y,all") }
function cols(i,name) {
  at   = more(a,"all")
  what = name ~/^[A-Z]/ ? "Num" : "Sym"
  @what(i.all[at], at, name)
  push( i[name ~/[!+-]$/ ? "y" : "x"], at) }
   
function Sym(i,at,s)  {
  i.at=at?at:0; i.txt=s?s:""; i.n=0; has(i,"kept"); Some(i.kept) }
function sym(i,x) {

function Num(i,at,s)  {
  i.at=at?at:0; i.txt=s?s:""; i.n=0;i.hi=-1E32; i.lo =1E32; Some(i.kept) }
function num(i,x) {if (x !="?") { if (x>i.hi) i.hi=x; if (x<i.lo) i.lo=x}}

function Some(i) { i,n=0; imax=256; iok=0; has(i,"kept") }
function some(i,x,   len,pos) {
  if (x !="?") {
    i.n++
    len = length(i.kept)
    if       (len<256)                   {pos = len+1}
    else if  (rand() < i.max/i.n)  {pos =1+int(rand()*len)};
    if (pos) {
      i.ok=0; i.kept[pos] = x }}}

function have(a,ks,   b,k) { split(ks,b,","); for(k in b) has(a,b[k]) }
function more(a,k,    len) { len=length(a[k]+1; has(a[k], len); return len}
function has(a,k)          { a[k][0]; delete a[k][0] }

function push(a,x,    len) { len=length(a)+1; a[len]=x; return x }
EOF


if [ -t 1 ]
then         gawk -f /tmp/$$.awk $*
else cat - | gawk -f /tmp/$$.awk $*
fi

