BEGIN {FS=","}
{NR==1 ? names():data()}

function names(   i) {for(i=1;i<+NF;i++) name(i,$i) }
function name(col,s) {
 Name[col]=s
 if (s~/^[A-Z]/) Nump[col] }

function Some(i) { i["n"]=0; i["max"]=256; i["ok"]=0; has(i,"kept") }
function some(i,x,   len,pos) {
  if (x !="?") {
    i["n"]++
    len = length(i["kept"])
    if       (len<256)                   pos = len+1 
    else if  (rand() < i["max"]/i["n"])  pos=1+int(rand()*len);
      if (pos) {
        i["ok"]=0; i["kept"][pos] = x }}}

function has(a,j)  { a[j][0]; delete a[j][0] }

