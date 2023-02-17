BEGIN { FS="," }
NR==1 { for(i=1;i<=NF;i++) nump[i] = $i ~ /^[A-Z]/ }
      { for(i=1;i<=NF;i++) cell(i, coerce($i)) }

function coerce(s,    t) { t=s+0; return s==t ? t : s }
function zap(a) { split("",a,"") }

function read(f,a,    col,row) {
  zap(a)
  while( (getline<f)>0 ) {
    row++
    for(col=1; col<=NF; col++)
      a[row-1,col] = coerce($col) }
  close(f) }

