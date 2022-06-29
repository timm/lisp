clssify() {
  gawk '/^;/       {pre="Q "}
        /^\(/      {pre="C "}
        /^[ \t]*$/ {print "S "; next}
        NR>1       {print pre $0} '
}

render() {
 gawk '{ R[++N]["?"]=$1; $1=""; R[N]["="]=$0 }
       END {for(m=1;m<=N;m++) }
              if (R[m]["?"]=="S" && R[m+1]["?"] == "S") { continue }
              if (R[m]["?"]=="S") i               {print; continue } 
              if (R[m]["?"]=="C" && R[m+1]["?"] == "S") { continue }
              
       }}

}
classify $1 | render
