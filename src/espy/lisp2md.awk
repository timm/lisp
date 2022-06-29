clssify() {
  gawk '/^;/       {pre="Q "}
        /^\(/      {pre="C "}
        /^[ \t]*$/ {print "S "; next}
        NR>1       {print pre $0} '
}

function code0(m) { print "```lisp"; print R[m]["="] }
function code1(m) { print "``` }
function shpw(m) { print "" }

BEGIN { todo["S","S"] = "jump" 
      }

render() {
 gawk '{ R[++N]["?"]=$1; $1=""; R[N]["="]=$0 }
       END {for(m=1;m<=N;m++) }
              a=R[m]["?"]; b=R[m+1]]["?"]
              if (todo[a,b])
                @todo(todo[a,b])
              else
                print("?", a,b,m) }


if (R[m]["?"]=="S" && R[m+1]["?"]=="S") { continue }
              if (R[m]["?"]=="S") && R[m+1]["?"]==      { print; continue } 
              if (R[m]["?"]=="S")                     { print; continue } 
              if (R[m]["?"]=="C" && R[m+1]["?"]=="S") { print "```lisp"; print R[m]["="]; continue }
              
       }}

}
classify $1 | render
