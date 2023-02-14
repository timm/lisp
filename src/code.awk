BEGIN {
	Color1="DarkBlue"
     Color2="teal"
     Color3="f79a32"
     Color3="red"
     Color4="darkred"
     Vars="_ENV _G _VERSION assert collectgarbage coroutine debug dofile error file getmetatable "\
          "io ipairs load loadfile math next os package pairs pcall print rawequal rawget rawlen "\
           "rawset require select setmetatable string table tonumber tostring type utf8 warn xpcall"
     Words="and break do else elseif end "\
   	   "for function if in local not or "\
	   "repeat return then until while "
     split(Words,Tmp," ")
     for(Word in Tmp) { Pat1 = Pat1 Sep "\\y" Tmp[Word] "\\y"; Sep = "|" }
     Pat1 = "(" Pat1 ")"
     Sep=""
     split(Vars,Tmp," ")
     for(Word in Tmp) { Pat2 = Pat2 Sep "\\y" Tmp[Word] "\\y"; Sep = "|" }
     Pat2 = "(" Pat2 ")"
     In = 0
}
/^# /        { next }
!In &&/```/  { In=1; print("<pre>")  ; next }
In  && /```/ { In=0; print("     </pre>\n\n"); next }
             { print(In ? pretty($0) : $0)        }

function pretty(str) { 
  #gsub(/[\+=\*-/<>^{}\[\]]/, "<font color=gray><b>&</b></font>",str)
  gsub(Pat1,          "<font color="Color1"><b>&</b></font>",str) 
  gsub(Pat2,          "<font color="Color2"><b>&</b></font>",str) 
  gsub(/"[^"]*"/,    "<font color="Color3">&</font>",str)
  gsub(/--.*/,        "<font color="Color3">&</font>",str)
  str = gensub(/(\y[_a-zA-Z0-9]+\y)\(/, "<font color="Color4">\\1</font>(","g",str)
  return str
}

