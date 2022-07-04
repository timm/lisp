#!/usr/bin/env bash

noPara1()  { gawk '
  BEGIN {RS=""; FS="\n"}
  NR==1 { next }
        { if(last) print(last "\n")
          last = $0 }
  END   { if(last) print(last) }'
}

(cat README.md
clisp tiny.lisp -f $1 -g doc |
gawk 'BEGIN {FS="((^[;])?[ \t]*->[ \t]*"}
      /->/  {gsub(/:/," :"); $0="**"$1"** <br> "$2};
         1  {print}'
)


noPara1()  { gawk '
  BEGIN {RS=""; FS="\n"}
  NR==1 { next }
        { if(last) print(last "\n")
          last = $0 }
  END   { if(last) print(last) }'
}

#!/usr/bin/env bash

noPara1()  { gawk '
  BEGIN {RS=""; FS="\n"}
  NR==1 { next }
        { if(last) print(last "\n")
          last = $0 }
  END   { if(last) print(last) }'
}

classify() { gawk '
  /^--\[\[/,/--\]\]/ { print "DOC " $0; next }
  /^-- /             { print "DOC " $0; next;}
                     { print "CODE "$0}' 
}

apply() { gawk '
  BEGIN {top=1}
      { now=$1
        gsub(/(DOC |CODE )/,"");
        apply(now,b4,last) 
        last=$0
        b4=now} 
END   { apply(now,b4,last)
        if (now=="CODE") print "```" }

function apply(now,b4,last) {
  gsub(/^(-- |--\[\[[ \t]*|--\]\])/,"",last)
  if  (now==b4)  
     print(last)
  else {
    if (last !~ /^[ \t]*$/) last=last "\n"
    if (now=="CODE") {
      printf(last); 
      print"\n```lua"  }
    if (now=="DOC")  {
      printf(last)
      if (!top)  print"```"
    }
    top=0}}' 
}

one() {
cat <<'EOF'

<img alt="Lua" src="https://img.shields.io/badge/lua-v5.4-blue">&nbsp;<a 
href="https://github.com/timm/keys/blob/master/LICENSE.md"><img
alt="License" src="https://img.shields.io/badge/license-unlicense-red"></a> <img
src="https://img.shields.io/badge/purpose-ai%20,%20se-blueviolet"> <img
alt="Platform" src="https://img.shields.io/badge/platform-osx%20,%20linux-lightgrey"> <a
href="https://github.com/timm/keys/actions"><img
src="https://github.com/rezons/rezons.github.io/actions/workflows/tests.yml/badge.svg"></a>

<hr>

EOF

cat $1 | noPara1 | classify  | apply
}
for f in $*; do
  echo "# one $f >  ../docs/${f%.lua}.md"
  one $f >  ../docs/${f%.lua}.md
done

# cd ../docs
# pandoc -f markdown  --template=ez.html --toc  --mathjax \
#        --lua-filter linenums.lua \
#        --highlight-style pygments --css style.css -s $f.md -o $f.html
# git add $f.html
# open $f.html
classify() { gawk '
  /^--\[\[/,/--\]\]/ { print "DOC " $0; next }
  /^-- /             { print "DOC " $0; next;}
                     { print "CODE "$0}' 
}

apply() { gawk '
  BEGIN {top=1}
      { now=$1
        gsub(/(DOC |CODE )/,"");
        apply(now,b4,last) 
        last=$0
        b4=now} 
END   { apply(now,b4,last)
        if (now=="CODE") print "```" }

function apply(now,b4,last) {
  gsub(/^(-- |--\[\[[ \t]*|--\]\])/,"",last)
  if  (now==b4)  
     print(last)
  else {
    if (last !~ /^[ \t]*$/) last=last "\n"
    if (now=="CODE") {
      printf(last); 
      print"\n```lua"  }
    if (now=="DOC")  {
      printf(last)
      if (!top)  print"```"
    }
    top=0}}' 
}

one() {
cat <<'EOF'

<img alt="Lua" src="https://img.shields.io/badge/lua-v5.4-blue">&nbsp;<a 
href="https://github.com/timm/keys/blob/master/LICENSE.md"><img
alt="License" src="https://img.shields.io/badge/license-unlicense-red"></a> <img
src="https://img.shields.io/badge/purpose-ai%20,%20se-blueviolet"> <img
alt="Platform" src="https://img.shields.io/badge/platform-osx%20,%20linux-lightgrey"> <a
href="https://github.com/timm/keys/actions"><img
src="https://github.com/rezons/rezons.github.io/actions/workflows/tests.yml/badge.svg"></a>

<hr>

EOF

cat $1 | noPara1 | classify  | apply
}
for f in $*; do
  echo "# one $f >  ../docs/${f%.lua}.md"
  one $f >  ../docs/${f%.lua}.md
done

# cd ../docs
# pandoc -f markdown  --template=ez.html --toc  --mathjax \
#        --lua-filter linenums.lua \
#        --highlight-style pygments --css style.css -s $f.md -o $f.html
# git add $f.html
# open $f.html
apply() { gawk '
  BEGIN {top=1}
      { now=$1
        gsub(/(DOC |CODE )/,"");
        apply(now,b4,last) 
        last=$0
        b4=now} 
END   { apply(now,b4,last)
        if (now=="CODE") print "```" }

function apply(now,b4,last) {
  gsub(/^(-- |--\[\[[ \t]*|--\]\])/,"",last)
  if  (now==b4)  
     print(last)
  else {
    if (last !~ /^[ \t]*$/) last=last "\n"
    if (now=="CODE") {
      printf(last); 
      print"\n```lua"  }
    if (now=="DOC")  {
      printf(last)
      if (!top)  print"```"
    }
    top=0}}' 
}

one() {
cat <<'EOF'

<img alt="Lua" src="https://img.shields.io/badge/lua-v5.4-blue">&nbsp;<a 
href="https://github.com/timm/keys/blob/master/LICENSE.md"><img
alt="License" src="https://img.shields.io/badge/license-unlicense-red"></a> <img
src="https://img.shields.io/badge/purpose-ai%20,%20se-blueviolet"> <img
alt="Platform" src="https://img.shields.io/badge/platform-osx%20,%20linux-lightgrey"> <a
href="https://github.com/timm/keys/actions"><img
src="https://github.com/rezons/rezons.github.io/actions/workflows/tests.yml/badge.svg"></a>

<hr>

EOF

cat $1 | noPara1 | classify  | apply
}
for f in $*; do
  echo "# one $f >  ../docs/${f%.lua}.md"
  one $f >  ../docs/${f%.lua}.md
done

# cd ../docs
# pandoc -f markdown  --template=ez.html --toc  --mathjax \
#        --lua-filter linenums.lua \
#        --highlight-style pygments --css style.css -s $f.md -o $f.html
# git add $f.html
# open $f.html
