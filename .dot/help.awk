# help.awk -- self-doc for Makefiles:  awk -f help.awk $(MAKEFILE_LIST)
# Prints every `target: ## text` line. Targets from the FIRST file
# (this repo's Makefile) come first, in cyan. Targets from later
# files (the shared .dot/Makefile) come after a rule, dimmed -- so
# the local, interesting stuff is what the eye lands on.
BEGIN { C="\033[36m"; D="\033[2m"; Z="\033[0m" }
FNR==1 { nf++ }
/^[a-zA-Z0-9_.%\/-]+:.*## / {
  t = substr($0, 1, index($0, ":") - 1)
  sub(/^[^:]*:.*## /, "")
  if (nf == 1) L[++nl] = sprintf("  %s%-8s%s %s", C, t, Z, $0)
  else       { S[++ns] = sprintf("  %s%-8s %s%s",  D, t, $0, Z)
               B[ns]   = sprintf("  %s%-8s%s %s", C, t, Z, $0) } }
END {
  for (i = 1; i <= nl; i++) print L[i]
  if (nl && ns) print D "  ---- .dot ----" Z
  for (i = 1; i <= ns; i++) print (nl ? S[i] : B[i]) }
