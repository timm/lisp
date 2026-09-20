#!/usr/bin/env bash
# colorful banner printer. usage: banner.sh FILE
# each line a catppuccin-ish hue, bold. last line (quote) lands green.
f="$1"; [[ -f $f ]] || exit 0
printf '\033[2J\033[3J\033[H'       # clear screen + scrollback, cursor home
cols=(212 213 177 141 147 117 84)   # pink magenta mauve lavender blue green
i=0
while IFS= read -r line; do
  printf '\033[1;38;5;%sm%s\033[0m\n' "${cols[i % ${#cols[@]}]}" "$line"
  ((i++))
done < "$f"
