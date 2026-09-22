-- code.lua -- send the two kinds of code block to two LaTeX
-- environments, so they can be styled apart:
--   ```lisp fences  (real code)     -> lispcode  (gray panel)
--   indented blocks (doc examples)  -> egcode    (plain, no panel)
-- [TIP 7]{.tip .lisp} -> an icon badge. The category is the
-- second class; etc/acm.tex maps it to an icon and a colour.
function Span (el)
  if not FORMAT:match 'latex' then return nil end
  if not el.classes:includes('tip') then return nil end
  local cat = el.classes[2] or 'lisp'
  local n   = pandoc.utils.stringify(el):match('%d+') or '?'
  return pandoc.RawInline('latex', '\\tip{' .. cat .. '}{' .. n .. '}')
end

function CodeBlock (el)
  if not FORMAT:match 'latex' then return nil end
  local env = (el.classes[1] == 'lisp') and 'lispcode' or 'egcode'
  return pandoc.RawBlock('latex',
    '\\begin{' .. env .. '}\n' .. el.text .. '\n\\end{' .. env .. '}')
end
