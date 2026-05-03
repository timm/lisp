-- netrw -------------------------------------------------------------
vim.g.netrw_banner = 0
vim.g.netrw_liststyle = 3
vim.g.netrw_browse_split = 4
vim.g.netrw_winsize = 15

-- ui / editing ------------------------------------------------------
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.cursorline = true
vim.opt.mouse = "a"
vim.opt.clipboard = "unnamedplus"

vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.expandtab = true
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2

vim.opt.splitright = true
vim.opt.splitbelow = true


vim.g.netrw_banner = 0

-- undo --------------------------------------------------------------
vim.opt.undofile = true
vim.opt.undodir = vim.fn.expand("~/.vim/undo")

-- statusline --------------------------------------------------------
vim.opt.statusline =
  "%#StatusLine# ▶ %f %m%r%=%y ❖ %l:%c ❖ %p%% "

vim.opt.laststatus = 2

-- keymaps -----------------------------------------------------------
vim.keymap.set("n", "Q", ":quitall<CR>", { noremap = true, silent = true })

-- Toggle spelling with <Leader>s (usually \s)
vim.keymap.set("n", "<leader>s", ":set spell!<CR>", { silent = true })

-- colors ------------------------------------------------------------
vim.cmd.colorscheme("sorbet")

-- lisp indent: body always 2 spaces past innermost open paren ------
_G.LispIndent = function()
  if vim.v.lnum <= 1 then return 0 end
  vim.fn.cursor(vim.v.lnum, 1)
  local skip = [[synIDattr(synID(line("."), col("."), 1), "name") =~? "string\|comment"]]
  local pos = vim.fn.searchpairpos('(', '', ')', 'bnW', skip)
  if pos[1] == 0 then return 0 end
  return pos[2] + 1
end

vim.api.nvim_create_autocmd("FileType", {
  pattern = "lisp",
  callback = function()
    vim.bo.lisp = false
    vim.bo.indentexpr = "v:lua.LispIndent()"
  end,
})

