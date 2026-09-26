vim.opt.expandtab      = true   -- expand tabs to spaces
vim.opt.shiftwidth     = 2      -- indent = 2 spaces
vim.opt.tabstop        = 2
vim.opt.softtabstop    = 2
vim.opt.textwidth      = 65     -- linewidth
vim.opt.colorcolumn    = "66"   -- ... and show where that is
vim.opt.number         = true   -- absolute on the cursor line
vim.opt.relativenumber = true   -- ... offsets elsewhere 
vim.opt.list           = true   -- show hard breaks (eol spaces)
vim.opt.listchars = { trail = "·", tab = "» ", nbsp = "+" }
vim.opt.termguicolors  = true   -- 24-bit colour
vim.cmd.colorscheme("lunaperche")  -- comments high contrast
vim.g.mapleader        = " "
vim.keymap.set("n", "<leader>q", "gqap")  -- <space>q reformats
vim.opt.lispwords:append{      -- bodies indent +2, instead
  "loop","format","error",     -- of lining up under the head
  "aif","has","kv","my","?","->","->>"}
