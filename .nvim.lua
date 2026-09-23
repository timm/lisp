vim.opt.termguicolors  = true   -- 24-bit colour
vim.opt.expandtab      = true   -- expand tabs to soaces
vim.opt.shiftwidth     = 2      -- indent = 2 spaces
vim.opt.tabstop        = 2
vim.opt.softtabstop    = 2
vim.opt.textwidth      = 65     -- linewidth
vim.opt.colorcolumn    = "66"   -- ... and show where that is
vim.opt.number         = true   -- absolute number of cursor line
vim.opt.relativenumber = true   -- ... offsets elsewhere, so 12dd
                                -- and 7j need no counting
vim.opt.list           = true -- display hard breaks (eol spaces)
vim.opt.listchars      = { trail = "·", tab = "» ", nbsp = "+" }
vim.g.mapleader        = " "
vim.keymap.set("n", "<leader>q", "gqap") -- <space>q == reformat
  
vim.cmd.colorscheme("lunaperche") -- makes comments high contrast
vim.api.nvim_create_autocmd("FileType", {
  pattern = "lisp",
  callback = function()
    -- loop bodies indent +2 instead of lining up under `for`
    vim.opt_local.lispwords:append(
      {"loop","format","error","labels","aif","handler-case"}) 
end })
  
