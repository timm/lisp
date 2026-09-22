-- init.lua -- least config, most advantage.  Run with:
--
--     nvim -u ,/go.lua lib.lisp
--
-- `nvim --clean` already sets, for .lisp: filetype, the `lisp`
-- option (so lispwords works), `comments` (so ";;" is a comment
-- leader) and formatoptions=cqj (so `gq` reflows comments).
-- All that is missing is the house style.

vim.opt.termguicolors = true        -- 24-bit colour
vim.cmd.colorscheme("lunaperche")   -- ships with nvim; no plugin

vim.opt.expandtab   = true   -- the file is spaces; keep it spaces
vim.opt.shiftwidth  = 2
vim.opt.tabstop     = 2
vim.opt.softtabstop = 2

vim.opt.textwidth   = 65     -- TIP2: 65 chars, max
vim.opt.colorcolumn = "66"   -- ... and show where that is

vim.opt.number         = true   -- absolute number on the cursor line
vim.opt.relativenumber = true   -- ... offsets elsewhere, so 12dd
                                -- and 7j need no counting

-- Trailing "  " is a markdown hard break, so it must be visible.
vim.opt.list      = true
vim.opt.listchars = { trail = "·", tab = "» ", nbsp = "+" }

vim.api.nvim_create_autocmd("FileType", {
  pattern = "lisp",
  callback = function()
    -- loop bodies indent +2 instead of lining up under `for`
    vim.opt_local.lispwords:append(
      { "loop", "format", "error", "labels", "aif", "handler-case" })
  end })

-- <space>r runs the file, <space>q reflows the comment under the
-- cursor to 65 columns (what ,/lisp2md.awk assumes).
vim.g.mapleader = " "
vim.keymap.set("n", "<leader>r", ":w<CR>:!sbcl --script %<CR>")
vim.keymap.set("n", "<leader>q", "gqap")
