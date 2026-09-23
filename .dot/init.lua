-- Neovim config: catppuccin-mocha + nvim-tree (launch: nvim --clean -u this)
vim.g.mapleader = " "
vim.o.termguicolors = true
vim.o.number = true
vim.o.cursorline = true
vim.o.expandtab = true
vim.o.shiftwidth, vim.o.tabstop, vim.o.softtabstop = 2, 2, 2
vim.o.ignorecase, vim.o.smartcase = true, true
vim.o.clipboard = "unnamedplus"
vim.o.autoread = true                                 -- reload disk changes
vim.o.updatetime = 250                                -- CursorHold fires faster

-- autoread needs a poll: re-check on idle / buffer-enter / focus.
vim.api.nvim_create_autocmd(
  { "FocusGained", "BufEnter", "CursorHold", "CursorHoldI" },
  { callback = function()
      if vim.fn.mode() ~= "c" and vim.fn.getcmdwintype() == "" then
        vim.cmd("checktime")
      end
    end })

-- plugins via vim.pack (nvim 0.12). --clean drops site from packpath; re-add it.
vim.g.loaded_netrw, vim.g.loaded_netrwPlugin = 1, 1   -- nvim-tree replaces netrw
vim.opt.packpath:prepend(vim.fn.stdpath("data") .. "/site")
vim.pack.add({
  "https://github.com/catppuccin/nvim",
  "https://github.com/nvim-tree/nvim-web-devicons",
  "https://github.com/nvim-tree/nvim-tree.lua",
})
vim.cmd.colorscheme("catppuccin-mocha")

-- file manager: 25% sidebar, nerd-font icons
require("nvim-tree").setup({
  view     = { width = "25%" },
  renderer = { group_empty = true, highlight_git = true },
  filters  = { dotfiles = false },
})
vim.keymap.set("n", "<leader>e", "<cmd>NvimTreeToggle<CR>")  -- toggle sidebar

-- per-repo overrides (loaded last so they win). silent if missing.
-- lisp: vim-style lisp indenting, plus this repo's kit words
-- (fn let+ ...) indented as special forms
vim.api.nvim_create_autocmd("FileType", { pattern = "lisp",
  callback = function()
    vim.opt_local.lisp = true
    vim.opt_local.lispwords:append({ "fn", "let+", "defmethod", "loop" })
  end })

pcall(dofile, vim.fn.getcwd() .. "/init.local.lua")

-- vim.o.termguicolors = true
-- vim.o.expandtab = true
-- vim.o.tabstop = 2
-- vim.o.shiftwidth = 2
-- vim.o.softtabstop = 2
-- vim.o.ignorecase = true
-- vim.o.smartcase = true
--
-- vim.g.maplocalleader = "\\"
--
-- vim.o.relativenumber = true
-- vim.o.cursorline = true
-- vim.o.mouse = "a"
-- vim.o.termguicolors = true
-- vim.o.expandtab = true
-- vim.o.tabstop = 2
-- vim.o.shiftwidth = 2
-- vim.o.softtabstop = 2
-- vim.o.ignorecase = true
-- vim.o.smartcase = true
-- vim.o.scrolloff = 8
-- vim.o.signcolumn = "yes"
-- vim.o.splitbelow = true
-- vim.o.splitright = true
-- vim.o.wrap = true
-- vim.o.linebreak = true
-- vim.o.fillchars = "vert:\u{2502},eob:\u{00b7}"
-- vim.o.winborder = "rounded"
-- vim.o.splitkeep = "screen"
-- vim.o.smoothscroll = true
-- vim.o.undofile = true
-- vim.o.timeout = false
-- vim.o.sidescrolloff = 7
-- vim.o.sidescroll = 1
-- vim.o.autoindent = true
-- vim.opt.wildignore:append({
--   "*/node_modules/*","*/tmp/*","*/target/*","*/build/*",
-- })
--
-- local map = vim.keymap.set
-- map("n", "x", '"_x')              -- x/X to black hole
-- map("n", "X", '"_X')
-- map("n", "<c-j>", ":m .+1<CR>==") -- move line down
-- map("n", "<c-k>", ":m .-2<CR>==") -- move line up
-- map("t", "<Esc>", [[<C-\><C-n>]]) -- escape terminal
-- map("n", "<leader><tab>", "<c-^>")
-- map("n", "<leader>w", "<c-w>")
--
-- vim.filetype.add({ extension = { mal = "lisp" } })
--
-- vim.api.nvim_create_autocmd("BufReadPost", {  -- restore cursor pos
--   callback = function()
--     local m = vim.api.nvim_buf_get_mark(0, '"')
--     if m[1] > 1 and m[1] <= vim.api.nvim_buf_line_count(0) then
--       vim.api.nvim_win_set_cursor(0, m)
--     end
--   end,
-- })
--
-- vim.opt.packpath:prepend(vim.fn.stdpath("data") .. "/site")
-- vim.pack.add({
--   "https://github.com/catppuccin/nvim",
--   "https://github.com/nvim-lualine/lualine.nvim",
--   "https://github.com/zaldih/themery.nvim",
--   "https://github.com/folke/tokyonight.nvim",
--   "https://github.com/rebelot/kanagawa.nvim",
--   "https://github.com/rose-pine/neovim",
--   "https://github.com/EdenEast/nightfox.nvim",
-- })
-- vim.cmd.colorscheme("catppuccin-mocha")
-- vim.cmd("hi! WinSeparator guifg=#7aa2f7 guibg=NONE")
-- vim.cmd("hi! VertSplit    guifg=#7aa2f7 guibg=NONE")
-- require("lualine").setup({ options = { theme = "auto" } })
-- require("themery").setup({
--   themes = {
--     "catppuccin-mocha","catppuccin-latte",
--     "tokyonight-storm","tokyonight-day",
--     "kanagawa-wave","kanagawa-dragon",
--     "rose-pine","rose-pine-dawn",
--     "nightfox","duskfox","carbonfox",
--   },
--   livePreview = true,
-- })
--
-- vim.api.nvim_create_autocmd("TextYankPost", {
--   callback = function() vim.hl.on_yank() end,
-- })
--
-- vim.diagnostic.config({
--   virtual_text     = false,
--   signs            = true,
--   underline        = true,
--   update_in_insert = false,
--   severity_sort    = true,
--   float            = { border = "rounded" },
-- })
--
-- if vim.fn.executable("pyright-langserver") == 1 then
--   vim.lsp.config("pyright", {
--     cmd          = {"pyright-langserver", "--stdio"},
--     filetypes    = {"python"},
--     root_markers = {"pyproject.toml", ".git"},
--   })
--   vim.lsp.enable("pyright")
--   vim.api.nvim_create_autocmd("LspAttach", {
--     callback = function(a)
--       local b = { buffer = a.buf, silent = true }
--       vim.keymap.set("n", "K",         vim.lsp.buf.hover,         b)
--       vim.keymap.set("n", "gd",        vim.lsp.buf.definition,    b)
--       vim.keymap.set("n", "gr",        vim.lsp.buf.references,    b)
--       vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, b)
--     end,
--   })
-- end
