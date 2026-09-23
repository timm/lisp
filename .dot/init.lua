-- Neovim config (nvim >= 0.12, vim.pack). launch: nvim --clean -u this
-- keys: <space>f files  <space>g grep  <space>b buffers  <space>t todos
--       <space>e files-float  -  sidebar  <tab>/<s-tab> buffers
--       <space>? keymap help  ]h/[h hunks  <space>hs stage  <space>hr reset
local o, map, au = vim.o, vim.keymap.set, vim.api.nvim_create_autocmd
vim.g.mapleader = " "
o.termguicolors, o.number, o.cursorline, o.signcolumn = true, true, true, "yes"
o.expandtab, o.shiftwidth, o.tabstop, o.softtabstop = true, 2, 2, 2
o.ignorecase, o.smartcase, o.clipboard = true, true, "unnamedplus"
o.splitbelow, o.splitright, o.scrolloff, o.undofile = true, true, 6, true
o.autoread, o.updatetime, o.timeoutlen = true, 250, 400

-- autoread needs a poll: re-check on idle / buffer-enter / focus.
au({ "FocusGained", "BufEnter", "CursorHold", "CursorHoldI" }, {
  callback = function()
    if vim.fn.mode() ~= "c" and vim.fn.getcmdwintype() == "" then vim.cmd("checktime") end
  end })
au("TextYankPost", { callback = function() vim.hl.on_yank() end })

-- plugins. --clean drops site from packpath/rtp; re-add it.
local site = vim.fn.stdpath("data") .. "/site"
vim.opt.packpath:prepend(site)
vim.opt.runtimepath:prepend(site)
vim.g.loaded_netrw, vim.g.loaded_netrwPlugin = 1, 1   -- oil replaces netrw
vim.pack.add({
  "https://github.com/catppuccin/nvim",
  "https://github.com/nvim-lua/plenary.nvim",         -- for todo-comments
  "https://github.com/echasnovski/mini.nvim",
  "https://github.com/nvim-treesitter/nvim-treesitter",
  "https://github.com/nvim-treesitter/nvim-treesitter-context",
  "https://github.com/HiPhish/rainbow-delimiters.nvim",
  "https://github.com/stevearc/oil.nvim",
  "https://github.com/lewis6991/gitsigns.nvim",
  "https://github.com/folke/which-key.nvim",
  "https://github.com/julienvincent/nvim-paredit",
  "https://github.com/folke/todo-comments.nvim",
})
vim.cmd.colorscheme("catppuccin-mocha")

-- mini: icons, fuzzy pick, statusline, tabline, pairs, surround, comment
for _, m in ipairs({ "icons", "pick", "statusline", "tabline", "pairs", "surround", "comment" }) do
  require("mini." .. m).setup()
end
map("n", "<leader>f", "<cmd>Pick files<CR>")
map("n", "<leader>g", "<cmd>Pick grep_live<CR>")
map("n", "<leader>b", "<cmd>Pick buffers<CR>")
map("n", "<tab>", "<cmd>bnext<CR>")
map("n", "<s-tab>", "<cmd>bprev<CR>")

-- treesitter: parsers auto-installed (needs tree-sitter cli + cc)
local langs = { "lua", "python", "commonlisp", "markdown", "bash", "make", "json" }
require("nvim-treesitter").install(langs)
au("FileType", { callback = function(a)
  pcall(vim.treesitter.start, a.buf)
  vim.bo[a.buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
end })
require("treesitter-context").setup({ max_lines = 3 })

-- files: oil edits dirs as buffers
require("oil").setup({ view_options = { show_hidden = true },
  keymaps = {                                          -- in oil: - parent, q close
    q = function() vim.api.nvim_win_close(0, true) end,
    ["<CR>"] = function()                              -- dir: enter; file: open in other window
      local oil, e = require("oil"), require("oil").get_cursor_entry()
      if not e then return elseif e.type == "directory" then return oil.select() end
      local path, win = oil.get_current_dir() .. e.name, nil
      for _, w in ipairs(vim.api.nvim_list_wins()) do
        if vim.bo[vim.api.nvim_win_get_buf(w)].filetype ~= "oil" then win = w break end end
      if win then vim.api.nvim_set_current_win(win) else vim.cmd("botright vsplit") end
      vim.cmd.edit(vim.fn.fnameescape(path))
    end } })
map("n", "-", function()                              -- toggle left sidebar
  for _, w in ipairs(vim.api.nvim_list_wins()) do
    if vim.bo[vim.api.nvim_win_get_buf(w)].filetype == "oil" then
      return vim.api.nvim_win_close(w, true) end end
  vim.cmd("topleft 30vsplit | Oil")
end)
map("n", "<leader>e", function() require("oil").toggle_float() end)

-- git gutter
require("gitsigns").setup({ on_attach = function(b)
  local gs = require("gitsigns")
  map("n", "]h", function() gs.nav_hunk("next") end, { buffer = b })
  map("n", "[h", function() gs.nav_hunk("prev") end, { buffer = b })
  map("n", "<leader>hs", gs.stage_hunk, { buffer = b })
  map("n", "<leader>hr", gs.reset_hunk, { buffer = b })
  map("n", "<leader>hp", gs.preview_hunk, { buffer = b })
end })

require("which-key").setup({ preset = "helix" })
map("n", "<leader>?", function() require("which-key").show({ global = false }) end)
require("todo-comments").setup()
map("n", "<leader>t", "<cmd>TodoQuickFix<CR>")

-- lisp: vim indenting + this repo's kit words; paredit slurp/barf/raise
au("FileType", { pattern = "lisp", callback = function()
  vim.opt_local.lisp = true
  vim.opt_local.lispwords:append({ "fn", "let+", "defmethod", "loop" })
end })
require("nvim-paredit").setup()

-- per-repo overrides (loaded last so they win). silent if missing.
pcall(dofile, vim.fn.getcwd() .. "/init.local.lua")
