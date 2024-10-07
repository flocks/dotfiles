-- automatically install lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({ "git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git", "--branch=stable", lazypath })
end
vim.opt.rtp:prepend(lazypath)

vim.g.mapleader = "-" -- set leader key
vim.g.maplocalleader = '-' -- set local leader key

vim.o.breakindent = true -- enable break indent
vim.o.completeopt = 'menuone,noselect' -- set completeopt to have a better completion experience
vim.o.hlsearch = false -- set highlight on search
vim.o.ignorecase = true -- case insentitive search
vim.o.mouse = 'c' -- enable mouse mode
vim.o.smartcase = true -- sensitive search as soon as a different case is used
vim.o.termguicolors = true -- term colors 
vim.o.updatetime = 5 -- decrease update time
vim.opt.autoread = true -- auto reload files when changed
vim.opt.colorcolumn = "100" -- show the 100 chars column
vim.opt.cursorline = true -- show cursor line
vim.opt.encoding = "utf-8" -- encoding
vim.opt.expandtab = true -- use space to indent
vim.opt.hidden = true -- don't write empty unsaved files
vim.opt.hlsearch = false -- prevent annoying highlight on search
vim.opt.incsearch = true -- move on search
vim.opt.laststatus = 2 -- always show status bar
vim.opt.list = true -- show blank characters
vim.opt.listchars = "tab:>-,trail:·,nbsp:%" -- define blank characters
vim.opt.pumheight = 10 -- maximum number of items to show in the popup menu
vim.opt.relativenumber = true -- relative line numbers
vim.opt.scrolloff = 5 -- number of lines to keep above & below cursor when scrolling
vim.opt.shiftwidth = 2 -- tab shiftwidth
vim.opt.showmode = false -- show "lint" column
vim.opt.sidescrolloff = 5 -- number of cols to keep above & below cursor when scrolling
vim.opt.signcolumn = "yes" -- show "lint" column
vim.opt.splitbelow = true -- behavior when splitting horizontally
vim.opt.splitright = true -- behavior when splitting vertically
vim.opt.swapfile = false -- don't create useless files
vim.opt.tabstop = 2 -- tab tabstop
vim.opt.termguicolors = true
vim.opt.wb = false -- don't create useless files
vim.opt.wildmenu = true -- enable wild menu
vim.opt.wildmode = "longest,full" -- wild menu completion
vim.opt.wrap = false
vim.opt.writebackup = false -- don't create useless files
vim.wo.number = true -- Make line numbers default
vim.wo.signcolumn = 'yes' -- always show sign column
vim.g.netrw_banner = 0; -- hide netrw file explorer banner

vim.g.asyncrun_open = 8
vim.b.prettier_exec_cmd = "prettierd"



local signs = {Error = "", Warn = "", Hint = "", Info = ""}
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, {text = icon, texthl = hl, numhl = hl})
end

vim.cmd("au BufReadPost * if line(\"'\\\"\") > 1 && line(\"'\\\"\") <= line(\"$\") | exe \"normal! g'\\\"\" | endif") -- retrieve last edited line
vim.cmd("autocmd BufNewFile,BufRead tsconfig.json set filetype=jsonc") -- properly highlight json5 files
vim.cmd("autocmd FileType typescript,typescriptreact compiler tsc")

vim.api.nvim_command("set grepprg=rg\\ --vimgrep\\ --no-heading\\ --smart-case") -- use rg for :grep/lgrep
--
-- insert dir of current file into prompt
vim.api.nvim_set_keymap('c', '<C-M-e>', 'expand("%:p:h") . "/"<Space>', { expr = true, noremap = true, silent = true })

vim.keymap.set({ 'n', 'v' }, '-', '<Nop>', { silent = true }) -- disable default behavior of '-' (because leader)

-- Diagnostic navigation
vim.keymap.set('n', '<leader>N', vim.diagnostic.goto_prev)
vim.keymap.set('n', '<leader>n', vim.diagnostic.goto_next)
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float)
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist)
vim.keymap.set('n', '<leader>Y', ":let @+ = @%<CR>")
vim.keymap.set('n', '<leader>y', ":let @+ = expand('%:p')<CR>")
vim.keymap.set('n', '<leader>d', ":let @+ = expand('%:p:h')<CR>")

vim.keymap.set('v', "<C-y>", "\"+y") -- yank in systemclipboard
vim.keymap.set('n', "<C-c><C-f>", ":Prettier<CR>") -- reformat
vim.keymap.set('n', "<C-x><C-j>", ":Explore<CR>") -- file explorer
vim.keymap.set('n', "<C-c><C-r>", ":%s/<C-r><C-w>/") -- s/foo/bar current word
vim.keymap.set('n', "<M-p>", ":cprev<CR>") -- previous in quickfix list
vim.keymap.set('n', "<M-n>", ":cnext<CR>") -- next in quickfix list

-- save with <C-Enter>
vim.keymap.set("i", "<C-Enter>", "<C-O>:w<CR>")
vim.keymap.set("n", "<C-Enter>", ":w<CR>")

-- split management
vim.keymap.set('n', "vv", "<C-w>v")   
vim.keymap.set('n', "ss", "<C-w>s")   
vim.keymap.set('n', "<C-h>", "<C-w>h")
vim.keymap.set('n', "<C-l>", "<C-w>l")
vim.keymap.set('n', "<C-j>", "<C-w>j")
vim.keymap.set('n', "<C-k>", "<C-w>k")

vim.keymap.set('n', "<M-p>", ":cprevious<CR>") -- reformat
vim.keymap.set('n', "<space>", ":copen<CR>") -- openquickfix lsit
vim.keymap.set('n', "<Leader>o", ":%bd|e#<CR>") -- close all buffers except the current one
vim.keymap.set('n', "<Leader>m", "^vg_o") -- select all line content
vim.keymap.set('n', "Q", ":cq<CR>") -- never use Ex useless mode


-- vim.keymap.set('n', "<C-c>c", ":AsyncRun<Space>") -- never use Ex useless mode
vim.keymap.set('n', "<C-c>c", ":AsyncRun<Space>") -- never use Ex useless mode
vim.keymap.set('n', "<C-c><C-c>", ":AsyncRun<Space>make<CR>") -- never use Ex useless mode



vim.keymap.set('n', "<leader>f", ":Git grep -w -q <C-r><C-w><CR>") -- search word under cursor and populates quickfix

vim.keymap.set({ 'n', 'i' }, "<c-s>", "<Esc>:x<CR>") -- c-s to save

vim.keymap.set("i", "AA", "<Esc>A") -- quick command in insert mode: go to line end
vim.keymap.set("i", "II", "<Esc>I") -- quick command in insert mode: go to line start
vim.keymap.set("i", "OO", "<Esc>O") -- quick command in insert mode: go to line above



-- automatically set current file directory into register d
vim.api.nvim_create_autocmd('BufEnter', {
  callback = function()
    local current_file = vim.fn.expand("%:p")
    local folder = vim.fn.fnamemodify(current_file, ":p:h")
    local filename_without_extension = vim.fn.fnamemodify(current_file, ":t:r")
    folder = folder .. "/"

    vim.fn.setreg('d', folder)
    vim.fn.setreg('f', filename_without_extension)
  end,
  pattern = '*',
})

-- highlight on yank
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})

-- LSP attach handler
local on_attach = function(_, bufnr)
  local nmap = function(keys, func, desc)
    if desc then
      desc = 'LSP: ' .. desc
    end
    vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
  end

  nmap('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')
  nmap('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction')

  nmap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
  nmap('gr', require('telescope.builtin').lsp_references, '[G]oto [R]eferences')
  nmap('gi', vim.lsp.buf.implementation, '[G]oto [I]mplementation')
  nmap('<leader>D', vim.lsp.buf.type_definition, 'Type [D]efinition')
  nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
end

-- PLUGINS
-- =======

local lsp_servers = {
  'tsserver',
  'tailwindcss',
  'pylsp'
}

require("lazy").setup({
  {
    "kylechui/nvim-surround",
    version = "*", -- Use for stability; omit to use `main` branch for the latest features
    event = "VeryLazy",
    config = function()
        require("nvim-surround").setup({
            -- Configuration here, or leave empty to use defaults
        })
    end
  },
  {
    "lambdalisue/vim-manpager"
  },
  {
    "skywind3000/asyncrun.vim"
  },
  {"prettier/vim-prettier"},
  {
    "tpope/vim-eunuch"
  },
  {"mogelbrod/vim-jsonpath"},
  {
    "blazkowolf/gruber-darker.nvim",
    config = function ()
      -- vim.cmd("colorscheme gruber-darker")
    end
  },
  {
    "Shatur/neovim-ayu",
    config = function ()
      -- vim.cmd("colorscheme ayu-dark")
    end
  },
  {
    "miikanissi/modus-themes.nvim",
    config = function()
      -- vim.cmd("colorscheme modus_vivendi")
    end
  },
  {
    "thallada/farout.nvim",
    config = function()
      vim.cmd("colorscheme farout-night")
    end
  },
  {
    "tpope/vim-commentary"
  },
  {"norcalli/nvim-colorizer.lua", 
  config = function ()
    local colorizer = require("colorizer")
    colorizer.setup()
  end
  },
  { 
    "ruifm/gitlinker.nvim",
     config = function()
        require("gitlinker").setup()
     end
  },
  {
    "tpope/vim-fugitive"
  },
  {
    "tpope/vim-rsi"
  },
  {
    "itchyny/vim-qfedit"                   -- edit quickfix list
  },
  { "ThePrimeagen/harpoon",
    config = function()
      require("harpoon").setup({})
      local harpoon_ui = require("harpoon.ui")
      local harpoon_mark = require("harpoon.mark")
         vim.keymap.set('n', '<C-e>', harpoon_ui.toggle_quick_menu)
         vim.keymap.set('n', '<C-M-e>', harpoon_mark.add_file)
    end
  },
  -- auto pairs
  {
    "windwp/nvim-autopairs",
    config = function()
      require("nvim-autopairs").setup({ map_cr = true })
    end
  },
  -- fuzzy finder
  { "nvim-lua/plenary.nvim", build = 'make' },
  { "nvim-telescope/telescope-fzf-native.nvim" },
  {
    "nvim-telescope/telescope.nvim",
    config = function()
      local telescope = require('telescope')
      local ts_builtin = require('telescope.builtin')

      pcall(telescope.load_extension, 'fzf')

      telescope.setup({
        defaults = {
          preview_cutoff = 20,
          layout_config = {
            preview_width = 0.65,
            vertical = {
              width = 0.95,
            },
            horizontal = {
              width = 0.95,
            }
          },
          mappings = {
            i = {
              ['<C-u>'] = false,
              ['<C-d>'] = false,
            },
          },
        },
        pickers = {
          find_files = {
            previewer = false
          }
        },
      })

      vim.api.nvim_create_user_command(
          'FindCurrentDir',
          function ()
              ts_builtin.find_files({cwd = vim.fn.expand('%:p:h')})
          end,
          {}
      )

      vim.api.nvim_set_keymap('n', '<C-c>p', ':FindCurrentDir<CR>', { noremap = true, silent = true })
      vim.api.nvim_set_keymap('n', '<C-c>f', ':Telescope diagnostics<CR>', { noremap = true, silent = true })
      vim.keymap.set('n', '<C-p>', ts_builtin.find_files, { desc = '[S]earch [F]iles' })
      vim.keymap.set('n', '<C-;>', ts_builtin.buffers )
      vim.keymap.set('n', '<C-b>', ts_builtin.buffers )
      -- vim.keymap.set('n', '<leader>f', ts_builtin.grep_string, { desc = '[S]earch current [W]ord' })
      vim.keymap.set('n', '<C-f>', ts_builtin.live_grep, { desc = '[S]earch by [G]rep' })
      vim.keymap.set('n', '<leader>sd', ts_builtin.diagnostics, { desc = '[S]earch [D]iagnostics' })
    end,
  },

  -- LSP
  { "williamboman/mason.nvim" },
  {
    "williamboman/mason-lspconfig.nvim",
    config = function()
      require('mason').setup()
      require('mason-lspconfig').setup({
        ensure_installed = lsp_servers,
      })
    end
  },
  {
    "neovim/nvim-lspconfig",
    config = function()
      local lspconfig = require('lspconfig')
      local servers = {
        'tsserver',
        'tailwindcss',
        'pylsp'
      }

      local capabilities = vim.lsp.protocol.make_client_capabilities()
      capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

      for _, lsp in ipairs(servers) do
        if lsp == 'tsserver' then
          lspconfig[lsp].setup {
            on_attach = on_attach,
            capabilities = capabilities,
            filetypes = {"typescript", "typescriptreact", "typescript.tsx"}
          }
        elseif lsp == "pylsp" then
          lspconfig[lsp].setup {
            on_attach = on_attach,
            capabilities = capabilities,
            filetypes = {"python"}
          }
        else
          lspconfig[lsp].setup {
            on_attach = on_attach,
            capabilities = capabilities,
          }
        end
      end

      lspconfig.clangd.setup({
        cmd = { "clangd-12"},
        on_attach = on_attach,
        capabilities = capabilities,
      })


    end
  },
  -- completion
  { "hrsh7th/cmp-nvim-lsp" },
  {
    "L3MON4D3/LuaSnip",
    config = function()
      require("luasnip.loaders.from_snipmate").load()
    end
  },
  { "saadparwaiz1/cmp_luasnip" },
  { "onsails/lspkind.nvim" },
  { "hrsh7th/cmp-buffer" },
  {
    "hrsh7th/nvim-cmp",
    config = function()
      local cmp = require('cmp')
      local luasnip = require('luasnip')
      local lspkind = require('lspkind')
      cmp.setup({
        formatting = {
          format = lspkind.cmp_format({
            mode = 'symbol', -- show only symbol annotations
            -- maxwidth = 50, -- prevent the popup from showing more than provided characters (e.g 50 will not show more than 50 characters)
          })
        },
        snippet = {
          expand = function(args)
            luasnip.lsp_expand(args.body)
          end,
        },
        mapping = cmp.mapping.preset.insert {
          ['<C-d>'] = cmp.mapping.scroll_docs(-4),
          ['<C-f>'] = cmp.mapping.scroll_docs(4),
          ['<C-Space>'] = cmp.mapping.complete(),
          ['<CR>'] = cmp.mapping.confirm {
            behavior = cmp.ConfirmBehavior.Replace,
            select = true,
          },
          ['<Tab>'] = cmp.mapping(function(fallback)
            if luasnip.expand_or_jumpable() then
              luasnip.expand_or_jump()
            elseif cmp.visible() then
              cmp.select_next_item()
            else
              fallback()
            end
          end, { 'i', 's' }),
          ['<S-Tab>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
              luasnip.jump(-1)
            else
              fallback()
            end
          end, { 'i', 's' }),
        },
        sources = {
          { name = 'nvim_lsp' },
          { name = 'luasnip' },
          { name = 'buffer'}
        },
        fallback = {
          enable = true,
          source = 'vim',
          -- You can specify additional sources here if desired.
        },
      })
    end
  },
}, opts)

