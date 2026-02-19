vim.cmd('source ~/.vimrc')
-- automatically install lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({ "git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git", "--branch=stable",
    lazypath })
end
vim.opt.rtp:prepend(lazypath)

local signs = { Error = "", Warn = "", Hint = "", Info = "" }
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

vim.cmd("autocmd BufNewFile,BufRead tsconfig.json set filetype=jsonc") -- properly highlight json5 files
vim.cmd("autocmd FileType typescript,typescriptreact compiler tsc")


-- Diagnostic navigation
vim.keymap.set('n', '<leader>N', vim.diagnostic.goto_prev)
vim.keymap.set('n', '<leader>n', vim.diagnostic.goto_next)
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float)
vim.keymap.set('n', '<leader>q', vim.diagnostic.setqflist)

vim.keymap.set('v', "<C-y>", "\"+y")                               -- yank in systemclipboard
vim.keymap.set('n', "<C-;>", ":Buffers<CR>")                       -- yank in systemclipboard

vim.keymap.set("n", "<C-c>f", vim.lsp.buf.format, { noremap = true, silent = true })

vim.keymap.set('n', "<C-c><C-r>", ":%s/<C-r><C-w>/")               -- s/foo/bar current word


-- add some roundness to neovim popup
local orig_util_open_floating_preview = vim.lsp.util.open_floating_preview
---@diagnostic disable-next-line: duplicate-set-field
vim.lsp.util.open_floating_preview = function(contents, syntax, opts, ...)
  opts = opts or {}
  opts.border = opts.border or 'rounded'
  return orig_util_open_floating_preview(contents, syntax, opts, ...)
end

-- set cmd height to 0
vim.opt.cmdheight = 0

-- highlight on yank
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})


-- ================================
-- Lazy.nvim plugin setup
-- ================================
require("lazy").setup({

  -- ================================
  -- LSP Config
  -- ================================
  {
    "neovim/nvim-lspconfig",
    config = function()
      local on_attach = function(_, bufnr)
        local opts = { noremap=true, silent=true, buffer=bufnr }
        vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
        vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
        vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
        vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts)
        vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts)
      end

      vim.lsp.config("tsgo", {
        on_attach = on_attach,
        settings = {
          preferences = {
            importModuleSpecifierPreference = 'non-relative'
          }
        }
      })
      vim.lsp.enable("tsgo")

      vim.lsp.config("biome", {
        on_attach = on_attach,
      })
      vim.lsp.enable("biome")
    end,

    
    vim.diagnostic.config {
      severity_sort = true,
      float = {
        border = 'rounded',
        source = 'if_many',
        header = '',
      },
      underline = {
        severity = vim.diagnostic.severity.ERROR,
      },
      signs = vim.g.have_nerd_font and {
        text = {
          [vim.diagnostic.severity.ERROR] = '',
          [vim.diagnostic.severity.WARN] = '',
          [vim.diagnostic.severity.INFO] = '',
          [vim.diagnostic.severity.HINT] = '',
        },
      } or {},
      virtual_text = {
        source = 'if_many',
        spacing = 2,
        format = function(diagnostic)
          local diagnostic_message = {
            [vim.diagnostic.severity.ERROR] = diagnostic.message,
            [vim.diagnostic.severity.WARN] = diagnostic.message,
            [vim.diagnostic.severity.INFO] = diagnostic.message,
            [vim.diagnostic.severity.HINT] = diagnostic.message,
          }
          return diagnostic_message[diagnostic.severity]
        end,
      },
    }
  },

  -- ================================
  -- Autocompletion
  -- ================================
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "L3MON4D3/LuaSnip",
      "saadparwaiz1/cmp_luasnip",
    },
    config = function()
      local cmp = require("cmp")
      local luasnip = require("luasnip")
      require("luasnip.loaders.from_snipmate").load()

      cmp.setup({
        snippet = {
          expand = function(args)
            luasnip.lsp_expand(args.body)
          end
        },
        mapping = cmp.mapping.preset.insert({
          ["<C-n>"] = cmp.mapping.select_next_item(),
          ['<C-M-i>'] = cmp.mapping.complete(),
          ["<C-p>"] = cmp.mapping.select_prev_item(),
          ["<CR>"] = cmp.mapping.confirm({ select = true }),
          ['<Tab>'] = cmp.mapping(function(fallback)
            if luasnip.expand_or_jumpable() then
              luasnip.expand_or_jump()
            elseif cmp.visible() then
              cmp.select_next_item()
            else
              fallback()
            end
          end, { 'i', 's' }),
        }),
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "buffer" },
          { name = "path" },
          { name = "luasnip" },
        }),
      })
    end,
  },

  -- ================================
  -- Mason for LSP installation
  -- ================================
  {
    "williamboman/mason.nvim",
    config = function()
      require("mason").setup()
    end
  },
  {
    "williamboman/mason-lspconfig.nvim",
    config = function()
      require("mason-lspconfig").setup({
        ensure_installed = { "tailwindcss", "tsgo", "biome" },
      })
    end
  },


  { "nvim-lua/plenary.nvim", build = 'make' },
  { "nvim-telescope/telescope-fzf-native.nvim" },
  --
  -- ================================
  -- Telescope
  -- ================================
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
        function()
          ts_builtin.find_files({ cwd = vim.fn.expand('%:p:h') })
        end,
        {}
      )

      vim.api.nvim_set_keymap('n', '<C-c>p', ':FindCurrentDir<CR>', { noremap = true, silent = true })
      vim.keymap.set('n', '<C-p>', ts_builtin.find_files, { desc = '[S]earch [F]iles' })
      vim.keymap.set('n', '<C-;>', ts_builtin.buffers)
      vim.keymap.set('n', '<C-b>', ts_builtin.buffers)
      -- vim.keymap.set('n', '<leader>f', ts_builtin.grep_string, { desc = '[S]earch current [W]ord' })
      vim.keymap.set('n', '<C-f>', ts_builtin.live_grep, { desc = '[S]earch by [G]rep' })
      vim.keymap.set('n', '<leader>sd', ts_builtin.diagnostics, { desc = '[S]earch [D]iagnostics' })
    end,
  },

  --
  -- ================================
  -- Tim Pope sections :)
  -- ================================
  { "tpope/vim-eunuch" },
  { "tpope/vim-rsi" },
  { "tpope/vim-repeat" },
  { "tpope/vim-commentary" },
  { "tpope/vim-fugitive" },
  --
  -- ================================
  -- misc
  -- ================================
  { "itchyny/vim-qfedit" }, -- edit quickfix list
  {
    "ruifm/gitlinker.nvim",
    config = function()
      require("gitlinker").setup()
    end
  },
  {
    "windwp/nvim-autopairs",
    config = function()
      require("nvim-autopairs").setup({ map_cr = true })
    end
  },

  -- ================================
  -- Theme
  -- ================================
  {
    "gbprod/nord.nvim",
    config = function()

      vim.cmd("colorscheme nord")
      -- darker background
      local darkerBg = "#181818" 
      vim.api.nvim_set_hl(0, "Normal", { bg = darkerBg })
      vim.api.nvim_set_hl(0, "NormalFloat", { bg = darkerBg })
      vim.api.nvim_set_hl(0, "SignColumn", { bg = darkerBg })
      vim.api.nvim_set_hl(0, "EndOfBuffer", { bg = darkerBg })
    end
  }

})
